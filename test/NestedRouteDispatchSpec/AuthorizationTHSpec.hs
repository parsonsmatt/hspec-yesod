{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NestedRouteDispatchSpec.AuthorizationTHSpec (spec) where

import Data.IORef
import Data.Maybe (listToMaybe)
import Control.Monad (forM_)
import Language.Haskell.TH (listE, litE, Lit(..), recover, runIO)
import NestedRouteDispatchSpec.Resources (App)
import Test.Hspec (Spec, describe, it, shouldBe)
import Yesod.Core
import Yesod.Routes.TH.Types (Resource(..), ResourceTree(..), Dispatch(..))
import qualified Yesod.EmbeddedStatic as Embedded

type RawSubsite = WaiSubsite
type SubsiteAlias a = a
type EmbeddedAlias = Embedded.EmbeddedStatic

-- The type name alone must not cause a false positive for a user's own type.
data EmbeddedStatic = EmbeddedStatic
data Phantom a = Phantom

$(pure [])

mountFailures :: [(String, [([Bool], [Bool])])]
mountFailures = $(do
    let mount sub = ResourceLeaf (Resource "MountR" [] (Subsite sub "getSub") [] True)
        nested sub = [ResourceParent "MountParentR" True mempty [] [mount sub]]
        generate opts sub =
            [ mkYesodDispatchOpts opts "App" [mount sub]
            , mkYesodDispatchOpts opts "App" (nested sub)
            , mkYesodDispatchOpts (setFocusOnNestedRoute "MountParentR" opts) "App" (nested sub)
            , mkYesodDispatchOpts opts "Phantom a" (nested sub)
            ]
        wrapper = setRouteHandlerWrapper (\handler _ -> handler)
        options bypasses =
            [ (defaultOpts, False)
            , (wrapper defaultOpts, True)
            , (setRouteAuthorization RouteAuthPerResource defaultOpts, bypasses)
            , (setRouteAuthorization RouteAuthSubtree defaultOpts, bypasses)
            , (wrapper $ setRouteAuthorization RouteAuthPerResource defaultOpts, bypasses)
            ]
        types =
            [ ("WaiSubsite", True)
            , ("RawSubsite", True)
            , ("(SubsiteAlias WaiSubsite)", True)
            , ("(SubsiteAlias (SubsiteAlias WaiSubsite))", True)
            , ("Embedded.EmbeddedStatic", True)
            , ("EmbeddedAlias", True)
            , ("WaiSubsiteWithAuth", False)
            , ("(SubsiteAlias (SubsiteAlias WaiSubsiteWithAuth))", False)
            , ("EmbeddedStatic", False)
            ]
        rejects action = recover [| True |] (action >> [| False |])
        row sub (opts, expected) =
            [| ($(listE (map rejects (generate opts sub))), replicate 4 expected) |]
    listE
        [ [| ($(litE $ StringL sub), $(listE $ map (row sub) (options bypasses))) |]
        | (sub, bypasses) <- types ])

dataOnlyMounts :: Bool
dataOnlyMounts = $(recover [| False |] $ do
    let opts = setRouteHandlerWrapper (\_ _ -> fail "data-only callback ran") $
            setRouteAuthorization RouteAuthPerResource defaultOpts
        resources = [parseRoutes|
/ DataR GET
/raw RawR WaiSubsite getRaw
/embedded EmbeddedR Embedded.EmbeddedStatic getEmbedded
|]
    _ <- mkYesodDataOpts opts "App" resources
    _ <- mkYesodSubDataOpts opts "App" resources
    [| True |])

-- Use only the published API. Default controls rule out unrelated generation
-- failures; enabling either unsupported subsite option must fail in Q.
subsiteFailures :: [[Bool]]
subsiteFailures = $(do
    let resources = [parseRoutes|
/group GroupR:
    / LeafR GET
|]
        options =
            [ defaultOpts
            , setRouteAuthorization RouteAuthPerResource defaultOpts
            , setRouteAuthorization RouteAuthSubtree defaultOpts
            , setRouteHandlerWrapper (\handler _ -> handler) defaultOpts
            ]
        generators opts =
            [ mkYesodSubDispatchInstanceOpts opts "App" resources
            , mkNestedSubDispatchInstance opts "GroupR" [] NoTyArgs pure resources
            ]
        rejects action = recover [| True |] (action >> [| False |])
    listE [listE (map rejects (generators opts)) | opts <- options])

callbackCounts :: [Int]
callbackCounts = $(do
    counter <- runIO $ newIORef (0 :: Int)
    let resources = [parseRoutes|
/many ManyR GET POST DELETE
/any AnyR
/group GroupR:
    / LeafR GET POST
|]
        opts = setRouteHandlerWrapper
            (\handler _ -> runIO (modifyIORef' counter (+ 1)) >> handler) defaultOpts
        count action = do
            runIO $ writeIORef counter 0
            _ <- action
            n <- runIO $ readIORef counter
            litE $ IntegerL $ fromIntegral n
    listE
        [ count (mkYesodDataOpts opts "App" resources)
        , count (mkYesodSubDataOpts opts "App" resources)
        , count (mkYesodDispatchOpts opts "App" resources)
        , count (mkYesodDispatchOpts (setFocusOnNestedRoute "GroupR" opts) "App" resources)
        ])

spec :: Spec
spec = describe "public authorization TH API" $ do
    forM_ mountFailures $ \(subsite, results) ->
        forM_ (zip ["defaults", "wrapper only", "per resource", "subtree", "named and wrapper"] results) $ \(policy, (actual, expected)) ->
            it (subsite ++ " mount under " ++ policy ++ " in flat/nested/focused/inline dispatch") $
                actual `shouldBe` expected
    it "skips dispatch validation in data splices with shared authorization options" $
        dataOnlyMounts `shouldBe` True
    it "keeps default subsite dispatch generation available" $
        listToMaybe subsiteFailures `shouldBe` Just [False, False]
    it "rejects per-resource authorization in both subsite entry points" $
        listToMaybe (drop 1 subsiteFailures) `shouldBe` Just [True, True]
    it "rejects subtree authorization in both subsite entry points" $
        listToMaybe (drop 2 subsiteFailures) `shouldBe` Just [True, True]
    it "rejects handler wrappers in both subsite entry points" $
        listToMaybe (drop 3 subsiteFailures) `shouldBe` Just [True, True]
    it "does not run wrappers in site data splices" $
        listToMaybe callbackCounts `shouldBe` Just 0
    it "does not run wrappers in subsite data splices" $
        listToMaybe (drop 1 callbackCounts) `shouldBe` Just 0
    it "runs one callback per leaf, independent of method count and 405 arms" $
        listToMaybe (drop 2 callbackCounts) `shouldBe` Just 3
    it "runs one callback for a separately generated fragment" $
        listToMaybe (drop 3 callbackCounts) `shouldBe` Just 1
