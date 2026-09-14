{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NestedRouteDispatchSpec.AuthorizationTHSpec (spec) where

import Data.IORef
import Data.Maybe (listToMaybe)
import Language.Haskell.TH (listE, litE, Lit(IntegerL), recover, runIO)
import NestedRouteDispatchSpec.Resources (App)
import Test.Hspec (Spec, describe, it, shouldBe)
import Yesod.Core

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
