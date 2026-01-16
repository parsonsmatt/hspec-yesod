-- | This module exposes functions that are used internally by yesod-test. The functions exposed here are _not_ a stable API—they may be changed or removed without any major version bump.
--
-- That said, you may find them useful if your application can accept API breakage.
module Test.Hspec.Yesod.Internal
    ( RequestBuilderData(..)
    , RBDPostData(..)
    , RequestPart(..)
    , voidRequestBuilderUrl
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Network.HTTP.Types as H
import Network.Wai.Test hiding (assertHeader, assertNoHeader, request)

data RequestBuilderData url site = RequestBuilderData
    { rbdPostData :: RBDPostData
    , rbdResponse :: (Maybe SResponse)
    , rbdMethod :: H.Method
    , rbdSite :: site
    , rbdUrl :: Maybe url
    , rbdPath :: [T.Text]
    , rbdGets :: H.Query
    , rbdHeaders :: H.RequestHeaders
    }

voidRequestBuilderUrl :: RequestBuilderData url site -> RequestBuilderData () site
voidRequestBuilderUrl rbd =
    rbd
        { rbdUrl = Just ()
        }

data RBDPostData = MultipleItemsPostData [RequestPart]
                 | BinaryPostData BSL8.ByteString

-- | Request parts let us discern regular key/values from files sent in the request.
data RequestPart
  = ReqKvPart T.Text T.Text
  | ReqFilePart T.Text FilePath BSL8.ByteString T.Text
