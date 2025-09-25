-- | This module exposes functions that are used internally by yesod-test. The functions exposed here are _not_ a stable API—they may be changed or removed without any major version bump.
--
-- That said, you may find them useful if your application can accept API breakage.
module Test.Hspec.Yesod.Internal
    ( RequestBuilderData(..)
    , RBDPostData(..)
    , RequestPart(..)
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Network.HTTP.Types as H
import Network.Wai.Test hiding (assertHeader, assertNoHeader, request)

data RequestBuilderData site = RequestBuilderData
    { rbdPostData :: RBDPostData
    , rbdResponse :: (Maybe SResponse)
    , rbdMethod :: H.Method
    , rbdSite :: site
    , rbdPath :: [T.Text]
    , rbdGets :: H.Query
    , rbdHeaders :: H.RequestHeaders
    }

data RBDPostData = MultipleItemsPostData [RequestPart]
                 | BinaryPostData BSL8.ByteString

-- | Request parts let us discern regular key/values from files sent in the request.
data RequestPart
  = ReqKvPart T.Text T.Text
  | ReqFilePart T.Text FilePath BSL8.ByteString T.Text
