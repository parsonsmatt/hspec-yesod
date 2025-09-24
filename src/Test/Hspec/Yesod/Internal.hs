-- | This module exposes functions that are used internally by yesod-test. The functions exposed here are _not_ a stable API—they may be changed or removed without any major version bump.
--
-- That said, you may find them useful if your application can accept API breakage.
module Test.Hspec.Yesod.Internal
    ( RequestBuilderData(..)
    , RBDPostData(..)
    , RequestPart(..)
    ) where

import Control.Monad.Catch (finally)
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Hooks
import qualified Data.List as DL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TErr
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Test.HUnit as HUnit
import qualified Network.HTTP.Types as H
import qualified Network.Socket as Sock
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Network.Wai
import Network.Wai.Test hiding (assertHeader, assertNoHeader, request)
import Control.Monad.IO.Class
import qualified Control.Monad.State.Class as MS
import Control.Monad.State.Class hiding (get)
import System.IO
import Yesod.Core.Unsafe (runFakeHandler)
import Yesod.Core
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8, decodeUtf8With)
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as C
import qualified Text.HTML.DOM as HD
import qualified Data.Map as M
import qualified Web.Cookie as Cookie
import qualified Blaze.ByteString.Builder as Builder
import Data.Time.Clock (getCurrentTime)
import Control.Applicative ((<$>))
import Text.Show.Pretty (ppShow)
import Data.Monoid (mempty)
import Data.ByteArray.Encoding (convertToBase, Base(..))
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson (eitherDecode')
import Control.Monad
import qualified Yesod.Test.TransversingCSS as YT.CSS
import Yesod.Test.TransversingCSS (HtmlLBS, Query)
import qualified Yesod.Test.Internal.SIO as YT.SIO
import Yesod.Test.Internal.SIO (SIO, execSIO, runSIO)
import qualified Yesod.Test.Internal as YT.Internal (getBodyTextPreview, contentTypeHeaderIsUtf8)

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
