module Network.Wai.Hastache (
		hastache,
		hastacheHTML,
		hastacheText,
		hastacheStr,
		hastacheStrHTML,
		hastacheStrText,
		MuConfig(..),
		MuContext,
		MuType(..)
	) where

import Data.String (IsString, fromString)
import Control.Monad.IO.Class (MonadIO)

import Network.Wai (Response(..))
import Network.HTTP.Types (Status, ResponseHeaders, Header)

import Text.Hastache (hastacheFileBuilder, hastacheStrBuilder, htmlEscape, emptyEscape, MuConfig(..), MuContext, MuType(..))
import Data.ByteString (ByteString)

mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseSource s h b) = ResponseSource s (f h) b

defHeader :: Header -> Response -> Response
defHeader h = mapHeader (defHeader' h)

defHeader' :: Header -> ResponseHeaders -> ResponseHeaders
defHeader' (n, v) headers = case lookup n headers of
		Just _  -> headers
		Nothing -> (n, v):headers

-- | Build a response based on a configuration and a template in a file.
hastache :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> MuConfig -> FilePath -> MuContext m -> m Response
hastache status headers cfg pth ctx = fmap (ResponseBuilder status headers) (
		hastacheFileBuilder cfg pth ctx
	)

-- | Build an HTML-escaped response based on a template in a file.
--   Defaults Content-Type to text/html if you do not specify.
hastacheHTML :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> FilePath -> MuContext m -> m Response
hastacheHTML status headers pth ctx = defHeader defCT `fmap` hastache status headers
	(MuConfig htmlEscape Nothing (Just "mustache")) pth ctx
	where
	defCT = (fromString "Content-Type", fromString "text/html; charset=utf-8")

-- | Build an unescaped response based on a template in a file.
--   Defaults Content-Type to text/plain if you do not specify.
hastacheText :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> FilePath -> MuContext m -> m Response
hastacheText status headers pth ctx = defHeader defCT `fmap` hastache status headers
	(MuConfig emptyEscape Nothing (Just "mustache")) pth ctx
	where
	defCT = (fromString "Content-Type", fromString "text/plain; charset=utf-8")

-- | Build a response based on a configuration and a template in a 'ByteString'.
hastacheStr :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> MuConfig -> ByteString -> MuContext m -> m Response
hastacheStr status headers cfg tpl ctx = fmap (ResponseBuilder status headers) (
		hastacheStrBuilder cfg tpl ctx
	)

-- | Build an HTML-escaped response based on a template in a 'ByteString'.
--   Defaults Content-Type to text/html if you do not specify.
hastacheStrHTML :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> ByteString -> MuContext m -> m Response
hastacheStrHTML status headers tpl ctx = defHeader defCT `fmap` hastacheStr status headers
	(MuConfig htmlEscape Nothing (Just "mustache")) tpl ctx
	where
	defCT = (fromString "Content-Type", fromString "text/html; charset=utf-8")

-- | Build an unescaped response based on a template in a 'ByteString'.
--   Defaults Content-Type to text/plain if you do not specify.
hastacheStrText :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> ByteString -> MuContext m -> m Response
hastacheStrText status headers tpl ctx = defHeader defCT `fmap` hastacheStr status headers
	(MuConfig htmlEscape Nothing (Just "mustache")) tpl ctx
	where
	defCT = (fromString "Content-Type", fromString "text/plain; charset=utf-8")
