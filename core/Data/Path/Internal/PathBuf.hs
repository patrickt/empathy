{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Provides the internal interface for 'Data.Path'. Relying on this module is not recommended.
module Data.Path.Internal.PathBuf (module Data.Path.Internal.PathBuf) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Build
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.RawFilePath (RawFilePath)
import Data.Text.Encoding qualified as E
import Data.Text.Encoding.Error qualified as E
import Data.Text (Text)
import Data.Data (Data)
import Data.Coerce
import Control.Monad.IO.Class
import Foreign.C.String qualified as F
import GHC.Stack (HasCallStack)
import Data.Hashable (Hashable)
import Data.Path.Internal.Component
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Control.DeepSeq (NFData)
import Data.List qualified as List
import Data.Word (Word8)

-- | An opaque blob of bytes representing a potential path to a file or directory.
--
-- No guarantees are made that a given 'PathBuf' is syntactically valid.
-- You should perform validity checks before invoking filesystem operations.
newtype PathBuf = PathBuf { bytes :: RawFilePath }
  deriving stock (Eq, Ord, Data)
  deriving newtype (NFData, Hashable)

-- | /O(1)/. Create a 'PathBuf' from a strict 'ByteString' or 'RawFilePath'.
fromBytes :: B.ByteString -> PathBuf
fromBytes = PathBuf

-- | /O(n)/. Create a 'PathBuf' from a 'String' (a list of 'Char' values.)
-- This is subject to the same caveats as 'BC.pack': all characters will
-- be truncated to eight bits.
fromString :: String -> PathBuf
fromString = coerce BC.pack

-- | /O(n)/. Create a 'PathBuf' from a 'Text' value. The buffer will be
-- stored as UTF-8.
fromText :: Text -> PathBuf
fromText = coerce E.encodeUtf8

-- | /O(1)/. View the contents of a 'PathBuf' as a 'ByteString' or 'RawFilePath'.
toBytes :: PathBuf -> B.ByteString
toBytes = bytes

-- | /O(n)/. Convert a 'PathBuf' to a 'String'. This is subject to the
-- same caveats as 'BC.pack': all characters will be truncated to
-- eight bits.
toString :: PathBuf -> String
toString = coerce BC.unpack

-- | /O(n)/. Convert a 'PathBuf' to a 'Text'. This may fail if the provided
-- 'PathBuf' cannot be represented as UTF-8. For a variant that replaces invalid
-- sequences with @U+FFFD REPLACEMENT CHARACTER@, use 'toText''. For a variant
-- that calls 'throw' if it encounters an 'E.UnicodeException', use 'toTextUnsafe'.
toText :: PathBuf -> Either E.UnicodeException Text
toText = coerce E.decodeUtf8'

-- | /O(n)/. Convert a 'PathBuf' to a 'Text'. Any invalid Unicode sequences are
-- replaced with  @U+FFFD REPLACEMENT CHARACTER@, which means that information may
-- be lost in roundtripping.
toText' :: PathBuf -> Text
toText' p = E.decodeUtf8With E.lenientDecode (coerce p)

-- | /O(n)/. Convert a 'PathBuf' to a 'Text'. The presence of invalid Unicode sequences
-- will throw an 'E.UnicodeException'. To handle this, either use 'Control.Exception.Catch'
-- or handle the 'Left' case when calling the safe 'toText'.
toTextUnsafe :: HasCallStack => PathBuf -> Either E.UnicodeException Text
toTextUnsafe = coerce E.decodeUtf8'

-- * Queries

isPrefixOf :: PathBuf -> PathBuf -> Bool
isPrefixOf = List.isPrefixOf `on` components

isSuffixOf :: PathBuf -> PathBuf -> Bool
isSuffixOf = List.isSuffixOf `on` components

hasRoot :: PathBuf -> Bool
hasRoot = error "unimplemented"

isAbsolute :: PathBuf -> Bool
isAbsolute = error "unimplemented"

isRelative :: PathBuf -> Bool
isRelative = error "unimplemented"

-- * Path manipulation

-- | Returns the argument without its final path component. Returns 'Nothing' if
-- the path is a root or prefix.
parent :: PathBuf -> Maybe PathBuf
parent = error "unimplemented"

-- | Returns the final component of the given path. If this is a normal file, it
-- returns the filename. If it's a directory, it returns the directory name.
-- If the path terminates in @..@, 'Nothing' is returned.
lastComponent :: PathBuf -> Maybe RawFilePath
lastComponent = error "unimplemented"

-- | Updates the last component of the 'PathBuf' to be the provided raw path.
-- If calling 'lastComponent' would return 'Nothing', then this is equivalent to
-- calling 'join'; otherwise, it is equivalent to calling 'join' on the path's
-- 'parent'.
setLastComponent :: PathBuf -> RawFilePath -> PathBuf
setLastComponent = error "unimplemented"

-- | Returns all but the last component of the provided path. Returns 'Nothing'
-- if the path is a root or prefix.
--
-- @
-- ('pathStem') ≡ fmap 'bytes' . 'parent'
-- @
pathStem :: PathBuf -> Maybe RawFilePath
pathStem = error "unimplemented"

setPathStem :: PathBuf -> RawFilePath -> PathBuf
setPathStem = error "unimplemented"

-- | Produces a list of the provided 'PathBuf' along with its ancestors, equivalent
-- to repeated 'parent' calls.
--
-- TODO: should this be something other than 'RawFilePath'?
ancestors :: PathBuf -> NonEmpty RawFilePath
ancestors = error "unimplemented"

components :: PathBuf -> [Component]
components = error "unimplemented"

rawComponents :: PathBuf -> NonEmpty RawFilePath
rawComponents = error "unimplemented"

-- | Extracts the extension of the 'lastComponent' of the file path, if possible.
-- If there is no file name, no embedded @.@, or the file begins with @.@ and has
-- no other @.@ characters, the result will be 'Nothing'.
extension :: PathBuf -> Maybe RawFilePath
extension = error "unimplemented"

-- | Updates the 'extension' of the provided 'PathBuf' to the provided 'RawFilePath'.
-- If the result of calling 'extension' on the file path is 'Nothing', then this
-- function is a no-op. Otherwise, the extension will be updated.
setExtension :: PathBuf -> RawFilePath -> PathBuf
setExtension = error "unimplemented"

hasPrefix :: PathBuf -> Bool
hasPrefix = error "unimplemented"

-- | @join base new@ creates a new path using @base@ and @new@. If @new@ is
-- an absolute path, it replaces @base@. Otherwise, it appends @new@ to @base@
-- with an appropriate directory separator. The behavior given absolute paths may
-- surprising, but was chosen to match the semantics of Rust's @PathBuf@ type.
join :: PathBuf -> PathBuf -> PathBuf
join self path = do
  if
    -- An absolute 'path' replaces 'self'.
    | isAbsolute path || hasPrefix path -> path
    -- 'path' has a root but no prefix, e.g., `\windows` (Windows only)
    | hasRoot path -> error "need to implement prefixRemaining on Component"
    | otherwise -> PathBuf . BL.toStrict . Build.toLazyByteString $ built
      where
        built = buildPathBuf self <> Build.word8 platformSeparator <> buildPathBuf path
        buildPathBuf = coerce Build.byteString

platformSeparator :: Word8
platformSeparator = 47 -- /

-- IO stuff

-- | /O(n)/. Returns the canonical, absolute form of a path, resolving symbolic links and
-- removing intermediate characters. On Unix systems, this calls @realpath(3)@; on Windows,
-- it opens the file and calls @GetFinalPathNameByHandle@. Error codes resulting from the
-- above call will be returned in the 'Left' case.
canonicalize :: MonadIO m => PathBuf -> m (Either IOError PathBuf)
canonicalize = error "unimplemented"

exists :: MonadIO m => PathBuf -> m Bool
exists = error "unimplemented"

pointsToDirectory :: MonadIO m => PathBuf -> m Bool
pointsToDirectory = error "unimplemented"

pointsToFile :: MonadIO m => PathBuf -> m Bool
pointsToFile = error "unimplemented"

readDirectoryContents :: MonadIO m => PathBuf -> m (Maybe [PathBuf])
readDirectoryContents = undefined



-- readFile and friends too

-- TODO:
-- metadata :: MonadIO m => PathBuf -> m Metadata

-- FFI stuff

-- | /O(n)/. Convert a 'CString' to a 'PathBuf'. The 'CString' must be null-terminated.
fromCString :: MonadIO m => F.CString -> m PathBuf
fromCString f = fromBytes <$> liftIO (B.packCString (coerce f))

-- | /O(n)/. Copy a 'PathBuf' into a CString and use it in an 'IO' action.
withCString :: MonadIO m => PathBuf -> (F.CString -> IO a) -> m a
withCString b f = liftIO (B.useAsCString (coerce b) f)
