--------------------------------------------------------------------------------
-- | A very light wrapper around the PCRE2 library.
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Text.Pcre2.Internal
    ( PcreException (..)

    , CompileOptions (..)
    , defaultCompileOptions
    , Regex (..)
    , compile

    , OVector (..)
    , withMatchData
    , readMatchData

    , MatchOptions (..)
    , defaultMatchOptions
    , Match (..)
    , Range (..)
    , ovectorsToMatches
    , countUtf8Characters

    , match
    ) where

import           Control.Exception      (Exception, bracket, throwIO)
import           Control.Monad          (forM, when)
import           Data.Bits              ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Word              (Word8)
import           Data.Word              (Word32)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

--------------------------------------------------------------------------------
-- Foreign imports and types.

-- These are to mock void* pointers.
data CompileContext
data Code
data MatchData
data MatchContext

foreign import ccall unsafe "pcre2_compile_8" pcre2_compile_8
    :: Ptr CChar           -- pattern
    -> CSize               -- length
    -> Word32              -- options
    -> Ptr CInt            -- errorcode
    -> Ptr CSize           -- erroroffset
    -> Ptr CompileContext  -- ccontext
    -> IO (Ptr Code)       -- code

foreign import ccall unsafe "&pcre2_code_free_8" pcre2_code_free_8
    :: FunPtr (Ptr Code -> IO ())

foreign import ccall unsafe "pcre2_get_error_message_8" pcre2_get_error_message_8
    :: CInt       -- errorcode
    -> Ptr CChar  -- buffer
    -> CSize      -- bufflen
    -> IO CInt

foreign import ccall unsafe "pcre2_match_data_create_from_pattern_8" pcre2_match_data_create_from_pattern_8
    :: Ptr Code
    -> Ptr CompileContext
    -> IO (Ptr MatchData)

foreign import ccall unsafe "pcre2_match_data_free_8" pcre2_match_data_free_8
    :: Ptr MatchData -> IO ()

foreign import ccall unsafe "pcre2_match_8" pcre2_match_8
    :: Ptr Code          -- code
    -> Ptr CChar         -- subject
    -> CSize             -- length
    -> CSize             -- startoffset
    -> Word32            -- options
    -> Ptr MatchData     -- match_data
    -> Ptr MatchContext  -- mcontext
    -> IO CInt

foreign import ccall unsafe "pcre2_get_ovector_count_8" pcre2_get_ovector_count_8
    :: Ptr MatchData  -- match_data
    -> IO Word32

foreign import ccall unsafe "pcre2_get_ovector_pointer_8" pcre2_get_ovector_pointer_8
    :: Ptr MatchData  -- match_data
    -> IO (Ptr CSize)

--------------------------------------------------------------------------------
-- Foreign imports from our own little library

foreign import ccall unsafe "pcre_simple_compile_options" pcre_simple_compile_options
    :: CInt       -- caseless
    -> CInt       -- multiline
    -> CInt       -- ucp
    -> CInt       -- utf
    -> IO Word32

foreign import ccall unsafe "pcre_simple_match_options" pcre_simple_match_options
    :: CInt       -- notempty_atstart
    -> IO Word32

--------------------------------------------------------------------------------
-- Exceptions

data PcreException
    = PcreCompileException Int String
    | PcreMatchException String
    | PcreInternalException String
    deriving (Eq)

instance Show PcreException where
    show (PcreCompileException offset msg) =
        "PCRE2 compilation failed at offset " ++ show offset ++ ": " ++ msg
    show (PcreMatchException msg) =
        "PCRE2 match failed: " ++ msg
    show (PcreInternalException msg) =
        "PCRE2 internal exception: " ++ msg

instance Exception PcreException

getErrorMessage :: CInt -> IO String
getErrorMessage errorCode = allocaBytes 256 $ \buffer -> do
    -- Convert it to a bytestring and then to a 'T.Text' value.
    bufferLen <- pcre2_get_error_message_8 errorCode buffer 256
    bs        <- B.unsafePackCStringLen (buffer, fromIntegral bufferLen)
    let msg = T.decodeUtf8 bs
    msg `seq` return (T.unpack msg)

--------------------------------------------------------------------------------
-- Compiling a regex

data CompileOptions = CompileOptions
    { coCaseless  :: Bool
    , coMultiline :: Bool
    , coUcp       :: Bool
    , coUtf       :: Bool
    } deriving (Show)

defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions
    { coCaseless   = False
    , coMultiline  = False
    , coUcp        = True
    , coUtf        = True
    }

newtype Regex = Regex (ForeignPtr Code)

compile :: CompileOptions -> T.Text -> IO Regex
compile opts pattern0 =
    alloca $ \errorNumberPtr ->
    alloca $ \errorOffsetPtr ->
    unsafeWithUtf8Text pattern0 $ \(str, len) -> do

        options <- pcre_simple_compile_options
            (if coCaseless opts then 1 else 0)
            (if coMultiline opts then 1 else 0)
            (if coUcp opts then 1 else 0)
            (if coUtf opts then 1 else 0)

        codePtr <- pcre2_compile_8 str
            (fromIntegral len) options errorNumberPtr errorOffsetPtr nullPtr

        when (codePtr == nullPtr) $ do
            errorNumber <- peek errorNumberPtr
            errorOffset <- fromIntegral <$> peek errorOffsetPtr
            msg         <- getErrorMessage errorNumber
            throwIO $ PcreCompileException errorOffset msg

        fptr <- newForeignPtr pcre2_code_free_8 codePtr
        return $ Regex fptr

--------------------------------------------------------------------------------
-- Ovector type and operations

data OVector = OVector CSize CSize [(CSize, CSize)]
    deriving (Show)

withMatchData :: Regex -> (Ptr MatchData -> IO a) -> IO a
withMatchData (Regex regexFPtr) f = withForeignPtr regexFPtr $ \regexPtr ->
    bracket
        (pcre2_match_data_create_from_pattern_8 regexPtr nullPtr)
        pcre2_match_data_free_8
        f

readMatchData :: Ptr MatchData -> IO OVector
readMatchData matchDataPtr = do
    count  <- fromIntegral <$> pcre2_get_ovector_count_8 matchDataPtr
    vect   <- pcre2_get_ovector_pointer_8 matchDataPtr
    list   <- peekArray (count * 2) vect
    case list of
        (x : y : t) -> return (OVector x y (per2 t))
        _           -> throwIO $ PcreInternalException "no results in ovector"
  where
    per2 (x : y : t) = (x, y) : per2 t
    per2 _           = []

--------------------------------------------------------------------------------
-- Match type and operations

data MatchOptions = MatchOptions {}
    deriving (Show)

defaultMatchOptions :: MatchOptions
defaultMatchOptions = MatchOptions

data Match = Match !Range [Range] deriving (Eq, Show)

data Range = Range {rStart :: !Int, rLength :: !Int} deriving (Eq, Show)

-- | An 'OVector' deals with byte offsets, and we want character offsets and
-- lengths.  This function does the conversion in a reasonably efficient way.
ovectorsToMatches :: Ptr CChar -> [OVector] -> IO [Match]
ovectorsToMatches ptr0 = go 0 0
  where
    go _numCharsBeforeOffset _offset [] = return []
    go numCharsBeforeOffset offset (OVector start end subs : vecs) = do
        numCharsBetweenOffsetAndStart <- countUtf8Characters
            (ptr0 `plusPtr` fromIntegral offset)
            (ptr0 `plusPtr` fromIntegral start)
        numCharsBetweenStartAndEnd <- countUtf8Characters
            (ptr0 `plusPtr` fromIntegral start)
            (ptr0 `plusPtr` fromIntegral end)

        let !matchStart  = numCharsBeforeOffset + numCharsBetweenOffsetAndStart
            !matchLength = numCharsBetweenStartAndEnd

        matchSubs <- forM subs $ \(subStart, subEnd) -> do
            numCharsBeforeSub <- countUtf8Characters
                (ptr0 `plusPtr` fromIntegral start)
                (ptr0 `plusPtr` fromIntegral subStart)
            subLen <- countUtf8Characters
                (ptr0 `plusPtr` fromIntegral subStart)
                (ptr0 `plusPtr` fromIntegral subEnd)
            return $ Range (matchStart + numCharsBeforeSub) subLen

        matches <- go (matchStart + matchLength) end vecs
        return $ Match (Range matchStart matchLength) matchSubs : matches

-- | We know that between 'start' and 'end', there are exactly 'end - start'
-- bytes.  However, we want to know how many /characters/ there are in between
-- those offsets, and a character may be composed out of multiple bytes.
--
-- 'start' and 'end' should always cleanly point to the start of a new character
-- (which is fortunately what PCRE guarantees).
countUtf8Characters :: Ptr Word8 -> Ptr Word8 -> IO Int
countUtf8Characters start end = do
    go 0 start
  where
    go !acc p
        | p >= end  = return acc
        | otherwise = do
            x <- peek p
            if | x .&. 0x80 == 0x00 -> go (acc + 1) (p `plusPtr` 1)
               | x .&. 0xE0 == 0xC0 -> go (acc + 1) (p `plusPtr` 2)
               | x .&. 0xF0 == 0xE0 -> go (acc + 1) (p `plusPtr` 3)
               -- Otherwise: x .&. 0xF8 == 0xF0
               | otherwise          -> go (acc + 1) (p `plusPtr` 4)

--------------------------------------------------------------------------------
-- Matching

match :: MatchOptions -> Regex -> T.Text -> IO [Match]
match _options regex@(Regex regexFPtr) subject0 =
    unsafeWithUtf8Text subject0 $ \(subject, len) ->
    withForeignPtr regexFPtr $ \regexPtr ->
    withMatchData regex $ \matchDataPtr -> do
        let go offset prevMatchWasEmpty
                | offset > len = return []
                | otherwise    = do
                    -- Prepare options
                    options <- pcre_simple_match_options
                        (if prevMatchWasEmpty then 1 else 0)

                    -- Matching call
                    rc <- pcre2_match_8
                        regexPtr
                        subject
                        (fromIntegral len)
                        (fromIntegral offset)
                        options
                        matchDataPtr
                        nullPtr

                    case rc of
                        -- OVector not big enough
                        0 -> throwIO $ PcreInternalException "ovector not big enough"

                        -- No match found.
                        -1 -> return []

                        -- Matches found.
                        _ | rc > 0 -> do
                            ovect@(OVector start end _) <- readMatchData matchDataPtr
                            let wasEmpty = start == end
                            vects <- go (fromIntegral end) wasEmpty
                            return (ovect : vects)

                        -- Actual error
                        _ -> do
                            msg <- getErrorMessage rc
                            throwIO $ PcreMatchException msg

        ovects <- go 0 False
        ovectorsToMatches subject ovects

--------------------------------------------------------------------------------
-- Internal

-- | This is a helper function like `B.unsafeUseAsCStringLen`.
--
-- If we have an empty string, `B.unsafeUseAsCStringLen` will give you the
-- equivalent of
--
-- > (ptr=NULL, len=0)
--
-- and a library like PCRE2 can't really deal with that elegantly.  PCRE2 does
-- not expect a NULL pointer.  So in order to solve this, we allocate a single
-- character and then use:
--
-- > (ptr=CHRPTR, len=0)
--
-- PCRE2 does not touch CHRPTR except to compare it with NULL.
unsafeWithUtf8Text :: T.Text -> ((Ptr CChar, Int) -> IO a) -> IO a
unsafeWithUtf8Text text f
    | B.null bytestring =
        -- We're using `B.unsafeUseAsCStringLen` for convenience but really any
        -- allocation would do.
        B.unsafeUseAsCStringLen "\0" $ \(ptr, _size) -> f (ptr, 0)
    | otherwise         =
        B.unsafeUseAsCStringLen bytestring f
  where
    bytestring = T.encodeUtf8 text
