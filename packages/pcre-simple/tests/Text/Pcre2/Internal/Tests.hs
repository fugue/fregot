module Text.Pcre2.Internal.Tests
    ( tests
    ) where

import qualified Data.ByteString.Unsafe as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Word              (Word8)
import           Foreign.Ptr            (Ptr, castPtr, plusPtr)
import qualified Test.QuickCheck        as QC
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.QuickCheck  as Tasty
import qualified Text.Pcre2.Internal    as Pcre2

tests :: Tasty.TestTree
tests = Tasty.testGroup "Ludwig.Pcre2.Internal.Tests"
    [ countUtf8Characters_prop01
    ]

-- | Check for consistency with string length after UTF-8 encoding.
countUtf8Characters_prop01 :: Tasty.TestTree
countUtf8Characters_prop01 = Tasty.testProperty "countUtf8Characters_prop01" $
    \string ->
        let bs = T.encodeUtf8 (T.pack string) in
        QC.ioProperty $ B.unsafeUseAsCStringLen bs $ \(ptr0, len) -> do
            let startPtr = castPtr ptr0           :: Ptr Word8
                endPtr   = startPtr `plusPtr` len :: Ptr Word8
            count <- Pcre2.countUtf8Characters startPtr endPtr
            return $ length string == count
