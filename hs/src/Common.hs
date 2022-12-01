module Common (readEntire) where

import Data.Text (Text)
import qualified Data.Text as T (null)
import Data.Text.Read (Reader)

readEntire :: Reader a -> Text -> Either String a
readEntire reader input = do
    (a, t) <- reader input
    if T.null t then Right a else Left "incomplete read"
