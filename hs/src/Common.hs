module Common (count, readEntire) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T (null)
import Data.Text.Read (Reader)

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

readEntire :: Reader a -> Text -> Either String a
readEntire reader input = do
    (a, t) <- reader input
    if T.null t then Right a else Left "incomplete read"
