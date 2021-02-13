module Solutions.P12 (decodeModified) where

import           Problems.P11.Definitions

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
decodeModified :: [Encoding a] -> [a]
decodeModified []                  = []
decodeModified (Single x : xs)     = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs
