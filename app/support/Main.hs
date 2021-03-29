import           Problems.BinaryTrees.SVG
import           Problems.P64
import           Problems.P65
import           Problems.P66
import           Problems.P80
import           Problems.P94
import           System.IO

main :: IO ()
main = do
  renderBinaryTreeLayouts
  printRegularGraphExamples

-- | Render binary trees according to their layout to SVG,
-- which will be included with the Haddock documentation.
renderBinaryTreeLayouts :: IO ()
renderBinaryTreeLayouts = do
  writeSVG "images/BinaryTrees/Layout-P64.svg" $ layoutInorder tree64
  writeSVG "images/BinaryTrees/Layout-P65.svg" $ layoutLevelConstant tree65
  writeSVG "images/BinaryTrees/Layout-P66.svg" $ layoutCompact tree65

-- | Print examples of regular graphs for "Problems.P94".
-- The output is a valid Haskell module.
printRegularGraphExamples :: IO ()
printRegularGraphExamples = withFile "src/Problems/P94/Examples.hs" WriteMode $ \h -> do
  hPutStrLn h "-- | Examples of k-regular graphs with n vertexes."
  hPutStrLn h "module Problems.P94.Examples where"
  hPutStrLn h ""
  hPutStrLn h "import Problems.Graphs"
  hPutStrLn h ""
  hPutStrLn h "-- | Examples of k-regular graphs with n vertexes."
  hPutStrLn h "-- Inspect the source code to view the examples."
  hPutStrLn h "regularGraphExamples :: [Paths]"
  hPutStrLn h "regularGraphExamples = ["
  hPutStrLn h ""
  mapM_ (printRegularGraphs h) $ [(n,k) | n <- [3..8], k <- [2..n-1]] ++ [(9,2)]
  -- Last manual output for the sake of avoiding an errant comma.
  hPutStrLn h "  -- (n,k) = (0,0) with 1 solution"
  hPutStrLn h "  Paths []"
  hPutStrLn h ""
  hPutStrLn h "  ]"

printRegularGraphs :: Handle -> (Int,Int) -> IO ()
printRegularGraphs h params@(n,k) = do
  let gs = regularGraphs n k
  hPutStr h "  -- (n,k) = "
  hPutStr h $ show params
  hPutStr h " with "
  hPutStr h $ show $ length gs
  hPutStrLn h $ case length gs of 1 -> " solution"
                                  _ -> " solutions"
  mapM_ showGraph gs
  hPutStrLn h ""
  where showGraph g = do
          hPutStr h "  "
          hPutStr h $ show $ toPaths g
          hPutStrLn h ","
