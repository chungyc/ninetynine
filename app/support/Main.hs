{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Main (main) where

import           Problems.BinaryTrees.SVG
import           Problems.P64
import           Problems.P65
import           Problems.P66
import           Problems.P80
import           Problems.P94
import           System.FilePath
import           System.IO
import           System.Process

-- | Tool for automatically generating supporting elements for use in documentation.
-- It is only meant to automatically generate various images and examples
-- when their sources change.
--
-- Requires the dot command-line tool from GraphViz to be installed.
main :: IO ()
main = do
  putStr "Rendering graphs ..."
  hFlush stdout
  renderGraphs
  putStrLn " done"

  putStr "Rendering binary tree layouts .."
  hFlush stdout
  renderBinaryTreeLayouts
  putStrLn " done"

  putStr "Printing regular graph examples ..."
  hFlush stdout
  printRegularGraphExamples
  putStrLn " done"

-- | Render graphs specified in DOT with GraphViz.
-- Requires the dot command-line tool from GraphViz to be installed.
renderGraphs :: IO ()
renderGraphs = do
  mapM_ (call . (</>) ("images" </> "BinaryTrees")) ["tree1", "tree4"]
  mapM_ (call . (</>) ("images" </> "MultiwayTrees")) ["tree1", "tree2", "tree3", "tree4", "tree5"]
  mapM_ (call . (</>) ("images" </> "Graphs")) ["Example", "Example-P83", "Example-P86"]
  mapM_ (call . (</>) ("images" </> "Miscellaneous")) ["Graceful-Tree-P92", "Tree14-P92"]
  where call name = callCommand $ "dot -Tsvg " ++ (name <.> "gv") ++ " -o " ++ (name <.> "svg")

-- | Render binary trees according to their layout to SVG,
-- which will be included with the Haddock documentation.
renderBinaryTreeLayouts :: IO ()
renderBinaryTreeLayouts = do
  writeSVG (dir </> "Layout-P64" <.> "svg") $ layoutInorder tree64
  writeSVG (dir </> "Layout-P65" <.> "svg") $ layoutLevelConstant tree65
  writeSVG (dir </> "Layout-P66" <.> "svg") $ layoutCompact tree65
  where dir = "images" </> "BinaryTrees"

-- | Print examples of regular graphs for "Problems.P94".
-- The output is a valid Haskell module.
printRegularGraphExamples :: IO ()
printRegularGraphExamples = withFile ("src" </> "Problems" </> "P94" </> "Examples" <.> "hs") WriteMode $ \h -> do
  hPutStrLn h "{- |"
  hPutStrLn h "Description: Examples of regular graphs"
  hPutStrLn h "Copyright: Copyright (C) 2021 Yoo Chung"
  hPutStrLn h "License: GPL-3.0-or-later"
  hPutStrLn h "Maintainer: dev@chungyc.org"
  hPutStrLn h ""
  hPutStrLn h "Examples of \\(k\\)-regular graphs with \\(n\\) vertexes."
  hPutStrLn h "-}"
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
