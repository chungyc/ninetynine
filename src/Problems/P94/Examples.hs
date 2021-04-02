{- |
Description: Examples of regular graphs
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Examples of \(k\)-regular graphs with \(n\) vertexes.
-}
module Problems.P94.Examples where

import Problems.Graphs

-- | Examples of k-regular graphs with n vertexes.
-- Inspect the source code to view the examples.
regularGraphExamples :: [Paths]
regularGraphExamples = [

  -- (n,k) = (3,2) with 1 solution
  Paths [[1,3,2,1]],

  -- (n,k) = (4,2) with 1 solution
  Paths [[1,4,2,3,1]],

  -- (n,k) = (4,3) with 1 solution
  Paths [[4,3],[2,4,1,3,2,1]],

  -- (n,k) = (5,2) with 1 solution
  Paths [[1,5,2,3,4,1]],

  -- (n,k) = (5,3) with 0 solutions

  -- (n,k) = (5,4) with 1 solution
  Paths [[3,5,4,3],[1,5,2,4,1,3,2,1]],

  -- (n,k) = (6,2) with 2 solutions
  Paths [[1,6,2,4,3,5,1]],
  Paths [[2,4,3,2],[1,6,5,1]],

  -- (n,k) = (6,3) with 2 solutions
  Paths [[6,3],[5,3,4],[2,6,1,5,2,4,1]],
  Paths [[5,4],[6,3],[2,6,1,5,2,3,4,1]],

  -- (n,k) = (6,4) with 1 solution
  Paths [[3,6,4,5,3],[1,6,2,5,1,4,2,3,1]],

  -- (n,k) = (6,5) with 1 solution
  Paths [[6,5],[4,6,3,5,4,3],[2,6,1,5,2,4,1,3,2,1]],

  -- (n,k) = (7,2) with 2 solutions
  Paths [[3,5,4,3],[1,7,2,6,1]],
  Paths [[1,7,2,5,4,3,6,1]],

  -- (n,k) = (7,3) with 0 solutions

  -- (n,k) = (7,4) with 2 solutions
  Paths [[3,6,5,3,7,4,3],[1,7,2,6,1,5,2,4,1]],
  Paths [[3,7,4,5,6,3],[1,7,2,6,1,5,2,3,4,1]],

  -- (n,k) = (7,5) with 0 solutions

  -- (n,k) = (7,6) with 1 solution
  Paths [[5,7,6,5],[3,7,4,6,3,5,4,3],[1,7,2,6,1,5,2,4,1,3,2,1]],

  -- (n,k) = (8,2) with 3 solutions
  Paths [[3,6,4,5,3],[1,8,2,7,1]],
  Paths [[1,8,2,6,4,5,3,7,1]],
  Paths [[3,5,4,3],[1,8,2,6,7,1]],

  -- (n,k) = (8,3) with 6 solutions
  Paths [[7,4],[8,3],[5,4,3,5,6],[2,8,1,7,2,6,1]],
  Paths [[7,4],[6,4,5],[8,3],[2,8,1,7,2,5,3,6,1]],
  Paths [[6,5],[7,4],[8,3],[2,8,1,7,2,5,4,3,6,1]],
  Paths [[7,5],[6,4],[8,3],[2,8,1,7,2,5,4,3,6,1]],
  Paths [[8,7],[6,4],[5,3],[2,8,1,7,2,5,4,3,6,1]],
  Paths [[5,4],[3,5,2,4,3,2],[8,7],[6,8,1,7,6,1]],

  -- (n,k) = (8,4) with 6 solutions
  Paths [[3,8,4,7,3,6,4,5,3],[1,8,2,7,1,6,2,5,1]],
  Paths [[3,8,4,7,3,6,5,4,3],[1,8,2,7,1,6,2,5,1]],
  Paths [[3,8,4,6,5,7,3],[1,8,2,7,1,6,2,4,3,5,1]],
  Paths [[4,6,5,4],[3,8,7,3],[1,8,2,7,1,6,2,4,3,5,1]],
  Paths [[5,7,6,5],[1,7,2,8,3,6,1,8,4,3,2,4,5,1]],
  Paths [[2,8,6,5,7,2],[1,8,3,6,1,7,4,3,2,4,5,1]],

  -- (n,k) = (8,5) with 3 solutions
  Paths [[7,6],[8,5],[4,8,3,6,5,3,7,4,3],[2,8,1,7,2,6,1,5,2,4,1]],
  Paths [[7,6],[8,4],[3,7,4,6,3,8,5],[2,8,1,7,2,5,3,2,6,1,5,4,1]],
  Paths [[7,6],[8,5],[4,8,3,7,4,5,6,3],[2,8,1,7,2,6,1,5,2,3,4,1]],

  -- (n,k) = (8,6) with 1 solution
  Paths [[5,8,6,7,5],[3,8,4,7,3,6,4,5,3],[1,8,2,7,1,6,2,5,1,4,2,3,1]],

  -- (n,k) = (8,7) with 1 solution
  Paths [[8,7],[6,8,5,7,6,5],[4,8,3,7,4,6,3,5,4,3],[2,8,1,7,2,6,1,5,2,4,1,3,2,1]],

  -- (n,k) = (9,2) with 4 solutions
  Paths [[3,7,4,5,6,3],[1,9,2,8,1]],
  Paths [[4,6,5,4],[1,9,2,7,3,8,1]],
  Paths [[1,9,2,7,4,5,6,3,8,1]],
  Paths [[3,5,4,3],[2,7,6,2],[1,9,8,1]],

  -- (n,k) = (0,0) with 1 solution
  Paths []

  ]
