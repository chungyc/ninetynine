{-|
Description: Render binary tree with layout to SVG
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

For a 'Tree' which has been laid out with position \((x,y)\) for every node,
such as with 'Problems.P64.layoutInorder', turns it into [SVG](https://www.w3.org/Graphics/SVG/)
so that the layout can be viewed graphically.  Only trees with 'Char' values are supported.

This is used to generate the graphical representation of binary trees
for inclusion in the Haddock documentation generated for the layout problems.
Doing it manually once was a painstaking and error-prone process;
the thought of doing it two more times was too much.
It could also be used to graphically explore alternative layout methods
that are not included in one of the problems.
-}
module Problems.BinaryTrees.SVG (toSVG, prettyXML, writeSVG, XML) where

import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as Text
import           Prettyprinter
import           Prettyprinter.Render.Text
import           Problems.BinaryTrees
import           System.IO

-- | Represents XML content for the purposes of rendering 'Tree' to SVG.
data XML
  -- | Reprsents an XML element.
  = E String [Attribute] [XML]
  -- | Represents text content.
  | T Text deriving Show

-- | Represents an attribute for an XML element.
data Attribute = A String String deriving Show

-- | Renders a binary tree with 'Char' values with annotated layout to SVG.
toSVG :: Tree (Char, (Int,Int)) -> XML
toSVG t = E "svg"
  [ A "version" "1.1"
  , A "baseProfile" "full"
  , A "width" "640"
  , A "viewBox" viewbox
  , A "preserveAspectRatio" "xMinYMin meet"
  , A "xmlns" "http://www.w3.org/2000/svg"
  ]
  [ E "g"
    [ A "font-family" "sans-serif"
    , A "font-size" "40"
    , A "dominant-baseline" "central"
    , A "text-anchor" "middle"
    ]
    [ toGridLabelSVG size
    , toGridSVG size
    , toEdgeSVG es
    , toNodeSVG ns
    ]
  ]
  where ns = nodes t
        es = edges t
        size@(w,h) = gridSize ns
        viewbox = concat ["0 0 ", c (fromIntegral w + 0.5 :: Float)
                         , " ",   c (fromIntegral h + 0.5 :: Float)
                         ]

-- | Writes the SVG for the binary tree to the given file.
writeSVG :: FilePath -> Tree (Char, (Int,Int)) -> IO ()
writeSVG path tree = withFile path WriteMode $
  \h -> renderIO h $ layoutPretty defaultLayoutOptions $
        prettyXML $ toSVG tree

-- | Labels for the grid axes.
toGridLabelSVG :: (Int,Int) -> XML
toGridLabelSVG (w, h) = E "g" [] $ xlabels ++ ylabels
  where xlabels = map xlabel [1..w]
        ylabels = map ylabel [1..h]
        xlabel x = E "text" [A "x" $ c x, A "y" $ c offset] [T $ Text.pack $ show x]
        ylabel y = E "text" [A "x" $ c offset, A "y" $ c y] [T $ Text.pack $ show y]
        offset = 0.3 :: Float

-- | Grid lines.
toGridSVG :: (Int,Int) -> XML
toGridSVG (w, h) = E "g" [A "stroke" "black"] $ xlines ++ ylines
  where xlines = map xline [1..w]
        ylines = map yline [1..h]
        xline x = E "line" [ A "x1" $ c x
                           , A "x2" $ c x
                           , A "y1" $ c (1 :: Int)
                           , A "y2" $ c h
                           ] []
        yline y = E "line" [ A "x1" $ c (1 :: Int)
                           , A "x2" $ c w
                           , A "y1" $ c y
                           , A "y2" $ c y
                           ] []

-- | The circle and label for the nodes.
toNodeSVG :: [(Text, (Int,Int))] -> XML
toNodeSVG ns = E "g" [] $ [circles, labels]
  where circles = E "g" [A "fill" "white",
                         A "stroke" "black",
                         A "stroke-width" "3"] $ map circle ns
        labels = E "g" [] $ map label ns
        circle (_, (x,y)) = E "circle" [ A "r" $ c (0.4 :: Float)
                                       , A "cx" $ c x
                                       , A "cy" $ c y
                                       ] []
        label (l, (x,y)) = E "text" [ A "x" $ c x
                                    , A "y" $ c y
                                    ] [ T l ]

-- | The lines for the edges.
toEdgeSVG :: [((Int,Int), (Int,Int))] -> XML
toEdgeSVG es = E "g" [A "stroke" "black", A "stroke-width" "3"] edgeLines
  where edgeLines = map edgeLine es
        edgeLine ((x1,y1), (x2,y2)) = E "line" [ A "x1" $ c x1
                                               , A "y1" $ c y1
                                               , A "x2" $ c x2
                                               , A "y2" $ c y2] []

-- | Gather labels and layout positions for each node in the binary tree.
nodes :: Tree (Char, (Int,Int)) -> [(Text, (Int,Int))]
nodes Empty                 = []
nodes (Branch (x, pos) l r) = (Text.pack [x], pos) : nodes l ++ nodes r

-- | Gather the layout positions for the end points of each edge in the binary tree.
edges :: Tree (a, (Int,Int)) -> [((Int,Int),(Int,Int))]
edges Empty                 = []
edges (Branch (_, pos) l r) = edge l ++ edge r ++ edges l ++ edges r
  where edge Empty                  = []
        edge (Branch (_, pos') _ _) = [(pos,pos')]

-- | Returns the size of the grid necessary to represent all nodes.
-- Assumes that the smallest layout position is (1,1).
gridSize :: [(a, (Int,Int))] -> (Int,Int)
gridSize ns = (w, h)
  where w = maximum $ map (fst . snd) ns
        h = maximum $ map (snd . snd) ns

-- | Converts the coordinate used for binary tree layout
-- to a coordinate in the view box for the SVG image.
c :: (Num a, Show a) => a -> String
c x = show $ 100 * x

-- | Pretty print XML, so we have prettier output instead of one very long line.
--
-- Supports a limited subset of XML sufficient to output SVG generated by 'toSVG'.
prettyXML :: XML -> Doc ann
prettyXML (T t) = pretty t
prettyXML (E name attributes []) =
  sep $ [pretty $ "<" ++ name, prettyAttributes attributes, pretty "/>"]
prettyXML (E name attributes xs) =
  cat $ [ nest 2 $ cat [startTag attributes, prettyContent xs], endTag ]
  where startTag []    = angles $ pretty name
        startTag attrs = angles $ pretty name <+> prettyAttributes attrs
        endTag = pretty $ "</" ++ name ++ ">"

prettyContent :: [XML] -> Doc ann
prettyContent xs = sep $ map prettyXML xs

prettyAttributes :: [Attribute] -> Doc ann
prettyAttributes as = align $ sep $ map prettyAttribute as

prettyAttribute :: Attribute -> Doc ann
prettyAttribute (A name value) = pretty name <> equals <> dquotes (pretty value)
