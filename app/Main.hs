module Main where

import           Cube
import           Data.Foldable                  ( forM_ )
import qualified Data.Map.Lazy                 as M
import           Solver

cubes =
    [ Cube
        "A"
        (M.fromList
            [ (One  , Green)
            , (Two  , Ochre)
            , (Three, Green)
            , (Four , Purple)
            , (Five , Blue)
            , (Six  , Ochre)
            ]
        )
    , Cube
        "B"
        (M.fromList
            [ (One  , Green)
            , (Two  , Purple)
            , (Three, Ochre)
            , (Four , Blue)
            , (Five , Purple)
            , (Six  , Purple)
            ]
        )
    , Cube
        "C"
        (M.fromList
            [ (One  , Ochre)
            , (Two  , Blue)
            , (Three, Green)
            , (Four , Green)
            , (Five , Purple)
            , (Six  , Blue)
            ]
        )
    , Cube
        "D"
        (M.fromList
            [ (One  , Ochre)
            , (Two  , Purple)
            , (Three, Ochre)
            , (Four , Purple)
            , (Five , Blue)
            , (Six  , Green)
            ]
        )
    ]

main :: IO ()
main = do
    forM_ (validCubeConfiguration cubes) displayConfiguration
    putStrLn $ "No. of valid configurations = " ++ show
        (countValidCubeConfigurations cubes)


displayPosition :: Position -> IO ()
displayPosition Position { cube = (Cube name _), frontFace = front, topFace = top }
    = putStrLn
        $  "Cube "
        ++ name
        ++ ": Front = "
        ++ show front
        ++ ", Top = "
        ++ show top

displayConfiguration :: [Position] -> IO ()
displayConfiguration conf = do
    putStrLn "One valid configuration:"
    foldMap displayPosition conf
    putStrLn ""
