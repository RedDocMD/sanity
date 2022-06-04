module Solver
    ( Position(..)
    , validCubeConfiguration
    , countValidCubeConfigurations
    ) where


import           Cube
import           Data.List                      ( find
                                                , permutations
                                                , sort
                                                )


data Position = Position
    { cube      :: Cube
    , frontFace :: Face
    , topFace   :: Face
    }
    deriving Show


frontRow :: [Position] -> [Colour]
frontRow = map frontColour
    where frontColour pos = faceColour (cube pos) (frontFace pos)

topRow :: [Position] -> [Colour]
topRow = map topColour
    where topColour pos = faceColour (cube pos) (topFace pos)

bottomRow :: [Position] -> [Colour]
bottomRow = map bottomColour
  where
    bottomColour pos = faceColour (cube pos) (oppositeFace . topFace $ pos)

backRow :: [Position] -> [Colour]
backRow = map backColour
  where
    backColour pos = faceColour (cube pos) (oppositeFace . frontFace $ pos)


isValidRow :: [Colour] -> Bool
isValidRow colours = sort colours == [Purple, Green, Blue, Ochre]

isValidPositions :: [Position] -> Bool
isValidPositions positions = all
    isValidRow
    ([frontRow, backRow, bottomRow, topRow] <*> replicate 4 positions)


cubePositions :: Cube -> [Position]
cubePositions cube = map (uncurry (Position cube)) $ filter
    validCombination
    [ (f, t) | f <- [One .. Six], t <- [One .. Six] ]
    where validCombination (f, t) = f /= t && f /= oppositeFace t


cubeConfigurations :: [Cube] -> [[Position]]
cubeConfigurations = mapM cubePositions


allCubeConfigurations :: [Cube] -> [[Position]]
allCubeConfigurations = concatMap cubeConfigurations . permutations


validCubeConfiguration :: [Cube] -> Maybe [Position]
validCubeConfiguration = find isValidPositions . allCubeConfigurations


countValidCubeConfigurations :: [Cube] -> Int
countValidCubeConfigurations =
    length . filter isValidPositions . allCubeConfigurations
