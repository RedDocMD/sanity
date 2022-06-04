module Cube
    ( Colour(..)
    , Face(..)
    , Cube(..)
    , faceColour
    , oppositeFace
    ) where


import qualified Data.Map.Lazy                 as M


data Colour = Purple | Green | Blue | Ochre deriving (Eq, Ord, Show)

data Face = One | Two | Three | Four | Five | Six deriving (Enum, Eq, Ord, Show)

data Cube = Cube String (M.Map Face Colour)
    deriving Show


faceColour :: Cube -> Face -> Colour
faceColour (Cube _ faces) face = faces M.! face

oppositeFace :: Face -> Face
oppositeFace One   = Six
oppositeFace Two   = Five
oppositeFace Three = Four
oppositeFace Four  = Three
oppositeFace Five  = Two
oppositeFace Six   = One
