module Types (
  Point2d( .. ),
  Triangle( .. ),
  Line2d( .. ),
) where

data Point2d = Point2d Double Double deriving (Eq, Ord, Show)
data Triangle = Triangle Point2d Point2d Point2d deriving (Eq, Show)
data Line2d = Line2d Point2d Point2d deriving (Show)