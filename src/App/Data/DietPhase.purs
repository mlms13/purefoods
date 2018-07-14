module App.Data.DietPhase where

import Prelude (class Eq, class Show)

data DietPhase
  = One
  | Two
  | Three

newtype Tolerance = Tolerance
  { reflux :: Int
  , irritant :: Int
  , bacteria :: Int
  }

tolerance :: DietPhase -> Tolerance
tolerance One = Tolerance
  { reflux: 3, irritant: 3, bacteria: 4 }
tolerance Two = Tolerance
  { reflux: 5, irritant: 5, bacteria: 5 }
tolerance Three = Tolerance
  { reflux: 7, irritant: 7, bacteria: 7 }

derive instance eqDietPhase :: Eq DietPhase

instance showDietPhase :: Show DietPhase where
  show One = "1"
  show Two = "2"
  show Three = "3"
