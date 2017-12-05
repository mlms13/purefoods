module App.Data.DietPhase where

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
  { reflux: 3, irritant: 3, bacteria: 3 }
tolerance Two = Tolerance
  { reflux: 5, irritant: 5, bacteria: 5 }
tolerance Three = Tolerance
  { reflux: 7, irritant: 7, bacteria: 7 }
