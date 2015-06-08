module Freq where

freq :: Double -> Double
freq n = 2 ** ((n - 49) / 12) * 440

-- generate frequencies around a freq number
cents d n = [floor $ freq (n-d/100)..floor $ freq (n+d/100)]
