import           Criterion.Main
import qualified Problems.P01Bench as P01
import qualified Problems.P02Bench as P02
import qualified Problems.P03Bench as P03
import qualified Problems.P04Bench as P04
import qualified Problems.P05Bench as P05
import qualified Problems.P06Bench as P06
import qualified Problems.P07Bench as P07
import qualified Problems.P08Bench as P08
import qualified Problems.P09Bench as P09
import qualified Problems.P10Bench as P10
import qualified Problems.P11Bench as P11

main :: IO()
main = defaultMain [ P01.group
                   , P02.group
                   , P03.group
                   , P04.group
                   , P05.group
                   , P06.group
                   , P07.group
                   , P08.group
                   , P09.group
                   , P10.group
                   , P11.group
                   ]
