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
import qualified Problems.P12Bench as P12
import qualified Problems.P13Bench as P13
import qualified Problems.P14Bench as P14
import qualified Problems.P15Bench as P15
import qualified Problems.P31Bench as P31
import qualified Problems.P32Bench as P32
import qualified Problems.P46Bench as P46

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
                   , P12.group
                   , P13.group
                   , P14.group
                   , P15.group
                   , P31.group
                   , P32.group
                   , P46.group
                   ]
