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
import qualified Problems.P16Bench as P16
import qualified Problems.P17Bench as P17
import qualified Problems.P18Bench as P18
import qualified Problems.P19Bench as P19
import qualified Problems.P20Bench as P20
import qualified Problems.P21Bench as P21
import qualified Problems.P31Bench as P31
import qualified Problems.P32Bench as P32
import qualified Problems.P33Bench as P33
import qualified Problems.P34Bench as P34
import qualified Problems.P35Bench as P35
import qualified Problems.P36Bench as P36
import qualified Problems.P37Bench as P37
import qualified Problems.P39Bench as P39
import qualified Problems.P40Bench as P40
import qualified Problems.P46Bench as P46
import qualified Problems.P48Bench as P48
import qualified Problems.P49Bench as P49
import qualified Problems.P55Bench as P55
import qualified Problems.P56Bench as P56
import qualified Problems.P57Bench as P57
import qualified Problems.P58Bench as P58
import qualified Problems.P59Bench as P59
import qualified Problems.P60Bench as P60
import qualified Problems.P61Bench as P61
import qualified Problems.P62Bench as P62
import qualified Problems.P70Bench as P70
import qualified Problems.P71Bench as P71
import qualified Problems.P80Bench as P80
import qualified Problems.P81Bench as P81
import qualified Problems.P82Bench as P82
import qualified Problems.P83Bench as P83
import qualified Problems.P84Bench as P84
import qualified Problems.P85Bench as P85
import qualified Problems.P86Bench as P86
import qualified Problems.P90Bench as P90
import qualified Problems.P91Bench as P91
import qualified Problems.P95Bench as P95

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
                   , P16.group
                   , P17.group
                   , P18.group
                   , P19.group
                   , P20.group
                   , P21.group
                   , P31.group
                   , P32.group
                   , P33.group
                   , P34.group
                   , P35.group
                   , P36.group
                   , P37.group
                   , P39.group
                   , P40.group
                   , P46.group
                   , P48.group
                   , P49.group
                   -- P54 benchmark intentionally omitted
                   , P55.group
                   , P56.group
                   , P57.group
                   , P58.group
                   , P59.group
                   , P60.group
                   , P61.group
                   , P62.group
                   , P70.group
                   , P71.group
                   , P80.group
                   , P81.group
                   , P82.group
                   , P83.group
                   , P84.group
                   , P85.group
                   , P86.group
                   , P90.group
                   , P91.group
                   , P95.group
                   ]
