{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Main (main) where

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
import qualified Problems.P22Bench as P22
import qualified Problems.P23Bench as P23
import qualified Problems.P24Bench as P24
import qualified Problems.P25Bench as P25
import qualified Problems.P26Bench as P26
import qualified Problems.P27Bench as P27
import qualified Problems.P28Bench as P28
import qualified Problems.P29Bench as P29
import qualified Problems.P30Bench as P30
import qualified Problems.P31Bench as P31
import qualified Problems.P32Bench as P32
import qualified Problems.P33Bench as P33
import qualified Problems.P34Bench as P34
import qualified Problems.P35Bench as P35
import qualified Problems.P36Bench as P36
import qualified Problems.P37Bench as P37
import qualified Problems.P39Bench as P39
import qualified Problems.P40Bench as P40
import qualified Problems.P41Bench as P41
import qualified Problems.P46Bench as P46
import qualified Problems.P48Bench as P48
import qualified Problems.P49Bench as P49
import qualified Problems.P50Bench as P50
import qualified Problems.P55Bench as P55
import qualified Problems.P56Bench as P56
import qualified Problems.P57Bench as P57
import qualified Problems.P58Bench as P58
import qualified Problems.P59Bench as P59
import qualified Problems.P60Bench as P60
import qualified Problems.P61Bench as P61
import qualified Problems.P62Bench as P62
import qualified Problems.P63Bench as P63
import qualified Problems.P64Bench as P64
import qualified Problems.P65Bench as P65
import qualified Problems.P66Bench as P66
import qualified Problems.P67Bench as P67
import qualified Problems.P68Bench as P68
import qualified Problems.P69Bench as P69
import qualified Problems.P70Bench as P70
import qualified Problems.P71Bench as P71
import qualified Problems.P72Bench as P72
import qualified Problems.P73Bench as P73
import qualified Problems.P80Bench as P80
import qualified Problems.P81Bench as P81
import qualified Problems.P82Bench as P82
import qualified Problems.P83Bench as P83
import qualified Problems.P84Bench as P84
import qualified Problems.P85Bench as P85
import qualified Problems.P86Bench as P86
import qualified Problems.P87Bench as P87
import qualified Problems.P88Bench as P88
import qualified Problems.P89Bench as P89
import qualified Problems.P90Bench as P90
import qualified Problems.P91Bench as P91
import qualified Problems.P92Bench as P92
import qualified Problems.P93Bench as P93
import qualified Problems.P94Bench as P94
import qualified Problems.P95Bench as P95
import qualified Problems.P96Bench as P96
import qualified Problems.P97Bench as P97
import qualified Problems.P98Bench as P98
import qualified Problems.P99Bench as P99

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
                   , P22.group
                   , P23.group
                   , P24.group
                   , P25.group
                   , P26.group
                   , P27.group
                   , P28.group
                   , P29.group
                   , P30.group
                   , P31.group
                   , P32.group
                   , P33.group
                   , P34.group
                   , P35.group
                   , P36.group
                   , P37.group
                   , P39.group
                   , P40.group
                   , P41.group
                   , P46.group
                   , P48.group
                   , P49.group
                   , P50.group
                   -- P54 benchmark intentionally omitted
                   , P55.group
                   , P56.group
                   , P57.group
                   , P58.group
                   , P59.group
                   , P60.group
                   , P61.group
                   , P62.group
                   , P63.group
                   , P64.group
                   , P65.group
                   , P66.group
                   , P67.group
                   , P68.group
                   , P69.group
                   , P70.group
                   , P71.group
                   , P72.group
                   , P73.group
                   , P80.group
                   , P81.group
                   , P82.group
                   , P83.group
                   , P84.group
                   , P85.group
                   , P86.group
                   , P87.group
                   , P88.group
                   , P89.group
                   , P90.group
                   , P91.group
                   , P92.group
                   , P93.group
                   , P94.group
                   , P95.group
                   , P96.group
                   , P97.group
                   , P98.group
                   , P99.group
                   ]
