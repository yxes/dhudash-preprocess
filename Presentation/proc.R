lead=read.csv("~/Downloads/StateConfirmedByYear_1997_2013_03042015 (2).csv")
homicide_wide=read.csv("~/Downloads/homicide_wide.csv")
stateConversions=read.csv("StateConversions.csv")
x=shiftedCorr(lead, homicide_wide, "PctConfirmedBllGT10","Overall",0,stateConversions)

