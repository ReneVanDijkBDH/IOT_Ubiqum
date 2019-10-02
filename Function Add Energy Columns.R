AddEnergyColumns <- function(EnergyData) {
  # this function adds energy-related columns to dataframe
  EnergyData$Active_whm   <- EnergyData$GAP * 1000 /60
  EnergyData$Reactive_whm <- EnergyData$GAP * 1000 /60
  EnergyData$Other_whm    <- with(EnergyData, Active_whm - Sub1 - Sub2 - Sub3)
  return(EnergyData)
}