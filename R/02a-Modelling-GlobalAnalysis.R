rm(list = ls())
#Load Data
source("R/00-DataPreparation.R")
library(ggplot2)
library(ggthemes)
library(stringr)
library(texreg)
soep <- LoadSoep(sampleSelection = c(7,9,11,12,14,16,17), MinYear = 1991, MaxYear = 2014, AgeGroups = c(0,30,50,Inf))
savePlots <- F
saveTables <- T
#pgerwzt
setnames(soep, old = "pgerwzt", new = "Tenure")
soep[, YearFactor := as.factor(syear)]
#Filter missing variables
soep <- soep[OldExp >= 0 & NewExp >= 0 & Tenure >= 0 & OldEdu >= 0 & NewEdu >= 0]
soep[, TotalExp := NewExp + OldExp]
soep[, TotalEdu := NewEdu + OldEdu]
soep[, TotalExpSquared := TotalExp^2]
soep[, NewExpSquared := NewExp^2]
soep[, OldExpSquared := OldExp^2]
#Filter out people without wage
soep <- soep[realGrossWage > 0]

soep[realGrossWage > 0, logGrossWage :=  log(realGrossWage)]

# Models by year group
years <- unique(as.numeric(soep$syear))
YearsInGroup <- 4
# #Non Overlapping Year Groups
YearGroups <- split(years, ceiling(seq_along(years)/YearsInGroup))

# #Overlapping Year Groups
# YearGroups <- list()
# for(year in years){
#       NewYears <- ((year - ceiling((YearsInGroup - 1)/2)):(year + ceiling((YearsInGroup - 1)/2)))
#       NewYears <- intersect(NewYears, years)
#       if(length(NewYears) == YearsInGroup)
#       YearGroups <- c(YearGroups, list(NewYears))
# }

FullTimeDataEast <- soep[as.numeric(sampreg) == 8,]
FullTimeDataWest <- soep[as.numeric(sampreg) == 7,]



EastModelsList <- list()
WestModelsList <- list()
Formula <- logGrossWage ~ TotalEdu + TotalExp + TotalExpSquared + Tenure + sex + YearFactor
rm(CoefficientsTotalExp)
for(yearGroup in YearGroups){
      EastModel <- lm(formula = Formula, data = FullTimeDataEast[syear %in% yearGroup,])
      WestModel <- lm(formula = Formula, data = FullTimeDataWest[syear %in% yearGroup,])
      EastCoefficients <- as.data.frame(cbind(t(c(Region = "East", YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(EastModel$coefficients)))
      WestCoefficients <- as.data.frame(cbind(t(c(Region = "West", YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(WestModel$coefficients)))
      EastCoefficients <- EastCoefficients[,-grep(names(EastCoefficients), pattern = "YearFactor")]
      WestCoefficients <- WestCoefficients[,-grep(names(WestCoefficients), pattern = "YearFactor")]
      if(!exists("CoefficientsTotalExp")){
            CoefficientsTotalExp <- rbind(EastCoefficients, WestCoefficients)
      }
      else{
            CoefficientsTotalExp <- rbind(CoefficientsTotalExp, EastCoefficients, WestCoefficients)
      }
      EastModelsList <- c(EastModelsList,list(EastModel))
      WestModelsList <- c(WestModelsList,list(WestModel))
}
CoefficientsTotalExp$Region <- as.factor(CoefficientsTotalExp$Region)
CoefficientsTotalExp$YearGroup <- as.ordered(CoefficientsTotalExp$YearGroup)


#Use Coefficients to get Returns 0-1, 0-2, ... years of Experience (Old/New)
ExpDiffs <- 1:10
for(expDiff in ExpDiffs){
      TotalExpName <- paste0("TotalExpDiff0to",expDiff)
      CoefficientsTotalExp[,TotalExpName] <- expDiff*as.numeric(as.character(CoefficientsTotalExp$TotalExp)) + expDiff^2*as.numeric(as.character(CoefficientsTotalExp$`TotalExpSquared`))
}
DiffDataTotalExp <- CoefficientsTotalExp[,c(1,2,grep(names(CoefficientsTotalExp), pattern = "0to"))]
CoefficientsTotalExp <- as.data.table(CoefficientsTotalExp)

DiffPlotDataTotalExp <- melt(as.data.table(DiffDataTotalExp), id.vars = c("Region", "YearGroup"))
DiffPlotDataTotalExp$UpperDiff <- str_split(DiffPlotDataTotalExp$variable, "[^0-9]+", simplify = TRUE)[,3]
DiffPlotDataTotalExp$Type <- str_sub(DiffPlotDataTotalExp$variable, end = 6)

plotDiffTotalExpSquareYearFactor <- ggplot(DiffPlotDataTotalExp[as.numeric(UpperDiff) <= 7 & Type == "TotalE",], aes(x = as.numeric(YearGroup) + 1991,y = value, col=UpperDiff)) + geom_line(aes(lty = Region)) +
      labs(x = "Year", y = "Wage Differentials across Experience") + scale_color_stata(name = "Experience Difference in Years", labels = paste0("0 - ", ExpDiffs)) #+ scale_linetype_stata(name = "Sample Region", labels = c("East Germany", "West Germany")) + theme_tufte() + geom_rangeframe()

#Analysis for NEw and Old Exp in one Model
Formula <- logGrossWage ~ OldEdu + NewEdu + OldExp + OldExpSquared + NewExp + NewExpSquared + Tenure + sex + YearFactor
rm(Coefficients)
for(yearGroup in YearGroups){
      EastModel <- lm(formula = Formula, data = FullTimeDataEast[syear %in% yearGroup,])
      WestModel <- lm(formula = Formula, data = FullTimeDataWest[syear %in% yearGroup,])
      EastCoefficients <- as.data.frame(cbind(t(c(Region = "East", YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(EastModel$coefficients)))
      WestCoefficients <- as.data.frame(cbind(t(c(Region = "West", YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(WestModel$coefficients)))
      EastCoefficients <- EastCoefficients[,-grep(names(EastCoefficients), pattern = "YearFactor")]
      WestCoefficients <- WestCoefficients[,-grep(names(WestCoefficients), pattern = "YearFactor")]
      if(!exists("Coefficients")){
            Coefficients <- rbind(EastCoefficients, WestCoefficients)
      }
      else{
            Coefficients <- rbind(Coefficients, EastCoefficients, WestCoefficients)
      }
      EastModelsList <- c(EastModelsList,list(EastModel))
      WestModelsList <- c(WestModelsList,list(WestModel))
}
Coefficients$Region <- as.factor(Coefficients$Region)
Coefficients$YearGroup <- as.ordered(Coefficients$YearGroup)


#Use Coefficients to get Returns 0-1, 0-2, ... years of Experience (Old/New)
ExpDiffs <- 1:10
for(expDiff in ExpDiffs){
      oldExpName <- paste0("OldExpDiff0to",expDiff)
      Coefficients[,oldExpName] <- expDiff*as.numeric(as.character(Coefficients$OldExp)) + expDiff^2*as.numeric(as.character(Coefficients$`OldExpSquared`))
      newExpName <- paste0("NewExpDiff0to",expDiff)
      Coefficients[,newExpName] <- expDiff*as.numeric(as.character(Coefficients$NewExp)) + expDiff^2*as.numeric(as.character(Coefficients$`NewExpSquared`))
}
DiffDataByAgeGroup <- Coefficients[,c(1,2,grep(names(Coefficients), pattern = "0to"))]
Coefficients <- as.data.table(Coefficients)

DiffPlotData <- melt(as.data.table(DiffDataByAgeGroup), id.vars = c("Region", "YearGroup"))
DiffPlotData$UpperDiff <- str_split(DiffPlotData$variable, "[^0-9]+", simplify = TRUE)[,3]
DiffPlotData$Type <- str_sub(DiffPlotData$variable, end = 6)

plotDiffOldExpSquareYearFactor <- ggplot(DiffPlotData[as.numeric(UpperDiff) <= 7 & Type == "OldExp",], aes(x = as.numeric(YearGroup) + 1991,y = value, col=UpperDiff)) + geom_line(aes(lty = Region)) +
      labs(x = "Year", y = "Wage Differentials across Experience") + scale_color_stata(name = "Experience Difference in Years", labels = paste0("0 - ", ExpDiffs)) #+ scale_linetype_stata(name = "Sample Region", labels = c("East Germany", "West Germany")) + theme_tufte() + geom_rangeframe()

plotDiffNewExpSquareYearFactor <- ggplot(DiffPlotData[as.numeric(UpperDiff) <= 7 & Type == "NewExp",], aes(x = as.numeric(YearGroup) + 1991,y = value, col=UpperDiff)) + geom_line(aes(lty = Region)) +
      labs(x = "Year", y = "Wage Differentials across Experience") + scale_color_stata(name = "Experience Difference in Years", labels = paste0("0 - ", ExpDiffs)) #+ scale_linetype_stata(name = "Sample Region", labels = c("East Germany", "West Germany")) + theme_tufte() + geom_rangeframe()

DiffPlotData <- rbind(DiffPlotData, DiffPlotDataTotalExp)
DiffPlotData[, Type := factor(Type, levels = c("NewExp","OldExp","TotalE"))] 
DiffPlotData[, Region := factor(Region, levels = c("West", "East"))]
plotDiffComparisonExp <- ggplot(DiffPlotData[as.numeric(UpperDiff) == 5], aes(x = as.numeric(YearGroup),y = value,  col=Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log wage differential 0-5 Years of Experience") + scale_color_stata(name = "Type", labels = c("New Experience", "Old Experience", "Total Experience")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

DiffPlotDataEdu <- merge(CoefficientsTotalExp[,.(Region, YearGroup, TotalEdu)], Coefficients[,.(Region, YearGroup, NewEdu, OldEdu)], by = c("Region","YearGroup"))
DiffPlotDataEdu <- melt.data.table(DiffPlotDataEdu, id.vars = c("Region","YearGroup"))
setnames(DiffPlotDataEdu, old = c("variable"), new = c("Type"))
DiffPlotDataEdu[,value := as.numeric(as.character(value))]
DiffPlotDataEdu[, Type := factor(Type, levels = c("NewEdu","OldEdu","TotalEdu"))] 
DiffPlotDataEdu[, Region := factor(Region, levels = c("West","East"))]

plotDiffComparisonEdu <- ggplot(DiffPlotDataEdu, aes(x = as.numeric(YearGroup),y = value*5,  col=Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log wage differential 0-5 Years of Education") + scale_color_stata(name = "Type", labels = c("New Education", "Old Education", "Total Education")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Analyse Human Capital Shares of education and experience by using mean experience and mean education values
#Create YearGroup variable
soep[,YearGroup := cut(syear, c(min(YearGroups[[1]])-1,sapply(YearGroups, max)))]
levels(soep$YearGroup) <- levels(DiffPlotData$YearGroup)

#Get Mean Wages
MeanWages <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg)]
MeanWages[, Region := droplevels(sampreg)]
MeanWages[, sampreg := NULL]
levels(MeanWages$Region) <- c("West", "East")
MeanWages[, Region := factor(Region, levels = c("West","East"))]

plotMeanWages <- ggplot(MeanWages, aes(x = as.numeric(YearGroup),y = MeanGrossWage)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by Old Experience Group
OldExpCuts <- c(0,5,10,Inf)
MeanWagesByOldExp <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, OldExpGroup = cut(OldExp, breaks = OldExpCuts))]
MeanWagesByOldExp <- MeanWagesByOldExp[!is.na(OldExpGroup),]
levels(MeanWagesByOldExp$OldExpGroup) <- c("<=5", "5 - 10", ">10")
MeanWagesByOldExp[, Region := droplevels(sampreg)]
MeanWagesByOldExp[, sampreg := NULL]
levels(MeanWagesByOldExp$Region) <- c("West", "East")
MeanWagesByOldExp[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByOldExp <- ggplot(MeanWagesByOldExp, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = OldExpGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of Old Experience") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by New Experience Group
NewExpCuts <- c(0,5,10,Inf)
MeanWagesByNewExp <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, NewExpGroup = cut(NewExp, breaks = NewExpCuts))]
MeanWagesByNewExp <- MeanWagesByNewExp[!is.na(NewExpGroup),]
levels(MeanWagesByNewExp$NewExpGroup) <- c("<=5", "5 - 10", ">10")
MeanWagesByNewExp[, Region := droplevels(sampreg)]
MeanWagesByNewExp[, sampreg := NULL]
levels(MeanWagesByNewExp$Region) <- c("West", "East")
MeanWagesByNewExp[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByNewExp <- ggplot(MeanWagesByNewExp, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = NewExpGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of New Experience") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by Total Experience Group
TotalExpCuts <- c(0,5,10,Inf)
MeanWagesByTotalExp <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, TotalExpGroup = cut(TotalExp, breaks = TotalExpCuts))]
MeanWagesByTotalExp <- MeanWagesByTotalExp[!is.na(TotalExpGroup),]
levels(MeanWagesByTotalExp$TotalExpGroup) <- c("<=5", "5 - 10", ">10")
MeanWagesByTotalExp[, Region := droplevels(sampreg)]
MeanWagesByTotalExp[, sampreg := NULL]
levels(MeanWagesByTotalExp$Region) <- c("West", "East")
MeanWagesByTotalExp[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByTotalExp <- ggplot(MeanWagesByTotalExp, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = TotalExpGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of Total Experience") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by Old Education Group
OldEduCuts <- c(0,10,15,Inf)
MeanWagesByOldEdu <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, OldEduGroup = cut(OldEdu, breaks = OldEduCuts))]
MeanWagesByOldEdu <- MeanWagesByOldEdu[!is.na(OldEduGroup),]
levels(MeanWagesByOldEdu$OldEduGroup) <- c("<=10", "10 - 15", ">15")
MeanWagesByOldEdu[, Region := droplevels(sampreg)]
MeanWagesByOldEdu[, sampreg := NULL]
levels(MeanWagesByOldEdu$Region) <- c("West", "East")
MeanWagesByOldEdu[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByOldEdu <- ggplot(MeanWagesByOldEdu, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = OldEduGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of Old Education") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by New Education Group
NewEduCuts <- c(0,10,15,Inf)
MeanWagesByNewEdu <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, NewEduGroup = cut(NewEdu, breaks = NewEduCuts))]
MeanWagesByNewEdu <- MeanWagesByNewEdu[!is.na(NewEduGroup),]
levels(MeanWagesByNewEdu$NewEduGroup) <- c("<=10", "10 - 15", ">15")
MeanWagesByNewEdu[, Region := droplevels(sampreg)]
MeanWagesByNewEdu[, sampreg := NULL]
levels(MeanWagesByNewEdu$Region) <- c("West", "East")
MeanWagesByNewEdu[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByNewEdu <- ggplot(MeanWagesByNewEdu, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = NewEduGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of New Education") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Get Mean Wage by Total Education Group
TotalEduCuts <- c(0,10,15,Inf)
MeanWagesByTotalEdu <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup, sampreg, TotalEduGroup = cut(TotalEdu, breaks = TotalEduCuts))]
MeanWagesByTotalEdu <- MeanWagesByTotalEdu[!is.na(TotalEduGroup),]
levels(MeanWagesByTotalEdu$TotalEduGroup) <- c("<=10", "10 - 15", ">15")
MeanWagesByTotalEdu[, Region := droplevels(sampreg)]
MeanWagesByTotalEdu[, sampreg := NULL]
levels(MeanWagesByTotalEdu$Region) <- c("West", "East")
MeanWagesByTotalEdu[, Region := factor(Region, levels = c("West","East"))]

plotMeanWagesByTotalEdu <- ggplot(MeanWagesByTotalEdu, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = TotalEduGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Years of Total Education") + 
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()


#Analyse Mean Old and New Experience over time
MeanExp <- soep[,.(MeanOldExp = mean(OldExp, na.rm = TRUE), MeanNewExp = mean(NewExp, na.rm = TRUE), MeanTotalExp = mean(TotalExp, na.rm = TRUE)), by = .(YearGroup, sampreg)]
MeanExp[, Region := droplevels(sampreg)]
MeanExp[, sampreg := NULL]
levels(MeanExp$Region) <- c("West", "East")
MeanExp[, Region := factor(Region, levels = c("West","East"))]

MeanExpPlotData <- melt(MeanExp, id.vars = c("YearGroup","Region"))
MeanExpPlotData[, Type := factor(variable, levels = c("MeanNewExp","MeanOldExp","MeanTotalExp"))]
MeanExpPlotData[, variable := NULL]

plotMeanExp <- ggplot(MeanExpPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Experience (Years)")  + scale_color_stata(name = "Type", labels = c("New Experience", "Old Experience", "Total Experience")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()


#Analyse Mean Old and New Experience over time
MeanEdu <- soep[,.(MeanOldEdu = mean(OldEdu, na.rm = TRUE), MeanNewEdu = mean(NewEdu, na.rm = TRUE), MeanTotalEdu = mean(TotalEdu, na.rm = TRUE)), by = .(YearGroup, sampreg)]
MeanEdu[, Region := droplevels(sampreg)]
MeanEdu[, sampreg := NULL]
levels(MeanEdu$Region) <- c("West","East")
MeanEdu[, Region := factor(Region, levels = c("West","East"))]

MeanEduPlotData <- melt(MeanEdu, id.vars = c("YearGroup","Region"))
MeanEduPlotData[, Type := factor(variable, levels = c("MeanNewEdu","MeanOldEdu","MeanTotalEdu"))]
MeanEduPlotData[, variable := NULL]

plotMeanEdu <- ggplot(MeanEduPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Education (Years)")  + scale_color_stata(name = "Type", labels = c("New Education", "Old Education", "Total Education")) + scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()

HumanCapitalData <- merge(MeanExp, MeanEdu, by = c("YearGroup", "Region"))
setnames(Coefficients, old = c("OldExpSquared","NewExpSquared"), new = c("OldExpSquared","NewExpSquared"))
HumanCapitalData <- merge(HumanCapitalData, Coefficients[,.(YearGroup, Region, OldEdu, NewEdu, OldExp, OldExpSquared, NewExp, NewExpSquared)], by = c("YearGroup", "Region"))

setnames(CoefficientsTotalExp, old = c("TotalExpSquared"), new = c("TotalExpSquared"))
HumanCapitalData <- merge(HumanCapitalData, CoefficientsTotalExp[,.(YearGroup, Region, TotalEdu, TotalExp, TotalExpSquared)], by = c("YearGroup", "Region"))

HumanCapitalData[, HCTotalEdu := as.numeric(as.character(TotalEdu))*MeanTotalEdu]
HumanCapitalData[, HCNewEdu := as.numeric(as.character(NewEdu))*MeanNewEdu]
HumanCapitalData[, HCOldEdu := as.numeric(as.character(OldEdu))*MeanOldEdu]

HumanCapitalData[, HCTotalExp := as.numeric(as.character(TotalExp))*MeanTotalExp + as.numeric(as.character(TotalExpSquared))*MeanTotalExp^2]
HumanCapitalData[, HCNewExp := as.numeric(as.character(NewExp))*MeanNewExp + as.numeric(as.character(NewExpSquared))*MeanNewExp^2]
HumanCapitalData[, HCOldExp := as.numeric(as.character(OldExp))*MeanOldExp + as.numeric(as.character(OldExpSquared))*MeanOldExp^2]

HumanCapitalData <- HumanCapitalData[,.(YearGroup, Region, HCTotalEdu, HCNewEdu, HCOldEdu, HCTotalExp, HCNewExp, HCOldExp)]
HumanCapitalPlotData <- melt.data.table(HumanCapitalData, id.vars = c("YearGroup","Region"))
setnames(HumanCapitalPlotData, old = "variable", new = "Type")
HumanCapitalPlotData[, Type := as.factor(Type)]
levels(HumanCapitalPlotData$Type) <- c("Total Education", "New Education", "Old Education", "Total Experience", "New Experience", "Old Experience")
HumanCapitalPlotData[, Type := factor(Type , levels = c("New Education", "Old Education", "Total Education","New Experience", "Old Experience", "Total Experience"))]

plotHumanCapital <- ggplot(HumanCapitalPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

EduSelection <- c("Total Education", "New Education", "Old Education")
plotHumanCapitalEdu <- ggplot(HumanCapitalPlotData[Type %in% EduSelection,], aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

ExpSelection <- c("Total Experience", "New Experience", "Old Experience")
plotHumanCapitalExp <- ggplot(HumanCapitalPlotData[Type %in% ExpSelection,], aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()


if(savePlots){
      plots <- ls()[grepl(ls(), pattern = "plot")]
      for(plot in plots){
            file <- paste0(plot, ".png")
            ggsave(filename = file, path = "./Graphics", plot = get(plot), device = "png", width = 18, height = 12, units = "cm")
      }
}

if(saveTables){
      #Create Tables for Model-Overviews
      #West German Models
      Filename = "Tex/Tables/WestModelsTotal.tex"
      texreg(l = WestModelsList[1:6],custom.model.names = levels(soep$YearGroup), fontsize = "small", file = Filename, digits = 4, omit.coef = "YearFactor", caption = "Model Coefficients for West German Data using Model 1", label = "table:WestModelsTotal")
      Filename = "Tex/Tables/WestModelsNewOld.tex"
      texreg(l = WestModelsList[7:12],custom.model.names = levels(soep$YearGroup), fontsize = "small", file = Filename, digits = 4,omit.coef = "YearFactor", caption = "Model Coefficients for West German Data using Model 2", label = "table:WestModelsOldNew")
      #East German Models
      Filename = "Tex/Tables/EastModelsTotal.tex"
      texreg(l = EastModelsList[1:6],custom.model.names = levels(soep$YearGroup), fontsize = "small", file = Filename, digits = 4, omit.coef = "YearFactor", caption = "Model Coefficients for East German Data using Model 1", label = "table:EastModelsTotal")
      Filename = "Tex/Tables/EastModelsNewOld.tex"
      texreg(l = EastModelsList[7:12],custom.model.names = levels(soep$YearGroup), fontsize = "small", file = Filename, digits = 4,omit.coef = "YearFactor", caption = "Model Coefficients for East German Data using Model 2", label = "table:EastModelsOldNew")
}
