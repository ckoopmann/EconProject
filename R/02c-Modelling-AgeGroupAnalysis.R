rm(list = ls())
#Load Data
source("R/00-DataPreparation.R")
library(ggplot2)
library(ggthemes)
library(stringr)
soep <- LoadSoep(sampleSelection = c(7,9,11,12,14,16,17), MinYear = 1991, MaxYear = 2014, AgeGroups = c(0,40,Inf))
savePlots <- T
levels(soep$AgeGroup) <- c("Age <= 40", "Age > 40")
#pgerwzt
setnames(soep, old = "pgerwzt", new = "Tenure")
soep[, YearFactor := as.factor(syear)]
#Filter missing variables
soep <- soep[OldExp >= 0 & NewExp >= 0 & Tenure >= 0 & OldEdu >= 0 & NewEdu >= 0]
soep <- soep[, TotalExp := NewExp + OldExp]
soep <- soep[, TotalEdu := NewEdu + OldEdu]
#Filter out people without wage
soep <- soep[realGrossWage > 0]

soep[realGrossWage > 0, logGrossWage :=  log(realGrossWage)]

# Models by year group
years <- unique(as.numeric(soep$syear))
YearsInGroup <- 4
# #Non Overlapping Year Groups
YearGroups <- split(years, ceiling(seq_along(years)/YearsInGroup))

FullTimeDataEast <- soep[as.numeric(sampreg) == 8,]
FullTimeDataWest <- soep[as.numeric(sampreg) == 7,]



EastModelsList <- list()
WestModelsList <- list()
Formula <- logGrossWage ~ TotalEdu + TotalExp + I(TotalExp^2) + Tenure + sex + YearFactor
rm(CoefficientsTotalExp)
AgeGroups <- levels(soep$AgeGroup)
for(ageGroup in AgeGroups){
      for(yearGroup in YearGroups){
            EastModel <- lm(formula = Formula, data = FullTimeDataEast[syear %in% yearGroup & AgeGroup == ageGroup,])
            WestModel <- lm(formula = Formula, data = FullTimeDataWest[syear %in% yearGroup & AgeGroup == ageGroup,])
            EastCoefficients <- as.data.frame(cbind(t(c(Region = "East", AgeGroup = ageGroup, YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(EastModel$coefficients)))
            WestCoefficients <- as.data.frame(cbind(t(c(Region = "West", AgeGroup = ageGroup, YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(WestModel$coefficients)))
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
}

CoefficientsTotalExp$Region <- as.factor(CoefficientsTotalExp$Region)
CoefficientsTotalExp$YearGroup <- as.ordered(CoefficientsTotalExp$YearGroup)


#Use Coefficients to get Returns 0-1, 0-2, ... years of Experience (Old/New)
expDiff <- 5
TotalExpName <- paste0("TotalExpDiff0to",expDiff)
CoefficientsTotalExp[,TotalExpName] <- expDiff*as.numeric(as.character(CoefficientsTotalExp$TotalExp)) + expDiff^2*as.numeric(as.character(CoefficientsTotalExp$`I(TotalExp^2)`))

DiffDataTotalExp <- CoefficientsTotalExp[,c(1,2,3, grep(names(CoefficientsTotalExp), pattern = "0to"))]
CoefficientsTotalExp <- as.data.table(CoefficientsTotalExp)

DiffPlotDataTotalExp <- melt(as.data.table(DiffDataTotalExp), id.vars = c("Region","AgeGroup","YearGroup"))
DiffPlotDataTotalExp$UpperDiff <- str_split(DiffPlotDataTotalExp$variable, "[^0-9]+", simplify = TRUE)[,3]
DiffPlotDataTotalExp$Type <- str_sub(DiffPlotDataTotalExp$variable, end = 6)

plotDiffTotalByYearGroup <- ggplot(DiffPlotDataTotalExp[as.numeric(UpperDiff) <= 7 & Type == "TotalE",], aes(x = as.numeric(YearGroup) + 1991,y = value, col=AgeGroup)) + geom_line(aes(lty = Region)) +
      labs(x = "Year", y = "Wage Differentials across Experience") + scale_color_stata(name = "Age Group") #+ scale_linetype_stata(name = "Sample Region", labels = c("East Germany", "West Germany")) + theme_tufte() + geom_rangeframe()

#Analysis for NEw and Old Exp in one Model
Formula <- logGrossWage ~ OldEdu + NewEdu + OldExp + I(OldExp^2) + NewExp + I(NewExp^2) + Tenure + sex + YearFactor
rm(Coefficients)
for(ageGroup in AgeGroups){
      for(yearGroup in YearGroups){
            EastModel <- lm(formula = Formula, data = FullTimeDataEast[syear %in% yearGroup & AgeGroup == ageGroup,])
            WestModel <- lm(formula = Formula, data = FullTimeDataWest[syear %in% yearGroup & AgeGroup == ageGroup,])
            EastCoefficients <- as.data.frame(cbind(t(c(Region = "East", AgeGroup = ageGroup, YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(EastModel$coefficients)))
            WestCoefficients <- as.data.frame(cbind(t(c(Region = "West", AgeGroup = ageGroup, YearGroup = paste(min(yearGroup), max(yearGroup), sep = " - "))),t(WestModel$coefficients)))
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
}
Coefficients$Region <- as.factor(Coefficients$Region)
Coefficients$YearGroup <- as.ordered(Coefficients$YearGroup)


#Use Coefficients to get Returns 0-1, 0-2, ... years of Experience (Old/New)
expDiff <- 5

oldExpName <- paste0("OldExpDiff0to",expDiff)
Coefficients[,oldExpName] <- expDiff*as.numeric(as.character(Coefficients$OldExp)) + expDiff^2*as.numeric(as.character(Coefficients$`I(OldExp^2)`))
newExpName <- paste0("NewExpDiff0to",expDiff)
Coefficients[,newExpName] <- expDiff*as.numeric(as.character(Coefficients$NewExp)) + expDiff^2*as.numeric(as.character(Coefficients$`I(NewExp^2)`))

DiffDataByAgeGroup <- Coefficients[,c(1,2,3,grep(names(Coefficients), pattern = "0to"))]
Coefficients <- as.data.table(Coefficients)

DiffPlotData <- melt(as.data.table(DiffDataByAgeGroup), id.vars = c("Region", "YearGroup","AgeGroup"))
DiffPlotData$UpperDiff <- str_split(DiffPlotData$variable, "[^0-9]+", simplify = TRUE)[,3]
DiffPlotData$Type <- str_sub(DiffPlotData$variable, end = 6)

DiffPlotData <- rbind(DiffPlotData, DiffPlotDataTotalExp)
DiffPlotData[, Type := factor(Type, levels = c("NewExp","OldExp","TotalE"))] 
DiffPlotData[, Region := factor(Region, levels = c("West", "East"))]
plotDiffComparisonExp <- ggplot(DiffPlotData[abs(value) < 5], aes(x = as.numeric(YearGroup),y = value,  col=Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log wage differential 0-5 Years of Experience") + scale_color_stata(name = "Type", labels = c("New Experience", "Old Experience", "Total Experience")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)

DiffPlotDataEdu <- merge(CoefficientsTotalExp[,.(Region,AgeGroup, YearGroup, TotalEdu)], Coefficients[,.(Region,AgeGroup,YearGroup, NewEdu, OldEdu)], by = c("Region","AgeGroup","YearGroup"))
DiffPlotDataEdu <- melt.data.table(DiffPlotDataEdu, id.vars = c("Region","AgeGroup","YearGroup"))
setnames(DiffPlotDataEdu, old = c("variable"), new = c("Type"))
DiffPlotDataEdu[,value := as.numeric(as.character(value))]
DiffPlotDataEdu[, Type := factor(Type, levels = c("NewEdu","OldEdu","TotalEdu"))] 
DiffPlotDataEdu[, Region := factor(Region, levels = c("West","East"))]

plotDiffComparisonEdu <- ggplot(DiffPlotDataEdu[abs(value) < 5], aes(x = as.numeric(YearGroup),y = value*5,  col=Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log wage differential 0-5 Years of Education") + scale_color_stata(name = "Type", labels = c("New Education", "Old Education", "Total Education")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()  + facet_grid(AgeGroup ~.)

#Analyse Human Capital Shares of education and experience by using mean experience and mean education values
#Create YearGroup variable
soep[,YearGroup := cut(syear, c(min(YearGroups[[1]])-1,sapply(YearGroups, max)))]
levels(soep$YearGroup) <- levels(DiffPlotData$YearGroup)

#Get Mean Wages
MeanWages <- soep[,.(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(YearGroup,AgeGroup, sampreg)]
MeanWages[, Region := droplevels(sampreg)]
MeanWages[, sampreg := NULL]
levels(MeanWages$Region) <- c("West", "East")
MeanWages[, Region := factor(Region, levels = c("West","East"))]

plotMeanWages <- ggplot(MeanWages, aes(x = as.numeric(YearGroup),y = MeanGrossWage, col = AgeGroup)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Gross Wage (2010 Euros)") + scale_color_stata(name = "Age Group") +
      scale_x_discrete(limits = 1:length(levels(MeanWages$YearGroup)), name = "Period", labels = levels(MeanWages$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe()

#Analyse Mean Old and New Experience over time
MeanExp <- soep[,.(MeanOldExp = mean(OldExp, na.rm = TRUE), MeanNewExp = mean(NewExp, na.rm = TRUE), MeanTotalExp = mean(TotalExp, na.rm = TRUE)), by = .(AgeGroup, YearGroup, sampreg)]
MeanExp[, Region := droplevels(sampreg)]
MeanExp[, sampreg := NULL]
levels(MeanExp$Region) <- c("West", "East")
MeanExp[, Region := factor(Region, levels = c("West","East"))]

MeanExpPlotData <- melt(MeanExp, id.vars = c("YearGroup", "AgeGroup", "Region"))
MeanExpPlotData[, Type := factor(variable, levels = c("MeanNewExp","MeanOldExp","MeanTotalExp"))]
MeanExpPlotData[, variable := NULL]

plotMeanExp <- ggplot(MeanExpPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Experience (Years)")  + scale_color_stata(name = "Type", labels = c("New Experience", "Old Experience", "Total Experience")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)


#Analyse Mean Old and New Experience over time
MeanEdu <- soep[,.(MeanOldEdu = mean(OldEdu, na.rm = TRUE), MeanNewEdu = mean(NewEdu, na.rm = TRUE), MeanTotalEdu = mean(TotalEdu, na.rm = TRUE)), by = .(AgeGroup, YearGroup, sampreg)]
MeanEdu[, Region := droplevels(sampreg)]
MeanEdu[, sampreg := NULL]
levels(MeanEdu$Region) <- c("West","East")
MeanEdu[, Region := factor(Region, levels = c("West","East"))]

MeanEduPlotData <- melt(MeanEdu, id.vars = c("YearGroup","AgeGroup", "Region"))
MeanEduPlotData[, Type := factor(variable, levels = c("MeanNewEdu","MeanOldEdu","MeanTotalEdu"))]
MeanEduPlotData[, variable := NULL]

plotMeanEdu <- ggplot(MeanEduPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Mean Education (Years)")  + scale_color_stata(name = "Type", labels = c("New Education", "Old Education", "Total Education")) + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)

HumanCapitalData <- merge(MeanExp, MeanEdu, by = c("YearGroup","AgeGroup", "Region"))
setnames(Coefficients, old = c("I(OldExp^2)","I(NewExp^2)"), new = c("OldExpSquared","NewExpSquared"))
HumanCapitalData <- merge(HumanCapitalData, Coefficients[,.(YearGroup, AgeGroup, Region, OldEdu, NewEdu, OldExp, OldExpSquared, NewExp, NewExpSquared)], by =  c("YearGroup","AgeGroup", "Region"))

setnames(CoefficientsTotalExp, old = c("I(TotalExp^2)"), new = c("TotalExpSquared"))
HumanCapitalData <- merge(HumanCapitalData, CoefficientsTotalExp[,.(YearGroup, AgeGroup, Region, TotalEdu, TotalExp, TotalExpSquared)], by =  c("YearGroup","AgeGroup", "Region"))

HumanCapitalData[, HCTotalEdu := as.numeric(as.character(TotalEdu))*MeanTotalEdu]
HumanCapitalData[, HCNewEdu := as.numeric(as.character(NewEdu))*MeanNewEdu]
HumanCapitalData[, HCOldEdu := as.numeric(as.character(OldEdu))*MeanOldEdu]

HumanCapitalData[, HCTotalExp := as.numeric(as.character(TotalExp))*MeanTotalExp + as.numeric(as.character(TotalExpSquared))*MeanTotalExp^2]
HumanCapitalData[, HCNewExp := as.numeric(as.character(NewExp))*MeanNewExp + as.numeric(as.character(NewExpSquared))*MeanNewExp^2]
HumanCapitalData[, HCOldExp := as.numeric(as.character(OldExp))*MeanOldExp + as.numeric(as.character(OldExpSquared))*MeanOldExp^2]

HumanCapitalData <- HumanCapitalData[,.(YearGroup,AgeGroup, Region, HCTotalEdu, HCNewEdu, HCOldEdu, HCTotalExp, HCNewExp, HCOldExp)]
HumanCapitalPlotData <- melt.data.table(HumanCapitalData, id.vars = c("YearGroup","AgeGroup","Region"))
setnames(HumanCapitalPlotData, old = "variable", new = "Type")
HumanCapitalPlotData[, Type := as.factor(Type)]
levels(HumanCapitalPlotData$Type) <- c("Total Education", "New Education", "Old Education", "Total Experience", "New Experience", "Old Experience")
HumanCapitalPlotData[, Type := factor(Type , levels = c("New Education", "Old Education", "Total Education","New Experience", "Old Experience", "Total Experience"))]

plotHumanCapital <- ggplot(HumanCapitalPlotData, aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)

EduSelection <- c("Total Education", "New Education", "Old Education")
plotHumanCapitalEdu <- ggplot(HumanCapitalPlotData[Type %in% EduSelection,], aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)

ExpSelection <- c("Total Experience", "New Experience", "Old Experience")
plotHumanCapitalExp <- ggplot(HumanCapitalPlotData[Type %in% ExpSelection,], aes(x = as.numeric(YearGroup),y = value, col = Type)) + geom_line(aes(lty = Region)) + geom_point() +
      labs(x = "Year", y = "Log Wage Gain for Mean Value")  + scale_color_stata(name = "Type") + 
      scale_x_discrete(limits = 1:length(levels(DiffPlotData$YearGroup)), name = "Period", labels = levels(DiffPlotData$YearGroup)) + 
      scale_linetype_stata(name = "Sample Region") + theme_tufte() + geom_rangeframe() + facet_grid(AgeGroup ~.)

# #Different HumanCapital calculation
# #Create Region variable in soep data
# soep[, Region := droplevels(sampreg)]
# levels(soep$Region) <- c("West","East")
# 
# setnames(Coefficients, old = c("NewExp","NewExpSquared","OldExp","OldExpSquared", "NewEdu", "OldEdu", "Tenure"), new = paste0("Coeff",c("NewExp","NewExpSquared","OldExp","OldExpSquared", "NewEdu", "OldEdu", "Tenure")))
# setnames(CoefficientsTotalExp, old = c("TotalExp","TotalExpSquared","TotalEdu", "Tenure"), new = paste0("Coeff",c("TotalExp","TotalExpSquared","TotalEdu", "Tenure")))
# 
# HumanCapital2 <- merge(soep, Coefficients, by = c("YearGroup","AgeGroup","Region"))
# HumanCapital2 <- merge(HumanCapital2, CoefficientsTotalExp, by = c("YearGroup","AgeGroup","Region"))
# HumanCapital2 <- HumanCapital2[,.(HCTotalEdu = mean(TotalEdu*as.numeric(as.character(CoeffTotalEdu)), na.rm = TRUE), HCOldEdu = mean(OldEdu*as.numeric(as.character(CoeffOldEdu)), na.rm = TRUE), HCNewEdu = mean(NewEdu*as.numeric(as.character(CoeffNewEdu)), na.rm = TRUE),
#                                   HCTotalExp = mean(TotalExp*as.numeric(as.character(CoeffTotalExp)) + TotalExp^2*as.numeric(as.character(CoeffTotalExpSquared)), na.rm = TRUE), HCOldExp = mean(OldExp*as.numeric(as.character(CoeffOldExp)) + OldExp^2*as.numeric(as.character(CoeffOldExpSquared)), na.rm = TRUE),  HCNewExp = mean(NewExp*as.numeric(as.character(CoeffNewExp)) + NewExp^2*as.numeric(as.character(CoeffNewExpSquared)), na.rm = TRUE)),
#                                by = .(YearGroup,Region,AgeGroup)]
#HumanCapitalData <- HumanCapital2

if(savePlots){
      plots <- ls()[grepl(ls(), pattern = "plot")]
      for(plot in plots){
            file <- paste0(plot,"ByAgeGroup", ".png")
            ggsave(filename = file, path = "./Graphics", plot = get(plot), device = "png", width = 18, height = 12, units = "cm")
      }
}

