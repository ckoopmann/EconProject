rm(list = ls())
#Load Data
source("R/00-DataPreparation.R")
library(ggplot2)
library(ggthemes)

soep <- LoadSoep(sampleSelection = c(7,9,11,12,14,16,17),MinYear = 1991, MaxYear = 2013, AgeGroups = c(0,24,34,44,54, Inf))

#Limit analysis to samples A and C and fulltime (>30h) employees
MeanHourlyWagesByAgeGroup <- soep[(syear >= 1991) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, AgeGroup)]
plotMeanHourlyWagesByAgeGroup <- ggplot(na.omit(MeanHourlyWagesByAgeGroup, cols = "AgeGroup"), aes(x = syear,y = MeanHourlyWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Age Group", labels = c("< 25", "25 - 34", "35 - 44", "45 - 54", " > 54")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesByAgeGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesByAgeGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to 25-34 Group
Reference <- MeanHourlyWagesByAgeGroup[AgeGroup == "(24,34]",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesByAgeGroup <- merge(MeanHourlyWagesByAgeGroup, Reference, by = c("syear","sampreg"))
RelWagesByAgeGroup[,RelWage := MeanHourlyWage/RefWage]
plotRelHourlyWagesByAgeGroup <- ggplot(na.omit(RelWagesByAgeGroup[as.numeric(AgeGroup)>=3,], cols = "AgeGroup"), aes(x = syear,y = RelWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
                                          labs(x = "Year", y = "Mean Hourly Gross Wages relative to 25-34 Age Group") + scale_color_stata(name = "Age Group", labels = c("35 - 44", "45 - 54", " > 54")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesByAgeGroup.png", path = "./Graphics", plot = plotRelHourlyWagesByAgeGroup, device = "png", width = 18, height = 12, units = "cm")


#Get Wage Differentials West East
MeanWageDifferentialsByAgeGroup <- melt.data.table(MeanHourlyWagesByAgeGroup, id.vars = c("syear","sampreg","AgeGroup"))
MeanWageDifferentialsByAgeGroup <- dcast.data.table(MeanWageDifferentialsByAgeGroup, formula = syear + AgeGroup ~ sampreg)
names(MeanWageDifferentialsByAgeGroup)[c(3,4)] <- c("WestWage", "EastWage")
MeanWageDifferentialsByAgeGroup[, RelWageDiff := WestWage/EastWage]
plotMeanWageDifferentialsByAgeGroup <- ggplot(na.omit(MeanWageDifferentialsByAgeGroup, cols = "AgeGroup"), aes(x = syear,y = RelWageDiff, col=AgeGroup)) + geom_line() + 
                                          labs(x = "Year", y = "Relative Hourly Wages (West/East)") + scale_color_stata(name = "Age Group", labels = c("< 25", "25 - 34", "35 - 44", "45 - 54", " > 54")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanWageDifferentialsByAgeGroup.png", path = "./Graphics", plot = plotMeanWageDifferentialsByAgeGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Do the same for SkilGroup

#Limit analysis to samples A and C and fulltime (>30h) employees
MeanHourlyWagesBySkillGroup <- soep[(syear >= 1991) & !is.na(SkillGroup) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, SkillGroup)]
plotMeanHourlyWagesBySkillGroup <- ggplot(MeanHourlyWagesBySkillGroup, aes(x = syear,y = MeanHourlyWage, col=SkillGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) +
      labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Skill Group") + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesBySkillGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesBySkillGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Plot Wages relative to reference Group (No Vocational Degree)
ReferenceSkill <- MeanHourlyWagesBySkillGroup[SkillGroup == "No Vocational Degree",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesBySkillGroup <- merge(MeanHourlyWagesBySkillGroup, ReferenceSkill, by = c("syear","sampreg"))
RelWagesBySkillGroup[,RelWage := MeanHourlyWage/RefWage]
plotRelHourlyWagesBySkillGroup <- ggplot(na.omit(RelWagesBySkillGroup[as.numeric(SkillGroup)!=2,], cols = "SkillGroup"), aes(x = syear,y = RelWage, col=SkillGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages relative to group wihtout vocational degree") + scale_color_stata(name = "Skill Group") + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesBySkillGroup.png", path = "./Graphics", plot = plotRelHourlyWagesBySkillGroup, device = "png", width = 18, height = 12, units = "cm")


#Get Wage Differentials
MeanWageDifferentialsBySkillGroup <- melt.data.table(MeanHourlyWagesBySkillGroup, id.vars = c("syear","sampreg","SkillGroup"))
MeanWageDifferentialsBySkillGroup <- dcast.data.table(MeanWageDifferentialsBySkillGroup, formula = syear + SkillGroup ~ sampreg)
names(MeanWageDifferentialsBySkillGroup)[c(3,4)] <- c("WestWage", "EastWage")
MeanWageDifferentialsBySkillGroup[, RelWageDiff := WestWage/EastWage]
plotMeanWageDifferentialsBySkillGroup <- ggplot(MeanWageDifferentialsBySkillGroup, aes(x = syear,y = RelWageDiff, col=SkillGroup)) + geom_line() +
      labs(x = "Year", y = "Relative Hourly Wages (West/East)") + scale_color_stata(name = "Skill Group") + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanWageDifferentialsBySkillGroup.png", path = "./Graphics", plot = plotMeanWageDifferentialsBySkillGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Analyse by both Groups through faceting across Skill Group
MeanHourlyWagesBySkillAge <- soep[(syear >= 1991) & !is.na(SkillGroup) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, SkillGroup, AgeGroup)]
plotMeanHourlyWagesBySkillAge <- ggplot(MeanHourlyWagesBySkillAge, aes(x = syear,y = MeanHourlyWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + facet_grid(facets = SkillGroup ~.)

#Get Relative Frequencies for Skill Groups
SkillProp<- soep[(syear >= 1991) & !is.na(SkillGroup) , .(PropNoVoc = mean(SkillGroup == "No Vocational Degree"), PropVoc = mean(SkillGroup == "Vocational Degree"),PropCol = mean(SkillGroup == "College Degree")), by = .(syear,sampreg)]
SkillPropPlotData <- melt.data.table(SkillProp, id.vars = c("syear", "sampreg"))

plotSkillProps <- ggplot(SkillPropPlotData, aes(x = syear,y = value, col= variable)) + geom_line(aes(lty = sampreg)) + 
      labs(x = "Year", y = "Proportion of Sample (West/East)") + scale_color_stata(name = "Skill Group", labels = c("No Vocational Degree", "Vocational Degree", "College Degree")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()

ggsave(filename = "plotSkillProps.png", path = "./Graphics", plot = plotSkillProps,  device = "png", width = 18, height = 12, units = "cm")

#Plot relative Frequencies for Age Groups
AgeProp <- soep[(syear >= 1991) & !is.na(AgeGroup), .(Number = .N), by = .(syear, sampreg, AgeGroup)]
AgeProp[,Total := sum(Number), by = .(syear, sampreg)]
AgeProp[, Prop := Number/Total]

plotAgeProps <- ggplot(AgeProp, aes(x = syear,y = Prop, col= AgeGroup)) + geom_line(aes(lty = sampreg)) + 
      labs(x = "Year", y = "Proportion of Sample (West/East)") + scale_color_stata(name = "Age Group", labels = c("< 25", "25 - 34", "35 - 44", "45 - 54", " > 54")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()

ggsave(filename = "plotAgeProps.png", path = "./Graphics", plot = plotAgeProps,  device = "png", width = 18, height = 12, units = "cm")

#Plot Age Mean 
AgeMean <- soep[(syear >= 1991), .(MeanAge = mean(age, na.rm = TRUE)), by = .(syear, sampreg)]

plotAgeMeans <- ggplot(AgeMean, aes(x = syear,y = MeanAge)) + geom_line(aes(lty = sampreg)) + 
      labs(x = "Year", y = "Proportion of Sample (West/East)")  + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()

ggsave(filename = "plotAgeMeans.png", path = "./Graphics", plot = plotAgeMeans,  device = "png", width = 18, height = 12, units = "cm")

#Analyse Mean Old and New Experience over time
MeanExp <- soep[syear >= 1991,.(MeanOldExp = mean(OldExp, na.rm = TRUE), MeanNewExp = mean(NewExp, na.rm = TRUE)), by = .(syear, sampreg)]
MeanExp <- melt(MeanExp, id.vars = c("syear","sampreg"))
plotMeanExp <- ggplot(MeanExp, aes(x = syear,y = value, col = variable)) + geom_line(aes(lty = sampreg)) + 
      labs(x = "Year", y = "Mean Experience (Years)")  + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + scale_color_stata(name = "Type", labels = c("Old","New")) + theme_tufte() + geom_rangeframe()

#Analysis by Experience group
MeanHourlyWagesByExpGroup <- soep[(syear >= 1991) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, ExpGroup)]
plotMeanHourlyWagesByExpGroup <- ggplot(na.omit(MeanHourlyWagesByExpGroup, cols = "ExpGroup"), aes(x = syear,y = MeanHourlyWage, col=ExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Total Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesByExpGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesByExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanHourlyWagesByExpGroup[ExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesByExpGroup <- merge(MeanHourlyWagesByExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByExpGroup[,RelWage := MeanHourlyWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelHourlyWagesByExpGroup <- ggplot(na.omit(RelWagesByExpGroup[as.numeric(ExpGroup) %in% GroupSelection], cols = "ExpGroup"), aes(x = syear,y = RelWage, col=ExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Total Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesByExpGroup.png", path = "./Graphics", plot = plotRelHourlyWagesByExpGroup, device = "png", width = 18, height = 12, units = "cm")

##Do the same for Old Experience
#Analysis by Old Experience group
MeanHourlyWagesByOldExpGroup <- soep[(syear >= 1991) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, OldExpGroup)]
plotMeanHourlyWagesByOldExpGroup <- ggplot(na.omit(MeanHourlyWagesByOldExpGroup, cols = "OldExpGroup"), aes(x = syear,y = MeanHourlyWage, col=OldExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Old Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesByOldExpGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesByOldExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanHourlyWagesByOldExpGroup[OldExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesByOldExpGroup <- merge(MeanHourlyWagesByOldExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByOldExpGroup[,RelWage := MeanHourlyWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelHourlyWagesByOldExpGroup <- ggplot(na.omit(RelWagesByOldExpGroup[as.numeric(OldExpGroup) %in% GroupSelection], cols = "OldExpGroup"), aes(x = syear,y = RelWage, col=OldExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Old Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesByOldExpGroup.png", path = "./Graphics", plot = plotRelHourlyWagesByOldExpGroup, device = "png", width = 18, height = 12, units = "cm")

##Do the same for New Experience
#Analysis by New Experience group##Do the same for Old Experience
#Analysis by Old Experience group
MeanHourlyWagesByNewExpGroup <- soep[(syear >= 1991) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, NewExpGroup)]
plotMeanHourlyWagesByNewExpGroup <- ggplot(na.omit(MeanHourlyWagesByNewExpGroup, cols = "NewExpGroup"), aes(x = syear,y = MeanHourlyWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Years of New Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesByNewExpGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesByNewExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanHourlyWagesByNewExpGroup[NewExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesByNewExpGroup <- merge(MeanHourlyWagesByNewExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByNewExpGroup[,RelWage := MeanHourlyWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelHourlyWagesByNewExpGroup <- ggplot(na.omit(RelWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection], cols = "NewExpGroup"), aes(x = syear,y = RelWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of New Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesByNewExpGroup.png", path = "./Graphics", plot = plotRelHourlyWagesByNewExpGroup, device = "png", width = 18, height = 12, units = "cm")

MeanHourlyWagesByNewExpGroup <- soep[(syear >= 1991) , .(MeanHourlyWage = mean(realHourlyWage, na.rm = TRUE)), by = .(syear, sampreg, NewExpGroup)]
GroupSelection <- c(1,2,3)
plotMeanHourlyWagesByNewExpGroup <- ggplot(na.omit(MeanHourlyWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection,], cols = "NewExpGroup"), aes(x = syear,y = MeanHourlyWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanHourlyWagesByNewExpGroup.png", path = "./Graphics", plot = plotMeanHourlyWagesByNewExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanHourlyWagesByNewExpGroup[NewExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanHourlyWage)]
RelWagesByNewExpGroup <- merge(MeanHourlyWagesByNewExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByNewExpGroup[,RelWage := MeanHourlyWage/RefWage]
GroupSelection <- c(2,3)
plotRelHourlyWagesByNewExpGroup <- ggplot(na.omit(RelWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection], cols = "NewExpGroup"), aes(x = syear,y = RelWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Hourly Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelHourlyWagesByNewExpGroup.png", path = "./Graphics", plot = plotRelHourlyWagesByNewExpGroup, device = "png", width = 18, height = 12, units = "cm")
