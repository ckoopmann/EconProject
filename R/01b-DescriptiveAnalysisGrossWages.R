rm(list = ls())
#Load Data
source("R/00-DataPreparation.R")
library(ggplot2)
library(ggthemes)

soep <- LoadSoep(sampleSelection = c(7,9,11,12,14,16,17),MinYear = 1991, MaxYear = 2013, AgeGroups = c(0,24,34,44,54, Inf))

#Limit analysis to samples A and C and fulltime (>30h) employees
MeanGrossWagesByAgeGroup <- soep[(syear >= 1991) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, AgeGroup)]
plotMeanGrossWagesByAgeGroup <- ggplot(na.omit(MeanGrossWagesByAgeGroup, cols = "AgeGroup"), aes(x = syear,y = MeanGrossWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Age Group", labels = c("< 25", "25 - 34", "35 - 44", "45 - 54", " > 54")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesByAgeGroup.png", path = "./Graphics", plot = plotMeanGrossWagesByAgeGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to 25-34 Group
Reference <- MeanGrossWagesByAgeGroup[AgeGroup == "(24,34]",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesByAgeGroup <- merge(MeanGrossWagesByAgeGroup, Reference, by = c("syear","sampreg"))
RelWagesByAgeGroup[,RelWage := MeanGrossWage/RefWage]
plotRelGrossWagesByAgeGroup <- ggplot(na.omit(RelWagesByAgeGroup[as.numeric(AgeGroup)>=3,], cols = "AgeGroup"), aes(x = syear,y = RelWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
                                          labs(x = "Year", y = "Mean Gross Wages relative to 25-34 Age Group") + scale_color_stata(name = "Age Group", labels = c("35 - 44", "45 - 54", " > 54")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesByAgeGroup.png", path = "./Graphics", plot = plotRelGrossWagesByAgeGroup, device = "png", width = 18, height = 12, units = "cm")


#Get Wage Differentials West East
MeanWageDifferentialsByAgeGroup <- melt.data.table(MeanGrossWagesByAgeGroup, id.vars = c("syear","sampreg","AgeGroup"))
MeanWageDifferentialsByAgeGroup <- dcast.data.table(MeanWageDifferentialsByAgeGroup, formula = syear + AgeGroup ~ sampreg)
names(MeanWageDifferentialsByAgeGroup)[c(3,4)] <- c("WestWage", "EastWage")
MeanWageDifferentialsByAgeGroup[, RelWageDiff := WestWage/EastWage]
plotMeanWageDifferentialsByAgeGroup <- ggplot(na.omit(MeanWageDifferentialsByAgeGroup, cols = "AgeGroup"), aes(x = syear,y = RelWageDiff, col=AgeGroup)) + geom_line() + 
                                          labs(x = "Year", y = "Relative Gross Wages (West/East)") + scale_color_stata(name = "Age Group", labels = c("< 25", "25 - 34", "35 - 44", "45 - 54", " > 54")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanWageDifferentialsByAgeGroup.png", path = "./Graphics", plot = plotMeanWageDifferentialsByAgeGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Do the same for SkilGroup

#Limit analysis to samples A and C and fulltime (>30h) employees
MeanGrossWagesBySkillGroup <- soep[(syear >= 1991) & !is.na(SkillGroup) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, SkillGroup)]
plotMeanGrossWagesBySkillGroup <- ggplot(MeanGrossWagesBySkillGroup, aes(x = syear,y = MeanGrossWage, col=SkillGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) +
      labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Skill Group") + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesBySkillGroup.png", path = "./Graphics", plot = plotMeanGrossWagesBySkillGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Plot Wages relative to reference Group (No Vocational Degree)
ReferenceSkill <- MeanGrossWagesBySkillGroup[SkillGroup == "No Vocational Degree",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesBySkillGroup <- merge(MeanGrossWagesBySkillGroup, ReferenceSkill, by = c("syear","sampreg"))
RelWagesBySkillGroup[,RelWage := MeanGrossWage/RefWage]
plotRelGrossWagesBySkillGroup <- ggplot(na.omit(RelWagesBySkillGroup[as.numeric(SkillGroup)!=2,], cols = "SkillGroup"), aes(x = syear,y = RelWage, col=SkillGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages relative to group wihtout vocational degree") + scale_color_stata(name = "Skill Group") + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesBySkillGroup.png", path = "./Graphics", plot = plotRelGrossWagesBySkillGroup, device = "png", width = 18, height = 12, units = "cm")


#Get Wage Differentials
MeanWageDifferentialsBySkillGroup <- melt.data.table(MeanGrossWagesBySkillGroup, id.vars = c("syear","sampreg","SkillGroup"))
MeanWageDifferentialsBySkillGroup <- dcast.data.table(MeanWageDifferentialsBySkillGroup, formula = syear + SkillGroup ~ sampreg)
names(MeanWageDifferentialsBySkillGroup)[c(3,4)] <- c("WestWage", "EastWage")
MeanWageDifferentialsBySkillGroup[, RelWageDiff := WestWage/EastWage]
plotMeanWageDifferentialsBySkillGroup <- ggplot(MeanWageDifferentialsBySkillGroup, aes(x = syear,y = RelWageDiff, col=SkillGroup)) + geom_line() +
      labs(x = "Year", y = "Relative Gross Wages (West/East)") + scale_color_stata(name = "Skill Group") + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWageDifferentialsBySkillGroup.png", path = "./Graphics", plot = plotMeanWageDifferentialsBySkillGroup ,  device = "png", width = 18, height = 12, units = "cm")

#Analyse by both Groups through faceting across Skill Group
MeanGrossWagesBySkillAge <- soep[(syear >= 1991) & !is.na(SkillGroup) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, SkillGroup, AgeGroup)]
plotMeanGrossWagesBySkillAge <- ggplot(MeanGrossWagesBySkillAge, aes(x = syear,y = MeanGrossWage, col=AgeGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + facet_grid(facets = SkillGroup ~.)

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
MeanGrossWagesByExpGroup <- soep[(syear >= 1991) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, ExpGroup)]
plotMeanGrossWagesByExpGroup <- ggplot(na.omit(MeanGrossWagesByExpGroup, cols = "ExpGroup"), aes(x = syear,y = MeanGrossWage, col=ExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Total Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesByExpGroup.png", path = "./Graphics", plot = plotMeanGrossWagesByExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanGrossWagesByExpGroup[ExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesByExpGroup <- merge(MeanGrossWagesByExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByExpGroup[,RelWage := MeanGrossWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelGrossWagesByExpGroup <- ggplot(na.omit(RelWagesByExpGroup[as.numeric(ExpGroup) %in% GroupSelection], cols = "ExpGroup"), aes(x = syear,y = RelWage, col=ExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Total Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesByExpGroup.png", path = "./Graphics", plot = plotRelGrossWagesByExpGroup, device = "png", width = 18, height = 12, units = "cm")

##Do the same for Old Experience
#Analysis by Old Experience group
MeanGrossWagesByOldExpGroup <- soep[(syear >= 1991) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, OldExpGroup)]
plotMeanGrossWagesByOldExpGroup <- ggplot(na.omit(MeanGrossWagesByOldExpGroup, cols = "OldExpGroup"), aes(x = syear,y = MeanGrossWage, col=OldExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Old Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesByOldExpGroup.png", path = "./Graphics", plot = plotMeanGrossWagesByOldExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanGrossWagesByOldExpGroup[OldExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesByOldExpGroup <- merge(MeanGrossWagesByOldExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByOldExpGroup[,RelWage := MeanGrossWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelGrossWagesByOldExpGroup <- ggplot(na.omit(RelWagesByOldExpGroup[as.numeric(OldExpGroup) %in% GroupSelection], cols = "OldExpGroup"), aes(x = syear,y = RelWage, col=OldExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Old Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesByOldExpGroup.png", path = "./Graphics", plot = plotRelGrossWagesByOldExpGroup, device = "png", width = 18, height = 12, units = "cm")

##Do the same for New Experience
#Analysis by New Experience group##Do the same for Old Experience
#Analysis by Old Experience group
MeanGrossWagesByNewExpGroup <- soep[(syear >= 1991) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, NewExpGroup)]
plotMeanGrossWagesByNewExpGroup <- ggplot(na.omit(MeanGrossWagesByNewExpGroup, cols = "NewExpGroup"), aes(x = syear,y = MeanGrossWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Years of New Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesByNewExpGroup.png", path = "./Graphics", plot = plotMeanGrossWagesByNewExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanGrossWagesByNewExpGroup[NewExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesByNewExpGroup <- merge(MeanGrossWagesByNewExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByNewExpGroup[,RelWage := MeanGrossWage/RefWage]
GroupSelection <- c(2,3,4)
plotRelGrossWagesByNewExpGroup <- ggplot(na.omit(RelWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection], cols = "NewExpGroup"), aes(x = syear,y = RelWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of New Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesByNewExpGroup.png", path = "./Graphics", plot = plotRelGrossWagesByNewExpGroup, device = "png", width = 18, height = 12, units = "cm")

MeanGrossWagesByNewExpGroup <- soep[(syear >= 1991) , .(MeanGrossWage = mean(realGrossWage, na.rm = TRUE)), by = .(syear, sampreg, NewExpGroup)]
GroupSelection <- c(1,2,3)
plotMeanGrossWagesByNewExpGroup <- ggplot(na.omit(MeanGrossWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection,], cols = "NewExpGroup"), aes(x = syear,y = MeanGrossWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages (EUR/h)") + scale_color_stata(name = "Years of Experience", labels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotMeanGrossWagesByNewExpGroup.png", path = "./Graphics", plot = plotMeanGrossWagesByNewExpGroup , device = "png", width = 18, height = 12, units = "cm")

#Get Means relative to <5 Group
Reference <- MeanGrossWagesByNewExpGroup[NewExpGroup == "(0,5]",.(syear, sampreg, RefWage = MeanGrossWage)]
RelWagesByNewExpGroup <- merge(MeanGrossWagesByNewExpGroup, Reference, by = c("syear","sampreg"))
RelWagesByNewExpGroup[,RelWage := MeanGrossWage/RefWage]
GroupSelection <- c(2,3)
plotRelGrossWagesByNewExpGroup <- ggplot(na.omit(RelWagesByNewExpGroup[as.numeric(NewExpGroup) %in% GroupSelection], cols = "NewExpGroup"), aes(x = syear,y = RelWage, col=NewExpGroup)) + geom_line(aes(lty = as.factor(as.numeric(sampreg)))) + 
      labs(x = "Year", y = "Mean Gross Wages relative to 0-5 Experience Group") +  scale_color_stata(name = "Years of Experience", labels = c( "<5", "5 - 10", "10 - 15", "15 - 20", "20 - 25","25 - 30", ">30")[GroupSelection]) + scale_linetype_stata(name = "Sample Region", labels = c("West Germany", "East Germany")) + theme_tufte() + geom_rangeframe()
ggsave(filename = "plotRelGrossWagesByNewExpGroup.png", path = "./Graphics", plot = plotRelGrossWagesByNewExpGroup, device = "png", width = 18, height = 12, units = "cm")
