library(foreign)
library(readstata13)
library(data.table)

LoadSoep <- function(sampleSelection = NULL, MinYear = NULL, MaxYear = NULL, PurchasingPowerAdaption = FALSE, FullTimeFilter = TRUE, OutlierHourlyWage = 100, AgeGroups = c(0,24,34,44,54, Inf), ExpGroups = c(0,5,10,15,20,25,30, Inf), PartTimeWeight = 0){
      #Load in SOEPLong pgen data
      pgen <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Code/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/pgen.dta")
      pgen <- as.data.table(pgen)
      setkeyv(pgen,c("pid","syear"))
      
      
      #Load in SOEPLong ppfad data (contains among others variable indicating residence before unification)
      ppfadl <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Code/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/ppfadl.dta")
      ppfadl <- as.data.table(ppfadl)
      setkeyv(ppfadl,c("pid", "syear"))
      
      soep <- merge(pgen, ppfadl)
      
      #Load in CPI-Data to deflate wages
      CPI <- read.csv("/Users/Christian/Statistik_Studium/EconProject/Code/Daten/InflationData.csv", sep = ";")
      names(CPI)[1] <- "syear"
      CPI <- as.data.table(CPI)
      setkeyv(CPI, "syear")
      soep <- merge(soep,CPI, all.x = TRUE)
      
      #Remove Observations from before 1990
      #soep <- soep[syear >= 1990,]
      
      # #Remove Observations with missing wage, actual weekly hours worked, work experience, years of education
      # Variables <- c("pglabgro", "pgtatzt", "pgexpft", "pgexppt", "pgbilzt")
      # for(variable in Variables){
      #    before <- nrow(soep)
      #    soep <- soep[as.numeric(get(variable)) >= 0,]
      #    after <- nrow(soep)
      #    print(variable)
      #    print(paste("Missing Values Removed", before - after))
      # }
      
      
      
      #Adjust Wages to 2010 Price Level
      soep[!is.na(CPIGermany) & pglabgro >= 0, realGrossWage := pglabgro/(CPIGermany/100),]
      
      #Generate Hourly Wages (Assumption 21.5 working days per month)
      avgWorkdays <- 21.5
      soep[!is.na(realGrossWage) & (pgtatzt >0), realHourlyWage := realGrossWage/((pgtatzt/5)*avgWorkdays)]
      
      #Generate age variable
      soep[syear > 0 & gebjahr > 0, age := syear - gebjahr]
      
      
      #Remove Observations with missing wage
      
      #Create Variables Old Experience
      #For Observations with available Experience Level in 1990 set OldExperience to that level
      soep[(syear <= 1990) & (pgexpft >= 0) & (pgexppt >= 0), OldExp := pgexpft + PartTimeWeight*pgexppt]
      #For Observations with with GebJahr + 6 + Years of Education > 1990 set Old Experience to 0.
      #Set OldExperience constant across observations after syear == 1990
      soep[syear >= 1990, OldExp := min(OldExp, na.rm = TRUE), by = pid]
      #Replace Inf by NA
      soep[OldExp == Inf, OldExp := NA]
      #Assumption: Consecutive Education started at the age of 6 with no experience gathered before finishing education
      soep[(gebjahr + 6 + pgbilzt >= 1990)&is.na(OldExp), OldExp := 0]
      
      #Create Variable New Expereince
      soep[(OldExp >= 0) & (pgexpft >= 0) & (pgexppt >= 0),NewExp := pgexpft + PartTimeWeight*pgexppt - OldExp]
      
      #Do the same for Education
      #For Observations with available Education Level in 1990 set OldExperience to that level
      soep[(syear <= 1990)&(pgbilzt >=0), OldEdu := pgbilzt]
      #Set Old Education constant across observations after syear == 1990
      soep[syear >= 1990, OldEdu := min(OldEdu, na.rm = TRUE), by = pid]
      #Replace Inf by NA
      soep[OldEdu == Inf, OldEdu := NA]
      #For others: Assume Education started at age 6 and was completed consecutively. Therefore OldEducation = min(Education,1990-(gebjahr + 6))
      #Max function avoids negative values. Use pmin and pmax to avoid aggregation across whole data.table
      soep[is.na(OldEdu)&(pgbilzt >=0), OldEdu := pmin(pgbilzt, pmax(1990-(gebjahr + 6),0))]
      
      #Create Variable New Education
      soep[!is.na(OldEdu)&(pgbilzt >=0),NewEdu := pgbilzt - OldEdu]
      
      #Generate Variable indicating East-West Migration
      soep[as.numeric(loc1989) %in% c(5,7,8,9),EastWestMigraton := (as.numeric(loc1989) == 7)&(as.numeric(sampreg) == 7)]
      #Generate Variable indicating West-East Migration
      soep[as.numeric(loc1989) %in% c(5,7,8,9),WestEastMigraton := (as.numeric(loc1989) == 8)&(as.numeric(sampreg) == 8)]
      
      #Generate Skill Group Variable
      soep[,NoVocationalDegree := as.numeric(pgbbil03) >= 7]
      soep[,VocationalDegree := (as.numeric(pgbbilo) >= 7) | (as.numeric(pgbbil01) >= 7)]
      soep[,CollegeDegree := (as.numeric(pgbbil02) >= 7)]
      soep$SkillGroup <- as.character(NA)
      soep[NoVocationalDegree == TRUE, SkillGroup := "No Vocational Degree"]
      soep[VocationalDegree == TRUE, SkillGroup := "Vocational Degree"]
      soep[CollegeDegree == TRUE, SkillGroup := "College Degree"]
      soep[,SkillGroup := droplevels(as.factor(SkillGroup))]
      
      if(!is.null(sampleSelection)){
            soep <- soep[as.numeric(psample) %in% sampleSelection,]
      }
      if(!is.null(MinYear)){
            soep <- soep[syear >= MinYear,]
      }
      if(!is.null(MaxYear)){
            soep <- soep[syear <= MaxYear,]
      }
      if(FullTimeFilter){
            soep <- soep[pgemplst == "[1] Voll erwerbstaetig" ,]
      }
      if(!is.null(AgeGroups)){
            soep[,AgeGroup := cut(age, breaks = AgeGroups)]
      }
      if(!is.null(ExpGroups)){
            soep[,ExpGroup := cut(OldExp + NewExp, breaks = ExpGroups)]
            soep[,OldExpGroup := cut(OldExp, breaks = ExpGroups)]
            soep[,NewExpGroup := cut(NewExp, breaks = ExpGroups)]
      }
      if(!is.null(OutlierHourlyWage)){
            soep <- soep[realHourlyWage <= OutlierHourlyWage,]
      }
      return(soep)
}
