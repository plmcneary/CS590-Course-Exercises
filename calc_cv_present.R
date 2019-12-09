# CALCULATE COEF. VAR. FOR EACH ROW PLOT
# INPUT DATA IS ALL IND AND HEADER FILES WITH A FEW OUTLIERS REMOVED, ODD TESTS
library(lattice)

# READ IN ALL THE DATA (APPENDED HEADER AND IND FILES)
ad <- read.csv("P:/tip/Staff_Folders/McNeary_Peter/2019/3-coefficient_of_variation_model/Data/all_data.csv",
colClasses = "character")

# Number of records

# IMPORT, MERGE THE PROGENY TEST DESCRIPTIONS FROM ACCESS DB
ptd <- read.csv("P:/tip/Staff_Folders/McNeary_Peter/2019/3-coefficient_of_variation_model/Data/2-CYCLE-PROGENY-TEST-DESCRIPTIONS.txt")
ad$SERTYPE <- ptd$TYPE[match(ad$ser, ptd$SERIES)]

# ONLY KEEP OP AND DIALLEL SERIES TYPES (DROP NA, SPECIAL, BLOCK, COLD HARDY, CFI-OP)
ad <- ad[which(ad$SERTYPE %in% c("PLANT","PLANT-OP","SG","SG-OP","SG&PLANT","SG&PLANT OP", "FG-OP")),]

# Number of records

# SUBSET TO AGE 6 MEASUREMENTS
# AGE MISSING FOR HEADER FILES, ASSUME 6 YEAR
# Make a dataset with only age 6 years (or NA age)
ad$age <- as.numeric(ad$age)
ad6 <- ad[which(is.na(ad$age) | ad$age == 6 ),]
# Make a dataset with 7 that are not measured at age 6, append it
ad7 <- ad[which(ad$age == 7 & !ad$ser%in%ad6$ser),]
ad67 <- rbind(ad6, ad7)
# Make a dataset with age 5 that are not measured at age 6 or 7, append it
ad5 <- ad[which(ad$age == 5 & !ad$ser%in%ad67$ser),]
ad567 <- rbind(ad67, ad5)

# Overwrite ad with ad567
ad <- ad567

# Number of records
####################################

# DROP NON STATUS 1 AND 2 TREES
ad <- ad[which(ad$status == "1" | ad$status == "2"),]

# CALC ROW PLOT STATs - NOTE NOT ALL ARE ROWPLOTS
treeCount <- aggregate(ht ~ ser + test + rep + seedlot + female + male,
                       FUN = length, ad)
# ONLY KEEP PLOTS WITH 6 STATUS 1 OR 2 TREES (STRICT)
plotsToKeep <- treeCount[which(treeCount$ht == 6),]
plotsToKeep$plotid <- paste(plotsToKeep$ser, plotsToKeep$test, plotsToKeep$rep, plotsToKeep$seedlot, plotsToKeep$female, plotsToKeep$male)
ad$plotid <- paste(ad$ser, ad$test, ad$rep, ad$seedlot, ad$female, ad$male)
ad <- ad[which(ad$plotid %in% plotsToKeep$plotid),]

# DROP TREES NOT IN PEDIGREE
ped <- read.csv("P:/tip/Staff_Folders/McNeary_Peter/2019/3-coefficient_of_variation_model/Data/pedigree_2019_11_07_12_06_58.csv")
ad$fem2 <- gsub(" ", "", ifelse(is.na(as.numeric(gsub("-","", ad$female))), gsub("-","", ad$female), paste0("N",gsub("-","", ad$female))))
ad$mal2 <- gsub(" ", "", ifelse(is.na(as.numeric(gsub("-","", ad$male))), gsub("-","", ad$male), paste0("N",gsub("-","", ad$male))))
#unique(ad$fem2[which(!ad$fem2%in%ped$Id)])
ad <- ad[which(ad$fem2%in%ped$Id), ]
ad[which(ad$mal2 == ""),"mal2"] <- "OP"
ad <- ad[which(ad$mal2%in%ped$Id | ad$mal2 == "OP"), ]
ad$female <- ad$fem2
ad$male <- ad$mal2
ad$FAMTYPE <- ifelse(ad$male == "OP", "OP", "FS")

# DROP PLOTS WITH LESS THAN 5 HTS
htCount <- aggregate(ht ~ plotid, FUN = function(x) length(which(!is.na(x))), ad)
plotsToDrop <- htCount$plotid[which(htCount$ht < 5)]
ad <- ad[which(!ad$plotid%in%plotsToDrop),]


# CALCULATE PLOT CVs
ad$ht <- as.numeric(ad$ht)
plotMeans <- aggregate(ht ~ ser + test + rep + seedlot + female + male + plotid + FAMTYPE, FUN = mean, ad)
plotSDs <- aggregate(ht ~ ser + test + rep + seedlot + female + male + plotid + FAMTYPE, FUN = sd, ad)
names(plotMeans)[which(names(plotMeans)=="ht")] <- "Mean"
names(plotSDs)[which(names(plotSDs)=="ht")] <- "SD"
plotStats <- merge(plotMeans, plotSDs, by = c("ser","test","rep","seedlot","female","male","plotid","FAMTYPE"))
plotStats$CV <- plotStats$SD/plotStats$Mean*100	

# IMPORT REGION
plotStats$SITE <- paste0(plotStats$ser, "-", plotStats$test)
ptd$SITE <- paste0(ptd$SERIES, "-", ptd$TEST)
plotStats$REGION <- ptd$REGION[match(plotStats$SITE,ptd$SITE)] 
plotStats$REGION[which(plotStats$ser == "516")] <- 1
plotStats$SERTYPE <- ptd$TYPE[match(plotStats$ser, ptd$SERIES)]
# MAKE REGION CODES (merged 8)
regCodes <- data.frame(REGCODE = 1:8,
                       REGLABEL = c("Virginia", "NC Coastal Plain",
                                   "SC Coastal Plain","GA/FL Coastal Plain", 
                                    "Lower Gulf", "Upper  Gulf", "GA/SC Piedmont",
                                    "GA/SC Piedmont"),
                        REGION = c("VA","NC","SCcoast","GAFLcoast","LG","UG","pied","pied"))
plotStats$REGION <- regCodes$REGION[match(plotStats$REGION, regCodes$REGCODE)]
plotStats$REGIONLabel <- regCodes$REGLABEL[match(plotStats$REGION, regCodes$REGION)]

# LOOK FOR OUTLIERS
densityplot(~Mean/10|REGION*FAMTYPE, plotStats) 
densityplot(~SD/10|REGION*FAMTYPE, plotStats) 

# CALCULATE MEANS
densityplot(~CV|REGIONLabel, groups = FAMTYPE, plotStats, auto.key=T)
aggregate(CV ~ FAMTYPE + REGION , plotStats, FUN = mean)
