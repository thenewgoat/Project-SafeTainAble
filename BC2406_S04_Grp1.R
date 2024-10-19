#--------------------------------------BC2406 Team Project R Script---------------------------------------------------#
#---------------------------------------Submitted By: S04 Group 1-------------------------------------------------------#

# Team Members: 1. Celeste Ang Jianing (U2222319H)
#               2. Lim Kiat Sen, Jaron (U2222010K)
#               3. Sally Ngui Yu Ying (U2222782A)
#               4. Wu Rixin (U2221172G)
#               5. Zhang Xinyang (U2221842C)

# ================================================================
# Load Libraries (Install packages if not found)
#=================================================================
library(data.table)
library(ggplot2)
library(summarytools)
library(dplyr)
library(gridExtra)
library(corrplot)
library(tidyr)
library(forcats)
library(stringr)
library(caTools)
library(olsrr)
library(car)
library(rpart)
library(caret)
library(rpart.plot)
library(olsrr)

#=================================================================
# Set Working Directory
#=================================================================
working_path <- getwd()
setwd(working_path)

# ++==========================++
# ||   Phase 1: Data Cleaning ||
# ++==========================++

#=================================================================
# Data import
#=================================================================
accident <- fread("accident_hazardous_liquid_jan2010_present.csv")
dim(accident)

# ================================================================
# Data Cleaning: handle empty string
#=================================================================
# For columns with empty string, replace with NA
accident[accident == ''] <- NA

#check NA values
colSums(is.na(accident))

# ================================================================
# Data Cleaning: handle NULL values
#=================================================================
# PIPE_FAC_NAME
accident[is.na(PIPE_FAC_NAME),.N]    # 97 Null values
# Column is just a name for pipe facilities, and is not likely to significantly impact any model built
# Choice: Fill with placeholder

accident[is.na(PIPE_FAC_NAME), PIPE_FAC_NAME := 'NO NAME']

#---------------------------------------------------------------------------------------------------------------------------------
# ONSHORE_STATE_ABBREVIATION
accident[is.na(ONSHORE_STATE_ABBREVIATION),.N]
accident[is.na(ONSHORE_STATE_ABBREVIATION) & ON_OFF_SHORE != "OFFSHORE",] 

# All stateless accidents occurred offshore
accident[is.na(ONSHORE_STATE_ABBREVIATION), ONSHORE_STATE_ABBREVIATION := "OFFSHORE"]

#---------------------------------------------------------------------------------------------------------------------------------
# ONSHORE_CITY_NAME
accident[, ONSHORE_CITY_NAME := gsub('\""', "", ONSHORE_CITY_NAME)]   # Remove triple quotes
accident[, ONSHORE_CITY_NAME := gsub(',', "", ONSHORE_CITY_NAME)]     # Remove commas
accident[, ONSHORE_CITY_NAME := gsub("\\.", "", ONSHORE_CITY_NAME)]   # Remove dot
accident <- accident[-grep("[0-9]", accident$ONSHORE_CITY_NAME)]  

temp <- c("California"," AL"," AK"," AZ"," AR"," CA"," CO"," CT"," DE"," FL"," GA"," HI"," ID"," IL"," IN"," IA"," KS"," KY",
          " LA"," ME"," MD"," MA"," MI"," MN"," MS"," MO"," MT"," NE"," NV"," NH"," NJ"," NM"," NY"," NC"," ND"," OH"," OK",
          " OR"," PA"," RI"," SC"," SD"," TN"," TX"," UT"," VT"," VA"," WA"," WV"," WI"," WY")
accident[, ONSHORE_CITY_NAME := sapply(ONSHORE_CITY_NAME, function(cell) {
  for (word in temp) {
    cell <- gsub(paste0("\\b", word, "\\b"), "", cell)  # Remove exact matches
    cell <- gsub("\\s{2,}", " ", cell)                  # Remove extra spaces if any
    cell <- trimws(cell)                                # Remove leading/trailing whitespace
  }
  return(cell)
})]


temp1 <- c('NOT WITHIN MUNICIPALITY', 'N/A','N/A - ACCIDENT OCCURED IN REMOTE MARSH AREA.','Near','Nearest','None',
           'NOT WITHIN A CITY')
pattern <- paste0("\\b", temp, "\\b", collapse = "|")
accident[grepl(pattern, ONSHORE_CITY_NAME,ignore.case = TRUE), ONSHORE_CITY_NAME := 'Not within a municipality']

# Null values can be due to a multitude of factors:
# such as being offshore, not within a municipality, or an error of omission
# Offshore rows can be kept, but we are unable to determine if the omission was deliberate (accidentally left out or left out because no city)
# Hence, Onshore&NA rows will be removed

accident[is.na(ONSHORE_CITY_NAME) & ON_OFF_SHORE == 'ONSHORE',.N]          # 81 rows
#Delete these 81 rows
accident <- accident[!(is.na(ONSHORE_CITY_NAME) & ON_OFF_SHORE == 'ONSHORE')]    # 81 rows deleted


# For Offshore rows: Replace NA with OFFSHORE
accident[is.na(ONSHORE_CITY_NAME) & ON_OFF_SHORE == 'OFFSHORE', ONSHORE_CITY_NAME := 'OFFSHORE']

#---------------------------------------------------------------------------------------------------------------------------------
# ONSHORE_COUNTY_NAME (104 Null Values)

accident[, ONSHORE_COUNTY_NAME := gsub('\""', "", ONSHORE_COUNTY_NAME)]   # Remove triple quotes
accident[, ONSHORE_COUNTY_NAME := gsub(',', "", ONSHORE_COUNTY_NAME)]     # Remove commas
accident[, ONSHORE_COUNTY_NAME := gsub("\\.", "", ONSHORE_COUNTY_NAME)]   # Remove dot


temp2 <- c("Parish","KS","Texas","Wyoming","County", "California"," AL"," AK"," AZ"," AR"," CA"," CO"," CT"," DE"," FL"," GA",
           " HI"," ID"," IL"," IN"," IA"," KS"," KY"," LA"," ME"," MD"," MA"," MI"," MN"," MS"," MO"," MT"," NE"," NV"," NH",
           " NJ"," NM"," NY"," NC"," ND"," OH"," OK"," OR"," PA"," RI"," SC"," SD"," TN"," TX"," UT"," VT"," VA"," WA"," WV",
           " WI"," WY")
accident[, ONSHORE_COUNTY_NAME := sapply(ONSHORE_COUNTY_NAME, function(cell) {
  for (word in temp) {
    cell <- gsub(paste0("\\b", word, "\\b"), "", cell)  # Remove exact matches
    cell <- gsub("\\s{2,}", " ", cell)                  # Remove extra spaces if any
    cell <- trimws(cell)                                # Remove leading/trailing whitespace
  }
  return(cell)
})]


# Null values can be due to a multitude of factors:
# such as being offshore, not within a county, or an error of omission
# Offshore rows can be kept, but we are unable to determine if the omission was deliberate (accidentally left out or left out because no county)
# Hence, Onshore&NA rows will be removed, similar to ONSHORE_CITY_NAME.

accident[is.na(ONSHORE_COUNTY_NAME) & ON_OFF_SHORE == 'ONSHORE',.N]          # 18 rows
accident[(is.na(ONSHORE_COUNTY_NAME) & ON_OFF_SHORE == 'ONSHORE'), ONSHORE_COUNTY_NAME := 'MISSING']    # 18 rows deleted

# For Offshore rows: Replace NA with OFFSHORE
accident[is.na(ONSHORE_COUNTY_NAME) & ON_OFF_SHORE == 'OFFSHORE', ONSHORE_COUNTY_NAME := 'OFFSHORE']

#---------------------------------------------------------------------------------------------------------------------------------
# LOCATION_TYPE 
accident[is.na(LOCATION_TYPE) & ON_OFF_SHORE == 'OFFSHORE',.N] #10 Null Values
# All incidents without Location Type are offshore.
# Choice: Replace NA with OFFSHORE
accident[is.na(LOCATION_TYPE) & ON_OFF_SHORE == 'OFFSHORE', LOCATION_TYPE := 'OFFSHORE']

#---------------------------------------------------------------------------------------------------------------------------------
# INCIDENT_AREA_TYPE 
accident[is.na(INCIDENT_AREA_TYPE) & ON_OFF_SHORE == 'OFFSHORE',.N] #10 Null Values
# All incidents without Location Type are offshore.
# Choice: Replace NA with OFFSHORE
accident[is.na(INCIDENT_AREA_TYPE) & ON_OFF_SHORE == 'OFFSHORE', INCIDENT_AREA_TYPE := 'OFFSHORE']

#---------------------------------------------------------------------------------------------------------------------------------
# INCIDENT_AREA_SUBTYPE                                    

# Extension of INCIDENT_AREA_TYPE, to be confirmed if columns need to be kept.
accident[is.na(INCIDENT_AREA_SUBTYPE),INCIDENT_AREA_SUBTYPE := INCIDENT_AREA_TYPE]
accident[is.na(INCIDENT_AREA_SUBTYPE), .N]

#View(accident)


#---------------------------------------------------------------------------------------------------------------------------------
# DEPTH_OF_COVER                                                      

# Specifically, depth of cover records how deep underground were the pipes buried underground in inches.

# Depth of cover as 0 if incident area not under soil
accident[is.na(DEPTH_OF_COVER) & !INCIDENT_AREA_TYPE == 'UNDER SOIL', DEPTH_OF_COVER := 0]
accident[is.na(DEPTH_OF_COVER), .N] # No more missing values

#---------------------------------------------------------------------------------------------------------------------------------
# SYSTEM_PART_INVOLVED

accident[, SYSTEM_PART_INVOLVED := gsub(pattern="['\"]", replacement=" ", SYSTEM_PART_INVOLVED)] # Remove quotes -- Aesthetic


# Restrict to pipeline
#---------------------------------------------------------------------------------------------------------------------------------
# COMMODITY_REACHED_HCA

# Yes if commodity released reached a High Consequence Area (HCA)
accident[is.na(COMMODITY_REACHED_HCA),]

# Predicting categorical variable --> Use Mode or Remove Row
# Decision: Remove row as statistical methods are insufficiently reliable to estimate whether
# commodities were released into a High Consequence Area. Furthermore, only 1 row will be affected.

accident <- accident[!is.na(COMMODITY_REACHED_HCA)]


#---------------------------------------------------------------------------------------------------------------------------------
# COMMODITY_SUBTYPE & COMMODITY_DETAILS   

# Extension of COMMODITY_RELEASED_TYPE

# Merge COMMODITY_SUBTYPE with COMMODITY_RELEASED_TYPE
accident[is.na(COMMODITY_SUBTYPE),COMMODITY_SUBTYPE := COMMODITY_RELEASED_TYPE]
accident[is.na(COMMODITY_SUBTYPE),.N]


unique(accident$COMMODITY_SUBTYPE) # Investigate unique values


# Investigate Missing values in COMMODITY_DETAILS
accident[COMMODITY_SUBTYPE == 'OTHER' & is.na(COMMODITY_DETAILS)]
accident[COMMODITY_SUBTYPE == 'OTHER HVL' & is.na(COMMODITY_DETAILS)]
accident[!COMMODITY_SUBTYPE == 'OTHER' & !COMMODITY_SUBTYPE == 'OTHER HVL' & !is.na(COMMODITY_DETAILS),]


# Merge COMMODITY_DETAILS with COMMODITY_SUBTYPE
accident[is.na(COMMODITY_DETAILS), COMMODITY_DETAILS := COMMODITY_SUBTYPE]
accident[is.na(COMMODITY_DETAILS),.N]


#---------------------------------------------------------------------------------------------------------------------------------
# INTENTIONAL_RELEASE_BBLS              
# From report breakdown: Estimated volume of intentional and/or controlled release/blowdown: (only reported for HVL and CO2 Commodities)

unique(accident$COMMODITY_RELEASED_TYPE) # List of all types of commodities released
accident[is.na(INTENTIONAL_RELEASE_BBLS) &  COMMODITY_RELEASED_TYPE %in% c('CO2 (CARBON DIOXIDE)',
                                                                           'HVL OR OTHER FLAMMABLE OR TOXIC FLUID WHICH IS A GAS AT AMBIENT CONDITIONS'
),.N]
# 16 rows where commodity released is CO2 or HVL but intentional release is not revealed, contrary to report requirements.
# Choice: Remove these 16 rows as they do not fit the reporting standards established.
accident <- accident[!(is.na(INTENTIONAL_RELEASE_BBLS) &  COMMODITY_RELEASED_TYPE %in% c('CO2 (CARBON DIOXIDE)',
                                                                                         'HVL OR OTHER FLAMMABLE OR TOXIC FLUID WHICH IS A GAS AT AMBIENT CONDITIONS'
)),]



accident[!is.na(INTENTIONAL_RELEASE_BBLS) &  !COMMODITY_RELEASED_TYPE %in% c('CO2 (CARBON DIOXIDE)',
                                                                             'HVL OR OTHER FLAMMABLE OR TOXIC FLUID WHICH IS A GAS AT AMBIENT CONDITIONS'
),.N]
# 669 rows where commodity released is not CO2 or HVL but intentional release is revealed, contrary to report requirements.
# Choice: Change these values to NA to fit reporting standards.
accident[!is.na(INTENTIONAL_RELEASE_BBLS) &  !COMMODITY_RELEASED_TYPE %in% c('CO2 (CARBON DIOXIDE)',
                                                                             'HVL OR OTHER FLAMMABLE OR TOXIC FLUID WHICH IS A GAS AT AMBIENT CONDITIONS'
), INTENTIONAL_RELEASE_BBLS:= NA]

# Now, all Null Values in this column are due to the fact that the commodity released is not CO2 or HVL
accident[is.na(INTENTIONAL_RELEASE_BBLS),INTENTIONAL_RELEASE_BBLS:= 0]       # Convert Null Values to -1 to indicate 'Not Applicable'

#---------------------------------------------------------------------------------------------------------------------------------
# Pipe-Related Data (PIPE_DIAMETER, PIPE_WALL_THICKNESS, PIPE_SMYS, PIPE_SPECIFICATION)

# These 4 variables are grouped together due to similar number of missing values.
# Corroborated by the following line of code.
accident[is.na(PIPE_DIAMETER) & !is.na(PIPE_WALL_THICKNESS)] # 0 cases of NA pipe diameter with pipe wall thickness


unique(accident[!is.na(PIPE_DIAMETER),SYSTEM_PART_INVOLVED])
unique(accident[is.na(PIPE_DIAMETER),SYSTEM_PART_INVOLVED])
# No visible correlation between SYSTEM_PART_INVOLVED values and Pipe-related Variables

# No cleaning to be performed as missing values appear to be random. Statistical methods
# to predict null values are not reliable as a majority of the rows have missing values for
# this column.

# As a placeholder, null values will be filled with 'missing'.
accident[is.na(PIPE_DIAMETER),PIPE_DIAMETER := -1]
accident[is.na(PIPE_WALL_THICKNESS),PIPE_WALL_THICKNESS := -1]
accident[is.na(PIPE_SMYS),PIPE_SMYS := -1]
accident[is.na(PIPE_SPECIFICATION),PIPE_SPECIFICATION := 'MISSING']

#View(accident)

# Two Options after performing EDA:
# > A -- Remove non-pipe rows and focus analysis on accidents involving pipes
# > B -- Remove these 4 variables


#---------------------------------------------------------------------------------------------------------------------------------
# INSTALLATION_YEAR 

unique(accident$INSTALLATION_YEAR) #Presence of NA values and "UNKNOWN" value
accident[is.na(INSTALLATION_YEAR) | INSTALLATION_YEAR == 'UNKNOWN', .N] # 1531 missing values

# A significant proportion of the values regarding installation year is missing.

hist(as.numeric(accident$INSTALLATION_YEAR)) # Most pipes (with tracked installation year) installed in 2010s
summary(as.numeric(accident$INSTALLATION_YEAR))

# There exists 2 possibilities.
# 1. The trend that most pipes are installed in the 2010s holds true for all pipes with missing values.
# In this case mode imputation might a viable option. However 1 is unlikely to be true for reasons
# given in 2.

# 2. The large number of records in the 2010s could point to better recordkeeping methods and
# procedures. As such, the actual installation years may not be as left-skewed as it currently is.
# Hence, mode imputation is not appropriate and a better method for approximation is required.


# Given the circumstances, the same method used in dealing with Pipe-related data can be applied again.

accident[is.na(INSTALLATION_YEAR), INSTALLATION_YEAR:= 'UNKNOWN']

# Two Options after performing EDA:
# > A -- Remove rows with missing years
# > B -- Ignore installation year as a variable

#---------------------------------------------------------------------------------------------------------------------------------
# MATERIAL_INVOLVED & MATERIAL_DETAILS 
unique(accident$MATERIAL_INVOLVED)

unique(accident$MATERIAL_DETAILS)

accident[is.na(MATERIAL_DETAILS)] #3586 NULL Values
accident[is.na(MATERIAL_INVOLVED)] #682 NULL Values

accident[is.na(MATERIAL_DETAILS) & MATERIAL_INVOLVED == "MATERIAL OTHER THAN CARBON STEEL"] #3041 NULL Values
# All Material Details are related to materials other than carbon steel

# Merge MATERIAL_DETAILS with MATERIAL_INVOLVED
accident[is.na(MATERIAL_DETAILS), MATERIAL_DETAILS := MATERIAL_INVOLVED]

# Remaining 641 rows are indeterminate. To be filled with placeholder "MISSING"
accident[is.na(MATERIAL_INVOLVED), MATERIAL_INVOLVED := 'MISSING']
accident[is.na(MATERIAL_DETAILS), MATERIAL_DETAILS:= 'MISSING']
# Two Options after performing EDA:
# > A -- Remove rows with missing information regarding material used
# > B -- Ignore material information as a variable

# In addition, MATERIAL_DETAILS contain other information not related to the exact material used
# to construct the part in question. For instance, some rows contain "COPPER" or "316 Stainless Steel",
# while others contain values such as "PUMP SEAL ASSEMBLY". The high variance of values within
# this variable makes it difficult to analyse and this column will be dropped.

#---------------------------------------------------------------------------------------------------------------------------------
#NUM_PUB_EVACUATED
summary(accident$NUM_PUB_EVACUATED) #if no public evacuation, value=0. So NA cannot be generalised into 0, probably due to omission upon filling up the form
#Choice - drop these rows
accident <- accident[!(is.na(NUM_PUB_EVACUATED))]

#---------------------------------------------------------------------------------------------------------------------------------
# Environment Indicators

accident[is.na(WILDLIFE_IMPACT_IND),WILDLIFE_IMPACT_IND:= 'NO']
accident[is.na(FISH_AQUATIC_IMPACT_IND),FISH_AQUATIC_IMPACT_IND:= 'NO']
accident[is.na(BIRDS_IMPACT_IND ),BIRDS_IMPACT_IND := 'NO']
accident[is.na(TERRESTRIAL_IMPACT_IND),TERRESTRIAL_IMPACT_IND:= 'NO']
accident[is.na(SOIL_CONTAMINATION),SOIL_CONTAMINATION:= 'NO']
accident[is.na(LONG_TERM_ASSESSMENT),LONG_TERM_ASSESSMENT:= 'NO']
accident[is.na(REMEDIATION_IND),REMEDIATION_IND:= 'NO']
accident[is.na(SURFACE_WATER_REMED_IND),SURFACE_WATER_REMED_IND:= 'NO']
accident[is.na(GROUNDWATER_REMED_IND),GROUNDWATER_REMED_IND:= 'NO']
accident[is.na(SOIL_REMED_IND),SOIL_REMED_IND:= 'NO']
accident[is.na(VEGETATION_REMED_IND),VEGETATION_REMED_IND:= 'NO']
accident[is.na(WILDLIFE_REMED_IND),WILDLIFE_REMED_IND:= 'NO']
accident[is.na(OCEAN_SEAWATER_IND),OCEAN_SEAWATER_IND:= 'NO']
accident[is.na(SURFACE_CONTAM_IND),SURFACE_CONTAM_IND:= 'NO']
accident[is.na(GROUNDWATER_CONTAM_IND),GROUNDWATER_CONTAM_IND:= 'NO']
accident[is.na(DRINKING_WATER_CONTAM_IND),DRINKING_WATER_CONTAM_IND:= 'NO']
accident[is.na(PRIVATE_WELL_CONTAM_IND),PRIVATE_WELL_CONTAM_IND:= 'NO']
accident[is.na(PUBLIC_WATER_CONTAM_IND),PUBLIC_WATER_CONTAM_IND:= 'NO']



#---------------------------------------------------------------------------------------------------------------------------------
##Injuries Data
#NUM_EMP_INJURIES 
accident[INJURY_IND=="YES" & is.na(NUM_EMP_INJURIES),]
accident[INJURY_IND=="NO" & !is.na(NUM_EMP_INJURIES),] #Since NA values belong to accidents with NO injury, it can be mapped to 0. This logic applies to other injuries count columns as well
accident$NUM_EMP_INJURIES[is.na(accident$NUM_EMP_INJURIES)] <- 0 #inplace replacement of NA with 0

#NUM_CONTR_INJURIES
accident[INJURY_IND=="YES" & is.na(NUM_CONTR_INJURIES),]
accident[INJURY_IND=="NO" & !is.na(NUM_CONTR_INJURIES),] #same logic
accident$NUM_CONTR_INJURIES[is.na(accident$NUM_CONTR_INJURIES)] <- 0

#NUM_ER_INJURIES 
accident[INJURY_IND=="YES" & is.na(NUM_ER_INJURIES),]
accident[INJURY_IND=="NO" & !is.na(NUM_ER_INJURIES),] #same logic
accident$NUM_ER_INJURIES[is.na(accident$NUM_ER_INJURIES)] <- 0

#NUM_WORKER_INJURIES 
accident[INJURY_IND=="YES" & is.na(NUM_WORKER_INJURIES),]
accident[INJURY_IND=="NO" & !is.na(NUM_WORKER_INJURIES),] #same logic
accident$NUM_WORKER_INJURIES[is.na(accident$NUM_WORKER_INJURIES)] <- 0

#NUM_GP_INJURIES 
accident[INJURY_IND=="YES" & is.na(NUM_GP_INJURIES),]
accident[INJURY_IND=="NO" & !is.na(NUM_GP_INJURIES),] #same logic
accident$NUM_GP_INJURIES[is.na(accident$NUM_GP_INJURIES)] <- 0

#---------------------------------------------------------------------------------------------------------------------------------
##Fatalities
#NUM_EMP_FATALITIES
accident[FATALITY_IND=="YES" & is.na(NUM_EMP_FATALITIES),] 
accident[FATALITY_IND=="NO" & !is.na(NUM_EMP_FATALITIES),] #Since NA values belong to accidents with NO fatality, it can be mapped to 0. This logic applies to other fatalities count columns as well
accident$NUM_EMP_FATALITIES[is.na(accident$NUM_EMP_FATALITIES)] <- 0

#NUM_CONTR_FATALITIES
accident[FATALITY_IND=="YES" & is.na(NUM_CONTR_FATALITIES),] 
accident[FATALITY_IND=="NO" & !is.na(NUM_CONTR_FATALITIES),] #same logic
accident$NUM_CONTR_FATALITIES[is.na(accident$NUM_CONTR_FATALITIES)] <- 0

#NUM_ER_FATALITIES
accident[FATALITY_IND=="YES" & is.na(NUM_ER_FATALITIES),] 
accident[FATALITY_IND=="NO" & !is.na(NUM_ER_FATALITIES),] #same logic
accident$NUM_ER_FATALITIES[is.na(accident$NUM_ER_FATALITIES)] <- 0

#NUM_WORKER_FATALITIES
accident[FATALITY_IND=="YES" & is.na(NUM_WORKER_FATALITIES),] 
accident[FATALITY_IND=="NO" & !is.na(NUM_WORKER_FATALITIES),] #same logic
accident$NUM_WORKER_FATALITIES[is.na(accident$NUM_WORKER_FATALITIES)] <- 0

#NUM_GP_FATALITIES
accident[FATALITY_IND=="YES" & is.na(NUM_GP_FATALITIES),] 
accident[FATALITY_IND=="NO" & !is.na(NUM_GP_FATALITIES),] #same logic
accident$NUM_GP_FATALITIES[is.na(accident$NUM_GP_FATALITIES)] <- 0

#---------------------------------------------------------------------------------------------------------------------------------
#EXPLODE_IND
#if there is NA, we would take that there is no explosion due to accident because unlike cost,this answer is definite and should be available to investigators
accident$EXPLODE_IND[is.na(accident$EXPLODE_IND)] <- "NO"

#---------------------------------------------------------------------------------------------------------------------------------
#SHUTDOWN_DUE_ACCIDENT_IND
#if there is NA, we would take that there is no shutdown due to accident because unlike cost, this answer is definite and should be available to investigators
#In addition, there is also no shutdown time and restart time provided for the same record.
accident$SHUTDOWN_DUE_ACCIDENT_IND[is.na(accident$SHUTDOWN_DUE_ACCIDENT_IND)] <- "NO"

#---------------------------------------------------------------------------------------------------------------------------------
#STILL_SHUTDOWN_IND
accident$STILL_SHUTDOWN_IND[is.na(accident$STILL_SHUTDOWN_IND)] <- "NO"

#---------------------------------------------------------------------------------------------------------------------------------
#SHUTDOWN_DATETIME
accident[SHUTDOWN_DUE_ACCIDENT_IND=="NO" & !is.na(accident$SHUTDOWN_DATETIME),] #NA is only for records with no shutdown, we will not clean this for now and will do it later when we create new duration columns

#---------------------------------------------------------------------------------------------------------------------------------
#RESTART_DATETIME
accident[SHUTDOWN_DUE_ACCIDENT_IND=="NO" & !is.na(accident$RESTART_DATETIME),] #NA is only for records with no shutdown, we will not clean this for now and will do it later when we create new duration columns

###For these 2 date and time, rows with NA should be dropped because these data cannot be estimated and are specific to the event. In addition, the number of rows with NA is small.
#INCIDENT_IDENTIFIED_DATETIME
accident[is.na(INCIDENT_IDENTIFIED_DATETIME),.N]    #29 NA
#ON_SITE_DATETIME
accident[is.na(ON_SITE_DATETIME),.N]  #32 NA

accident <- subset(accident, !is.na(accident$INCIDENT_IDENTIFIED_DATETIME) & !is.na(accident$ON_SITE_DATETIME))
dim(accident)

#---------------------------------------------------------------------------------------------------------------------------------
##Costs
### For the cost attributes below, the NA values are probably due to omission or not able to estimate the cost figures upon filling up the form
### However, we could not make the assumption that NA means there is no cost for that particular category because min value of these attribute are 0
### i.e., investigators are supposed to fill in 0 if there is no cost incurred for that particular category. In addition, the number of rows with NA 
### values are also insignificant as compared to the dimension of the dataset, so dropping these rows directly seems to be the most reasonable approach. 

#EST_COST_OPER_PAID
summary(accident$EST_COST_OPER_PAID)

#EST_COST_GAS_RELEASED
summary(accident$EST_COST_GAS_RELEASED)

#EST_COST_PROP_DAMAGE 
summary(accident$EST_COST_PROP_DAMAGE)

#EST_COST_EMERGENCY
summary(accident$EST_COST_EMERGENCY)

#EST_COST_ENVIRONMENTAL 
summary(accident$EST_COST_ENVIRONMENTAL)

#EST_COST_OTHER
summary(accident$EST_COST_OTHER)

##Drop records with NA
accident <- subset(accident, !is.na(accident$EST_COST_OPER_PAID) & !is.na(accident$EST_COST_GAS_RELEASED) & !is.na(accident$EST_COST_PROP_DAMAGE)
                   & !is.na(accident$EST_COST_EMERGENCY) & !is.na(accident$EST_COST_ENVIRONMENTAL) & !is.na(accident$EST_COST_OTHER))

##Check again after cleaning
colSums(is.na(accident)) 
dim(accident)

## Note: for other rows with NA, we would not clean them first because we are not sure it is going to be used for model building


# ================================================================
# Further Data Processing: transformation of existing columns
#=================================================================
#1. Remove quotations
accident[, NAME := gsub(pattern="['\"]", replacement="", NAME)] 
accident[, PIPE_FAC_NAME := gsub(pattern="['\"]", replacement="", PIPE_FAC_NAME)]
accident[, ONSHORE_COUNTY_NAME := gsub(pattern="['\"]", replacement="", ONSHORE_COUNTY_NAME)]
accident[, LOCATION_TYPE := gsub(pattern="['\"]", replacement="", LOCATION_TYPE)]
accident[, INCIDENT_AREA_SUBTYPE := gsub(pattern="['\"]", replacement="", INCIDENT_AREA_SUBTYPE)]
accident[, SYSTEM_PART_INVOLVED := gsub(pattern="['\"]", replacement="", SYSTEM_PART_INVOLVED)]
accident[, INCIDENT_AREA_TYPE := gsub(pattern="['\"]", replacement="", INCIDENT_AREA_TYPE)]
accident[, CAUSE_DETAILS := gsub(pattern="['\"]", replacement="", CAUSE_DETAILS)]
accident[, COMMODITY_SUBTYPE := gsub(pattern="['\"]", replacement="", COMMODITY_SUBTYPE)]
accident[, COMMODITY_DETAILS := gsub(pattern="['\"]", replacement="", COMMODITY_DETAILS)]



#2. Standardising Date/Time columns
# e.g., Shutdown Date/Time contains both "1/11/10 14:45" and 1/15/2010 15:10" format
standardize_date_time <- function(date_time_str) {
  parts <- unlist(strsplit(date_time_str, " "))
  date_parts <- unlist(strsplit(parts[1], "/"))
  
  if (length(date_parts) == 3) {
    # Assume four-digit years are complete
    date_format <- ifelse(nchar(date_parts[3]) == 2, "%m/%d/%y", "%m/%d/%Y")
    formatted_date_time <- as.POSIXct(date_time_str, format = paste0(date_format, " %H:%M"))
  } else {
    # Handle incomplete date or time
    formatted_date_time <- NA
  }
  
  return(formatted_date_time)
}

# Apply the function to standardize the date and time formats
accident[, shutdownDTTM:= sapply(accident$SHUTDOWN_DATETIME, standardize_date_time)]
accident[, restartDTTM:= sapply(accident$RESTART_DATETIME, standardize_date_time)]
accident[, incidentDTTM:= sapply(accident$INCIDENT_IDENTIFIED_DATETIME, standardize_date_time)]
accident[, onsiteDTTM:= sapply(accident$ON_SITE_DATETIME, standardize_date_time)]
#View(accident)

#check for NAs in the date columns
summary(accident$shutdownDTTM) #1991 NAs
summary(accident$restartDTTM) #1998 NAs
#the difference between the number of NAs is due to some facilities are still shutdown till today, i.e., there is no restart date
length(which(is.na(accident$restartDTTM)&accident$STILL_SHUTDOWN_IND=="YES"))  #7 permanently shutdown

#check whether Date and Time make sense
accident[shutdownDTTM>restartDTTM,] #check if there is any shutdown time that is later than restart time

accident[incidentDTTM>onsiteDTTM,] #check if there is any incident time that is later than onsite time: #103 records
#This does not make sense, onsite time should be always later than incident time: drop these rows
accident<-subset(accident,incidentDTTM<=onsiteDTTM)


# ================================================================
# Further Data Processing: creation of new columns
#=================================================================
#1. Evacuation indicators
accident[, EVACUATION:= ifelse(NUM_PUB_EVACUATED== 0, "NO", "YES")]


#2. standardized_shudownDTTM, standardized_restartDTTM and shutdown_duration
accident[, standardized_shudownDTTM:= ifelse(is.na(accident$shutdownDTTM), 0, as.POSIXct(shutdownDTTM, origin="1970-01-01 00:00"))]
accident[, standardized_restartDTTM:= ifelse(is.na(accident$restartDTTM), 0, as.POSIXct(restartDTTM, origin="1970-01-01 00:00"))]

#Logic: if there is no shutdown, shutdown duration is 0
#       else if there is shutdown but still shutdown, shutdown duration is NaN
#       else shutdown duration will be the difference between shutdownDTTM and restartDTTM
accident[, shutdown_duration:= ifelse((as.POSIXct(standardized_shudownDTTM, origin="1970-01-01 00:00")==0 & as.POSIXct(standardized_restartDTTM, origin="1970-01-01 00:00")==0), 0, 
                                      ifelse((as.POSIXct(standardized_restartDTTM, origin="1970-01-01 00:00")==0 &STILL_SHUTDOWN_IND=="YES"),NaN,
                                             difftime(as.POSIXct(standardized_restartDTTM, origin="1970-01-01 00:00"), as.POSIXct(standardized_shudownDTTM, origin="1970-01-01 00:00"), units="hours")))]


#3. Response_Delay: incidentDTTM-onsiteDTTM
accident[, response_delay:= as.numeric(difftime(as.POSIXct(onsiteDTTM, origin="1970-01-01 00:00"), as.POSIXct(incidentDTTM, origin="1970-01-01 00:00"), units="hours"))]
summary(accident$response_delay)


#4. Total Cost in $
accident[, TOTAL_COST:= rowSums(accident[ , c("EST_COST_OPER_PAID","EST_COST_GAS_RELEASED","EST_COST_PROP_DAMAGE",
                                              "EST_COST_EMERGENCY","EST_COST_ENVIRONMENTAL","EST_COST_OTHER")])]





# ================================================================
# Final Check 
#=================================================================
view(dfSummary(accident))
dim(accident)

#Please output the file as this originial dataframe will be read in again for model building later
store_path <-getwd()
write.csv(accident, paste(store_path, "/accident_clean.csv",sep=""), row.names=FALSE)

#---------------------------------------End of Phase 1-------------------------------------------



# ++=================++
# ||   Phase 2: EDA  ||
# ++=================++


# ================================================================
# EDA: Check for Outliers
#=================================================================
summary(accident$TOTAL_COST) #MAX is 21819684

#Check for outliers
#function to count the number of outliers
count_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], na.rm = TRUE, probs = c(0.25, 0.75))
  IQR_val <- IQR(data[[column_name]],na.rm=TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_val #Lower whisker
  upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker
  
  outliers_count <- sum(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, na.rm = TRUE) #IQR Method to identify outliers
  
  return(outliers_count)
}

ggplot(accident,aes(x=factor(0),TOTAL_COST))+geom_boxplot() #from the boxplot, there are indeed a lot of outliers which skewed the data significantly
count_outliers(accident,"TOTAL_COST") #566 outliers


# ================================================================
# EDA: Removal of Extreme Values
# To avoid skewness in our data, we decide to omit extremely high values in our EDA phase. 
#=================================================================
# Set a Z-score threshold
z_threshold <- 3 


# Calculate Z-scores for the dependent variable
accident$Zscore <- scale(accident$TOTAL_COST)
# Identify outliers based on the Z-score
outliers <- accident[accident$Zscore > z_threshold | accident$Zscore < -z_threshold, ]
accident_without_extreme_values <- accident[!(accident$Zscore > z_threshold | accident$Zscore < -z_threshold), ] #outliers/extreme values are removed

#Check number of outliers removed
dim(outliers) #21 rows are deleted
dim(accident_without_extreme_values)

accident <- accident_without_extreme_values #overwrite accident to exclude extreme values

#to check
view(dfSummary(accident)) #3891 rows -> 3870 rows 



#================================================================
# EDA: Categorise these variables by ranges for more meaningful data exploration and visualisation
#=================================================================
#Convert continuous variable to categorical based on interquartile ranges with jitter since labels are not unique
convert_to_category_jitter <- function(data, var_name) {
  data[[var_name]][data[[var_name]] == "UNKNOWN"] <- NA
  data[[paste0(var_name, "_numeric")]] <- as.numeric(data[[var_name]])
  
  #Jitter the numeric values
  set.seed(123)
  data[[paste0(var_name, "_jittered")]] <- jitter(data[[paste0(var_name, "_numeric")]], amount = 0.1)
  
  q1 <- quantile(data[[paste0(var_name, "_jittered")]], 0.25, na.rm = TRUE)
  q2 <- quantile(data[[paste0(var_name, "_jittered")]], 0.5, na.rm = TRUE)
  q3 <- quantile(data[[paste0(var_name, "_jittered")]], 0.75, na.rm = TRUE)
  
  labels <- c("(-Inf, Q1]", "(Q1, Q2]", "(Q2, Q3]", "(Q3, Inf)")
  breaks <- c(-Inf, q1, q2, q3, Inf)
  
  data[[paste0(var_name, "_category")]] <- cut(data[[paste0(var_name, "_jittered")]], 
                                               breaks = breaks, 
                                               labels = labels, 
                                               include.lowest = TRUE)
  
  data[[paste0(var_name, "_category")]] <- as.factor(data[[paste0(var_name, "_category")]])
  levels(data[[paste0(var_name, "_category")]]) <- c(levels(data[[paste0(var_name, "_category")]]), "UNKNOWN")
  data[[paste0(var_name, "_category")]][is.na(data[[paste0(var_name, "_numeric")]])] <- "UNKNOWN"
  
  return(data)
}

#Convert variables to categories with jitter
vars_to_convert_jitter <- c("DEPTH_OF_COVER","PIPE_DIAMETER", "PIPE_WALL_THICKNESS", "PIPE_SMYS", "INSTALLATION_YEAR")
for (var in vars_to_convert_jitter) {
  accident <- convert_to_category_jitter(accident, var)
}

#Print results
head(accident$DEPTH_OF_COVER_category)
head(accident$PIPE_DIAMETER_category)
head(accident$PIPE_WALL_THICKNESS_category)
head(accident$PIPE_SMYS_category)
head(accident$INSTALLATION_YEAR_category)

# ================================================================
# Data cleaning: convert to nominal categorical var
#=================================================================
accident$`NAME` <- factor(accident$`NAME`)
accident$`PIPE_FAC_NAME` <- factor(accident$`PIPE_FAC_NAME`)
accident$`ONSHORE_STATE_ABBREVIATION` <- factor(accident$`ONSHORE_STATE_ABBREVIATION`)
accident$`ONSHORE_CITY_NAME` <- factor(accident$`ONSHORE_CITY_NAME`)
accident$`ONSHORE_COUNTY_NAME` <- factor(accident$`ONSHORE_COUNTY_NAME`)
accident$ON_OFF_SHORE <- factor(accident$ON_OFF_SHORE)
accident$LOCATION_TYPE <- factor(accident$LOCATION_TYPE)
accident$INCIDENT_AREA_TYPE <- factor(accident$INCIDENT_AREA_TYPE)
accident$INCIDENT_AREA_SUBTYPE <- factor(accident$INCIDENT_AREA_SUBTYPE)
accident$`DEPTH_OF_COVER` <- factor(accident$`DEPTH_OF_COVER`)
accident$SYSTEM_PART_INVOLVED <- factor(accident$SYSTEM_PART_INVOLVED)
accident$COULD_BE_HCA <- factor(accident$COULD_BE_HCA)
accident$COMMODITY_REACHED_HCA <- factor(accident$COMMODITY_REACHED_HCA)
accident$CAUSE <- factor(accident$CAUSE)
accident$`CAUSE_DETAILS` <- factor(accident$`CAUSE_DETAILS`)
accident$COMMODITY_RELEASED_TYPE <- factor(accident$COMMODITY_RELEASED_TYPE)
accident$COMMODITY_SUBTYPE <- factor(accident$COMMODITY_SUBTYPE)
accident$`COMMODITY_DETAILS` <- factor(accident$`COMMODITY_DETAILS`) 
accident$`PIPE_DIAMETER` <- factor(accident$`PIPE_DIAMETER`) 
accident$`PIPE_WALL_THICKNESS` <- factor(accident$`PIPE_WALL_THICKNESS`) 
accident$`PIPE_SMYS` <- factor(accident$`PIPE_SMYS`) 
accident$`PIPE_SPECIFICATION` <- factor(accident$`PIPE_SPECIFICATION`) 
accident$`INSTALLATION_YEAR` <- factor(accident$`INSTALLATION_YEAR`)
accident$MATERIAL_INVOLVED <- factor(accident$MATERIAL_INVOLVED)
accident$MATERIAL_DETAILS <- factor(accident$MATERIAL_DETAILS)
accident$RELEASE_TYPE <- factor(accident$RELEASE_TYPE)
accident$IGNITE_IND <- factor(accident$IGNITE_IND)
accident$EXPLODE_IND <- factor(accident$EXPLODE_IND)
accident$SHUTDOWN_DUE_ACCIDENT_IND <- factor(accident$SHUTDOWN_DUE_ACCIDENT_IND)
accident$STILL_SHUTDOWN_IND <- factor(accident$STILL_SHUTDOWN_IND)
accident$INJURY_IND <- factor(accident$INJURY_IND)
accident$FATALITY_IND <- factor(accident$FATALITY_IND)
accident$WILDLIFE_IMPACT_IND <- factor(accident$WILDLIFE_IMPACT_IND) 
accident$FISH_AQUATIC_IMPACT_IND <- factor(accident$FISH_AQUATIC_IMPACT_IND) 
accident$BIRDS_IMPACT_IND <- factor(accident$BIRDS_IMPACT_IND) 
accident$TERRESTRIAL_IMPACT_IND <- factor(accident$TERRESTRIAL_IMPACT_IND) 
accident$SOIL_CONTAMINATION <- factor(accident$SOIL_CONTAMINATION) 
accident$LONG_TERM_ASSESSMENT <- factor(accident$LONG_TERM_ASSESSMENT) 
accident$REMEDIATION_IND <- factor(accident$REMEDIATION_IND) 
accident$SURFACE_WATER_REMED_IND <- factor(accident$SURFACE_WATER_REMED_IND) 
accident$GROUNDWATER_REMED_IND <- factor(accident$GROUNDWATER_REMED_IND) 
accident$SOIL_REMED_IND <- factor(accident$SOIL_REMED_IND) 
accident$VEGETATION_REMED_IND <- factor(accident$VEGETATION_REMED_IND) 
accident$WILDLIFE_REMED_IND <- factor(accident$WILDLIFE_REMED_IND) 
accident$WATER_CONTAM_IND <- factor(accident$WATER_CONTAM_IND) 
accident$OCEAN_SEAWATER_IND <- factor(accident$OCEAN_SEAWATER_IND) 
accident$SURFACE_CONTAM_IND <- factor(accident$SURFACE_CONTAM_IND) 
accident$GROUNDWATER_CONTAM_IND <- factor(accident$GROUNDWATER_CONTAM_IND) 
accident$DRINKING_WATER_CONTAM_IND <- factor(accident$DRINKING_WATER_CONTAM_IND) 
accident$PRIVATE_WELL_CONTAM_IND <- factor(accident$PRIVATE_WELL_CONTAM_IND) 
accident$PUBLIC_WATER_CONTAM_IND <- factor(accident$PUBLIC_WATER_CONTAM_IND) 
accident$EVACUATION <- factor(accident$EVACUATION)

view(dfSummary(accident))

# ================================================================
#Uni-variate EDA 
#=================================================================
# Visualise All Cost and Cost Breakdowns
ggplot(accident, aes(x = TOTAL_COST)) + geom_boxplot() +
  coord_cartesian(xlim = quantile(accident$TOTAL_COST, c(0.1, 0.9))) + 
  labs(x = "Total Cost", title = "Cost of accidents") +
  theme(plot.title = element_text(hjust = 0.5))

#Use when cost >0 as most values are 0 and boxplot is not conclusive
ggplot(accident[accident$EST_COST_OPER_PAID > 0], aes(x = EST_COST_OPER_PAID)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_OPER_PAID[accident$EST_COST_OPER_PAID > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Cost of accidents in operator payment") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$EST_COST_GAS_RELEASED > 0], aes(x = EST_COST_GAS_RELEASED)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_GAS_RELEASED[accident$EST_COST_GAS_RELEASED > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Cost of accidents in gas released") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$EST_COST_PROP_DAMAGE > 0], aes(x = EST_COST_PROP_DAMAGE)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_PROP_DAMAGE[accident$EST_COST_PROP_DAMAGE > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Cost of accidents in property damage") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$EST_COST_EMERGENCY > 0], aes(x = EST_COST_EMERGENCY)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_EMERGENCY[accident$EST_COST_EMERGENCY > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Cost of accidents in emergency fees") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$EST_COST_ENVIRONMENTAL > 0], aes(x = EST_COST_ENVIRONMENTAL)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_ENVIRONMENTAL[accident$EST_COST_ENVIRONMENTAL > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Environment cost of accidents") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$EST_COST_OTHER > 0], aes(x = EST_COST_OTHER)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$EST_COST_OTHER[accident$EST_COST_OTHER > 0], c(0.1, 0.9))) + 
  labs(x = "Cost", title = "Other cost of accidents") +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------

# Count of accident against accident year
ggplot(accident, aes(x = `IYEAR`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Accident Year", y = "Number of accidents", title = "No. of accidents by Year") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------

# Count of accident against pipeline location and type
ggplot(accident, aes(x = `ON_OFF_SHORE`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Pipeline Type") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `ONSHORE_STATE_ABBREVIATION`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "State", y = "Number of accidents", title = "No. of accidents by Pipeline Location (State)") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 3) + 
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `LOCATION_TYPE`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Location Type") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size = 3) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#-------------------------------------------------------------------------------------------------------

# Count of accident against incident area type and subtype
ggplot(accident, aes(x = `INCIDENT_AREA_TYPE`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Area Type") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.4) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

ggplot(accident, aes(x = `INCIDENT_AREA_SUBTYPE`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Area Subtype") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size=3) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=7)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))

#-------------------------------------------------------------------------------------------------------

# Count of accident against system part involved in accident
ggplot(accident, aes(x = `SYSTEM_PART_INVOLVED`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by System Part Involved") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size=3) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=7)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))

#-------------------------------------------------------------------------------------------------------
# Count of accidents relating to high consequence areas (HCA)

ggplot(accident, aes(x = `COULD_BE_HCA`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Could be HCA", y = "Number of accidents", title = "No. of accidents which could be in HCA") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `COMMODITY_REACHED_HCA`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Reached HCA", y = "Number of accidents", title = "No. of accidents commodity reached HCA") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------

# Count of accident against Cause
ggplot(accident, aes(x = `CAUSE`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Cause") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size=3) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=7)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

#-------------------------------------------------------------------------------------------------------

# Count of accident against commodity released type and subtype
ggplot(accident, aes(x = `COMMODITY_RELEASED_TYPE`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Type", y = "Number of accidents", title = "No. of accidents by Commodity Released Type") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size=3) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=7)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))  

ggplot(accident, aes(x = `COMMODITY_SUBTYPE`)) + geom_bar(fill = "darkseagreen") + coord_flip() +
  labs(x = "Subtype", y = "Number of accidents", title = "No. of accidents by Commodity Released Subtype") +
  geom_text(stat='count', aes(label=..count..), angle = -90, vjust=-0.5, size=3) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=7)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15), na.translate = FALSE) #remove NA 

#-------------------------------------------------------------------------------------------------------
# Count of accident against Pipe Details
ggplot(accident, aes(x = `MATERIAL_INVOLVED`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Accident Year", y = "Number of accidents", title = "No. of accidents by Pipe Material") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Accidents with relation to pipe details
ggplot(accident, aes(x = DEPTH_OF_COVER_category)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Depth", title = "Depth of cover") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$PIPE_DIAMETER != -1], aes(x = as.numeric(PIPE_DIAMETER))) + geom_boxplot() +
  labs(x = "Diameter", title = "Pipe Diameter") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$PIPE_WALL_THICKNESS != -1], aes(x = as.numeric(PIPE_WALL_THICKNESS))) + geom_boxplot() +
  labs(x = "Thickness", title = "Pipe Wall Thickness") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$PIPE_SMYS != -1 & accident$PIPE_SMYS != "UNKNOWN"], aes(x = as.numeric(PIPE_SMYS))) + geom_boxplot() +
  labs(x = "SMYS", title = "Pipe Specified Minimum Yield Strength") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$INSTALLATION_YEAR != "UNKNOWN"], aes(x = as.numeric(INSTALLATION_YEAR))) + 
  geom_freqpoly() + 
  labs(x = "Year", y = "Number of pipes installed", title = "Pipe Installation Year") +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------

# Count of accident against release type
ggplot(accident, aes(x = `RELEASE_TYPE`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Accident Year", y = "Number of accidents", title = "No. of accidents by Release Type") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------------------------------------------

# Count of accident that has ignition and explosion
ggplot(accident, aes(x = `IGNITE_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Ignition?", y = "Number of accidents", title = "No. of accidents with and without ignition") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `EXPLODE_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Explosion", y = "Number of accidents", title = "No. of accidents with and without explosion") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------------------------------------------

# Spread of loss / recovery by barrels 
ggplot(accident[accident$UNINTENTIONAL_RELEASE_BBLS >0], aes(x = `UNINTENTIONAL_RELEASE_BBLS`)) + geom_boxplot() +
  coord_cartesian(xlim = quantile(as.numeric(accident$UNINTENTIONAL_RELEASE_BBLS[accident$UNINTENTIONAL_RELEASE_BBLS > 0]), c(0, 0.9))) + 
  labs(x = "Number of Barrels", title = "Unintentional loss by barrels") + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident[accident$INTENTIONAL_RELEASE_BBLS >0], aes(x = `INTENTIONAL_RELEASE_BBLS`)) + geom_boxplot() + 
  coord_cartesian(xlim = quantile(as.numeric(accident$INTENTIONAL_RELEASE_BBLS[accident$INTENTIONAL_RELEASE_BBLS > 0]), c(0, 0.9))) + 
  labs(x = "Number of Barrels", title = "Intentional loss by barrels") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `RECOVERED_BBLS`)) + geom_boxplot() + 
  coord_cartesian(xlim = quantile(accident$RECOVERED_BBLS, c(0, 0.9))) + 
  labs(x = "Number of Barrels", title = "Recovered loss by barrels") + 
  theme(plot.title = element_text(hjust = 0.5))

#----------------------------------------------------------------------------------------------------
# Count of accidents with evacuation
ggplot(accident, aes(x = `EVACUATION`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Evacuation?", y = "Number of accidents", title = "No. of accidents with evacuation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

# Spread of evacuation numbers
ggplot(accident, aes(x = `NUM_PUB_EVACUATED`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of People Evacuation", title = "Public Evacuation Numbers") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#----------------------------------------------------------------------------------------------------
# Count of accidents with injuries
ggplot(accident, aes(x = `INJURY_IND`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Injuries", y = "Number of accidents", title = "No. of accidents with injuries") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5))

# Spread of number of injuries
ggplot(accident, aes(x = `NUM_EMP_INJURIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of employees injured") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 1, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_CONTR_INJURIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of contractors injured") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_ER_INJURIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of emergency room injured") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_WORKER_INJURIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of workers injured") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_GP_INJURIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of general public injured") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `INJURE`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of total injuries") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Count of accidents with fatalities
ggplot(accident, aes(x = `FATALITY_IND`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Fatalities", y = "Number of accidents", title = "No. of accidents with fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5))

# Spread of number of fatalities
ggplot(accident, aes(x = `NUM_EMP_FATALITIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of fatalities", y = "Count", title = "No. of employees fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 1, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_CONTR_FATALITIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of fatalities", y = "Count", title = "No. of contractors fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_ER_FATALITIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of fatalities", y = "Count", title = "No. of emergency room fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_WORKER_FATALITIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of fatalities", y = "Count", title = "No. of workers fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `NUM_GP_FATALITIES`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of fatalities", y = "Count", title = "No. of general public fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `FATAL`)) + geom_bar(fill = "darkseagreen") +
  labs(x = "Number of injuries", y = "Count", title = "No. of total fatalities") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Count of accident that has pipeline shutdown
ggplot(accident, aes(x = `SHUTDOWN_DUE_ACCIDENT_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Pipeline Shutdown?", y = "Number of accidents", title = "No. of accidents with or without Pipeline Shutdown") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

# Count of accident that has pipeline still shutdown
ggplot(accident, aes(x = `STILL_SHUTDOWN_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Pipeline Still Shutdown?", y = "Number of accidents", title = "No. of accidents with Pipeline still shutdown") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

# Spread of accident shutdown duration
ggplot(accident[is.na(accident$shutdown_duration) == FALSE], aes(x = shutdown_duration)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$shutdown_duration[is.na(accident$shutdown_duration) == FALSE], c(0, 0.85))) + 
  labs(x = "Shutdown Duration", title = "Duration of shutdown due to accident") +
  theme(plot.title = element_text(hjust = 0.5))

# Spread of accident response delay
ggplot(accident, aes(x = response_delay)) + geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = quantile(accident$response_delay, c(0, 0.9))) + 
  labs(x = "Delay", title = "Response Delay") +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Count of accident with environmental impacts
ggplot(accident, aes(x = `WILDLIFE_IMPACT_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Impact Wildlife?", y = "Number of accidents", title = "No. of accidents impacting Wildlife") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `FISH_AQUATIC_IMPACT_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Impact Aquatic Life?", y = "Number of accidents", title = "No. of accidents impacting Aquatic Life") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `BIRDS_IMPACT_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Impact Birds?", y = "Number of accidents", title = "No. of accidents impacting Birds") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `TERRESTRIAL_IMPACT_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Impact Terrain?", y = "Number of accidents", title = "No. of accidents impacting Terrain") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `SOIL_CONTAMINATION`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Soil Contamination?", y = "Number of accidents", title = "No. of accidents contaminating soil") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `LONG_TERM_ASSESSMENT`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Long-term Impact?", y = "Number of accidents", title = "No. of accidents with long-term impacts") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Count of accidents with remediation
ggplot(accident, aes(x = `REMEDIATION_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `SURFACE_WATER_REMED_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with surface water remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `GROUNDWATER_REMED_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with ground water remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `SOIL_REMED_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with soil remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `VEGETATION_REMED_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with vegetation remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `WILDLIFE_REMED_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Remediation?", y = "Number of accidents", title = "No. of accidents with wildlife remediation") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------------------
# Count of accidents with water contamination
ggplot(accident, aes(x = `WATER_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with water contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `OCEAN_SEAWATER_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with ocean seawater contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `SURFACE_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with surface water contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `GROUNDWATER_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with ground water contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `DRINKING_WATER_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with drinking water contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `PRIVATE_WELL_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with private well contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident, aes(x = `PUBLIC_WATER_CONTAM_IND`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Contamination?", y = "Number of accidents", title = "No. of accidents with public water contamination") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))



# ================================================================
#Bi-variate EDA 
##Response variable: TOTAL_COST
##Predictor variable: Categorical
#=================================================================

##Average total cost against year
ggplot(accident, aes(x=`TOTAL_COST`, y=as.factor(`IYEAR`))) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Accident Year",
       x="Average Cost",
       y="Accident Year")

#Breakdown of different costs out of total cost over the years
long_data <- accident %>%
  select(`IYEAR`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -`IYEAR`)

summed_data <- long_data %>%
  group_by(`IYEAR`, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

color_values <- c("EST_COST_OTHER" = "plum1", 
                  "EST_COST_ENVIRONMENTAL" = "pink", 
                  "EST_COST_EMERGENCY" = "indianred1", 
                  "EST_COST_PROP_DAMAGE" = "lightgoldenrod1", 
                  "EST_COST_GAS_RELEASED" = "lightgreen", 
                  "EST_COST_OPER_PAID" = "lightskyblue")

ggplot(summed_data, aes(x = `IYEAR`, y = Total_Cost, color = Cost_Type, group = Cost_Type, fill = Cost_Type)) +
  geom_area(alpha = 0.3) +
  labs(title = "Breakdown of Different Costs Over the Years", x = "Year", y = "Total Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values)

ggplot(summed_data, aes(x = IYEAR, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_values) +
  labs(title = "Breakdown of Different Costs Over the Years", 
       x = "Year", 
       y = "Total Cost") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#PEOPLE FACTORS -----------------------------------------------------------------------------------------------

##Average total cost against Injury Indicator 
ggplot(accident, aes(x=reorder(`INJURY_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Injury Indicator",
       x="Injury Indicator",
       y="Average Cost")

##Average total cost against Fatality Indicator 
ggplot(accident, aes(x=reorder(`FATALITY_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Fatality Indicator",
       x="Fatality Indicator",
       y="Average Cost")

##Average total cost against Evacuation Indicator 
ggplot(accident, aes(x=reorder(`EVACUATION`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Evacuation Indicator",
       x="Evacuation Indicator",
       y="Average Cost")

##Average total cost against Surface Water Remediation Indicator 
ggplot(accident, aes(x=reorder(`SURFACE_WATER_REMED_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Surface Water Remediation Indicator",
       x="Surface Water Remediation Indicator",
       y="Average Cost")

##Average total cost against Groundwater Remediation Indicator 
ggplot(accident, aes(x=reorder(`GROUNDWATER_REMED_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Groundwater Remediation Indicator",
       x="Groundwater Remediation Indicator",
       y="Average Cost")

##Average total cost against Water Contamination Indicator 
ggplot(accident, aes(x=reorder(`WATER_CONTAM_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Water Contamination Indicator",
       x="Water Contamination Indicator",
       y="Average Cost")

##Average total cost against Surface Contamination Indicator 
ggplot(accident, aes(x=reorder(`SURFACE_CONTAM_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Surface Contamination Indicator",
       x="Surface Contamination Indicator",
       y="Average Cost")

##Average total cost against Groundwater Contamination Indicator 
ggplot(accident, aes(x=reorder(`GROUNDWATER_CONTAM_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Groundwater Contamination Indicator",
       x="Groundwater Contamination Indicator",
       y="Average Cost")

#PRODUCT FACTORS ----------------------------------------------------------------------------------------------

##Average total cost against Installation Year 
ggplot(accident, aes(x=reorder(`INSTALLATION_YEAR_category`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Installation Year by Average Total Cost",
       x="Installation Year",
       y="Average Cost")

##Average total cost against Depth of Cover 
ggplot(accident, aes(x=reorder(`DEPTH_OF_COVER_category`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Depth of Cover by Average Total Cost",
       x="Depth of Cover",
       y="Average Cost")

##Average total cost against Cause
ggplot(accident, aes(x=reorder(`CAUSE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Cause",
       x="Cause",
       y="Average Cost")

##Average total cost against Commodity Released Type
accident$COMMODITY_RELEASED_TYPE <- str_wrap(accident$COMMODITY_RELEASED_TYPE, width = 25)

ggplot(accident, aes(x=reorder(`COMMODITY_RELEASED_TYPE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Commodity Released Type",
       x="Commodity Released Type",
       y="Average Cost")

##Average total cost against Commodity Subtype
accident$COMMODITY_SUBTYPE <- str_wrap(accident$COMMODITY_SUBTYPE, width = 25)

ggplot(accident, aes(x=reorder(`COMMODITY_SUBTYPE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Commodity Subtype",
       x="Commodity Subtype",
       y="Average Cost")

##Average total cost against Pipe Diameter 
ggplot(accident, aes(x=reorder(`PIPE_DIAMETER_category`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Pipe Diameter by Average Total Cost",
       x="Pipe Diameter",
       y="Average Cost")

##Average total cost against Pipe Wall Thickness 
ggplot(accident, aes(x=reorder(`PIPE_WALL_THICKNESS_category`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Pipe Wall Thickness by Average Total Cost",
       x="Pipe Wall Thickness",
       y="Average Cost")

##Average total cost against Pipe Specified Minimum Yield Strength 
ggplot(accident, aes(x=reorder(`PIPE_SMYS_category`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Pipe Specified Minimum Yield Strength by Average Total Cost",
       x="Pipe Specified Minimum Yield Strength",
       y="Average Cost")

##Average total cost against Release Type
ggplot(accident, aes(x=reorder(`RELEASE_TYPE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Release Type",
       x="Release Type",
       y="Average Cost")

#PROPERTY FACTORS ---------------------------------------------------------------------------------------------

##Average total cost against Location Type
accident$LOCATION_TYPE <- str_wrap(accident$LOCATION_TYPE, width = 25)

ggplot(accident, aes(x=reorder(`LOCATION_TYPE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Location Type",
       x="Location Type",
       y="Average Cost")

##Average total cost against Incident Area Type 
accident$INCIDENT_AREA_TYPE <- str_wrap(accident$INCIDENT_AREA_TYPE, width = 25)

ggplot(accident, aes(x=reorder(`INCIDENT_AREA_TYPE`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Incident Area Type",
       x="Incident Area Type",
       y="Average Cost")

##Average total cost against Could Affect High Consequence Area (HCA) Indicator (Promising)
ggplot(accident, aes(x=reorder(`COULD_BE_HCA`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Could Affect High Consequence Area Indicator",
       x="Could Affect High Consequence Area Indicator",
       y="Average Cost")

##Average total cost against Commodity Reached HCA Indicator (Promising)
ggplot(accident, aes(x=reorder(`COMMODITY_REACHED_HCA`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Commodity Reached HCA Indicator",
       x="Commodity Reached HCA Indicator",
       y="Average Cost")

##Average total cost against Shutdown due to Accident 
ggplot(accident, aes(x=reorder(`SHUTDOWN_DUE_ACCIDENT_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Shutdown due to Accident",
       x="Shutdown due to Accident",
       y="Average Cost")

##Average total cost against Those still shutdown due to Accident 
ggplot(accident, aes(x=reorder(`STILL_SHUTDOWN_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Those still shutdown due to Accident",
       x="Still shutdown due to Accident",
       y="Average Cost")

##Average total cost against Liquid Ignition 
ggplot(accident, aes(x=reorder(`IGNITE_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Liquid Ignition",
       x="Liquid Ignition",
       y="Average Cost")

##Average total cost against Liquid Explosion 
ggplot(accident, aes(x=reorder(`EXPLODE_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Liquid Explosion",
       x="Liquid Explosion",
       y="Average Cost")

#ENVIRONMENTAL FACTORS ----------------------------------------------------------------------------------------
##Average total cost against Wildlife Impact Indicator 
ggplot(accident, aes(x=reorder(`WILDLIFE_IMPACT_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Wildlife Impact Indicator",
       x="Wildlife Impact Indicator",
       y="Average Cost")

##Average total cost against Fish and Aquatic Life Impact Indicator 
ggplot(accident, aes(x=reorder(`FISH_AQUATIC_IMPACT_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Fish and Aquatic Life Impact Indicator",
       x="Fish and Aquatic Life Impact Indicator",
       y="Average Cost")

##Average total cost against Birds Impact Indicator 
ggplot(accident, aes(x=reorder(`BIRDS_IMPACT_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Birds Impact Indicator",
       x="Birds Impact Indicator",
       y="Average Cost")

##Average total cost against Terrestrial Impact Indicator 
ggplot(accident, aes(x=reorder(`TERRESTRIAL_IMPACT_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Terrestrial Impact Indicator",
       x="Terrestrial Impact Indicator",
       y="Average Cost")

##Average total cost against Soil Contamination Indicator 
ggplot(accident, aes(x=reorder(`SOIL_CONTAMINATION`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Soil Contamination Indicator",
       x="Soil Contamination Indicator",
       y="Average Cost")

##Average total cost against Long Term Assessment Indicator 
ggplot(accident, aes(x=reorder(`LONG_TERM_ASSESSMENT`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Long Term Assessment Indicator",
       x="Long Term Assessment Indicator",
       y="Average Cost")

##Average total cost against Remediation Indicator 
ggplot(accident, aes(x=reorder(`REMEDIATION_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Remediation Indicator",
       x="Remediation Indicator",
       y="Average Cost")

##Average total cost against Soil Remediation Indicator
ggplot(accident, aes(x=reorder(`SOIL_REMED_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Soil Remediation Indicator",
       x="Soil Remediation Indicator",
       y="Average Cost")

##Average total cost against Vegetation Remediation Indicator 
ggplot(accident, aes(x=reorder(`VEGETATION_REMED_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Vegetation Remediation Indicator",
       x="Vegetation Remediation Indicator",
       y="Average Cost")

##Average total cost against Wildlife Remediation Indicator 
ggplot(accident, aes(x=reorder(`WILDLIFE_REMED_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Wildlife Remediation Indicator",
       x="Wildlife Remediation Indicator",
       y="Average Cost")

##Average total cost against Ocean and Seawater Contamination Indicator
ggplot(accident, aes(x=reorder(`OCEAN_SEAWATER_IND`, `TOTAL_COST`, FUN=mean), y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  coord_flip() +
  labs(title="Average Total Cost by Ocean and Seawater Contamination Indicator",
       x="Ocean and Seawater Contamination Indicator",
       y="Average Cost")

# ================================================================
#Bi-variate EDA
##Response variable: TOTAL_COST
##Predictor variable: Continuous
##Continuous variables are converted to categorical variables based on IQR for more meaningful data exploration and visualisation
#=================================================================
##Average total cost by UNINTENTIONAL_RELEASE_BBLS 
accident_copy <- accident
q1 <- quantile(accident_copy$UNINTENTIONAL_RELEASE_BBLS, 0.25, na.rm = TRUE)
q2 <- quantile(accident_copy$UNINTENTIONAL_RELEASE_BBLS, 0.5, na.rm = TRUE)
q3 <- quantile(accident_copy$UNINTENTIONAL_RELEASE_BBLS, 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", q1, "]"),
            paste0("(", q1, ", ", q2, "]"),
            paste0("(", q2, ", ", q3, "]"),
            paste0("(", q3, ", Inf)"))

accident_copy$UNINTENTIONAL_RELEASE_BBLS_category <- cut(accident_copy$UNINTENTIONAL_RELEASE_BBLS, 
                                                         breaks = c(0, q1, q2, q3, Inf), 
                                                         labels = labels, 
                                                         include.lowest = TRUE)

accident_copy$UNINTENTIONAL_RELEASE_BBLS_category <- as.factor(accident_copy$UNINTENTIONAL_RELEASE_BBLS_category)
levels(accident_copy$UNINTENTIONAL_RELEASE_BBLS_category) <- c(levels(accident_copy$UNINTENTIONAL_RELEASE_BBLS_category))

head(accident_copy$UNINTENTIONAL_RELEASE_BBLS_category)

ggplot(accident_copy, aes(x = `UNINTENTIONAL_RELEASE_BBLS_category`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Unintentional release", y = "Number of accidents", title = "No. of accidents by Unintentional Release (Barrels)") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_copy, aes(x=`UNINTENTIONAL_RELEASE_BBLS_category`, y=TOTAL_COST)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Unintentional Release (Barrels)",
       x="Unintentional Release (Barrels)",
       y="Average Total Cost")

##Average total cost against INTENTIONAL_RELEASE_BBLS 
accident_copy <- accident
accident_copy <- accident_copy[accident_copy$INTENTIONAL_RELEASE_BBLS > 0, ] #removed 0s because breaks not unique
q1 <- quantile(accident_copy$INTENTIONAL_RELEASE_BBLS, 0.25, na.rm = TRUE)
q2 <- quantile(accident_copy$INTENTIONAL_RELEASE_BBLS, 0.5, na.rm = TRUE)
q3 <- quantile(accident_copy$INTENTIONAL_RELEASE_BBLS, 0.75, na.rm = TRUE)

labels <- c(paste0("(0, ", q1, "]"),
            paste0("(", q1, ", ", q2, "]"),
            paste0("(", q2, ", ", q3, "]"),
            paste0("(", q3, ", Inf)"))

accident_copy$INTENTIONAL_RELEASE_BBLS_category <- cut(accident_copy$INTENTIONAL_RELEASE_BBLS, 
                                                       breaks = c(0, q1, q2, q3, Inf), 
                                                       labels = labels, 
                                                       include.lowest = FALSE)

accident_copy$INTENTIONAL_RELEASE_BBLS_category <- as.factor(accident_copy$INTENTIONAL_RELEASE_BBLS_category)

ggplot(accident_copy, aes(x = `INTENTIONAL_RELEASE_BBLS_category`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Intentional release", y = "Number of accidents", title = "No. of accidents by Intentional Release (Barrels)") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_copy, aes(x=`INTENTIONAL_RELEASE_BBLS_category`, y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Intentional Release (Barrels)",
       x="Intentional Release (Barrels)",
       y="Average Total Cost")

##Average total cost against RECOVERED_BBLS 
accident_copy <- accident 
accident_copy <- accident_copy[accident_copy$RECOVERED_BBLS > 0,] #removed 0s because breaks not unique
q1 <- quantile(accident_copy$RECOVERED_BBLS, 0.25, na.rm = TRUE)
q2 <- quantile(accident_copy$RECOVERED_BBLS, 0.5, na.rm = TRUE)
q3 <- quantile(accident_copy$RECOVERED_BBLS, 0.75, na.rm = TRUE)

labels <- c(paste0("(0, ", q1, "]"),
            paste0("(", q1, ", ", q2, "]"),
            paste0("(", q2, ", ", q3, "]"),
            paste0("(", q3, ", Inf)"))

accident_copy$RECOVERED_BBLS_category <- cut(accident_copy$RECOVERED_BBLS, 
                                             breaks = c(0, q1, q2, q3, Inf), 
                                             labels = labels, 
                                             include.lowest = FALSE)

accident_copy$RECOVERED_BBLS_category <- as.factor(accident_copy$RECOVERED_BBLS_category)

ggplot(accident_copy, aes(x = `RECOVERED_BBLS_category`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Recovered commodity", y = "Number of accidents", title = "No. of accidents by Commodity Recovered (Barrels)") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_copy, aes(x=`RECOVERED_BBLS_category`, y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Commodity Recovered (Barrels)",
       x="Commodity Recovered (Barrels)",
       y="Average Total Cost")

##Average total cost against shutdown_duration 
#For shutdown_duration, NA means still shutdown hence it is included in (Q3,Inf) as it is shutdown for an indefinite period
accident_copy <- accident
accident_copy <- accident_copy[accident_copy$shutdown_duration > 0,] #removed 0s because breaks not unique
q1 <- round(quantile(accident_copy$shutdown_duration, 0.25, na.rm = TRUE), 2)
q2 <- round(quantile(accident_copy$shutdown_duration, 0.5, na.rm = TRUE), 2)
q3 <- round(quantile(accident_copy$shutdown_duration, 0.75, na.rm = TRUE), 2)

labels <- c(paste0("(0, ", q1, "]"),
            paste0("(", q1, ", ", q2, "]"),
            paste0("(", q2, ", ", q3, "]"),
            paste0("(", q3, ", Inf)"))

accident_copy$shutdown_duration_category <- cut(accident_copy$shutdown_duration, 
                                                breaks = c(0, q1, q2, q3, Inf), 
                                                labels = labels, 
                                                include.lowest = FALSE)

accident_copy$shutdown_duration_category[accident_copy$shutdown_duration > q3 | 
                                           is.na(accident_copy$shutdown_duration)] <- paste0("(", q3, ", Inf)")

accident_copy$shutdown_duration_category <- factor(accident_copy$shutdown_duration_category, levels = labels)

head(accident_copy$shutdown_duration_category)

ggplot(accident_copy, aes(x = `shutdown_duration_category`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Shutdown duration", y = "Number of accidents", title = "No. of accidents by Shutdown Duration") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_copy, aes(x=`shutdown_duration_category`, y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Shutdown Duration",
       x="Shutdown Duration",
       y="Average Total Cost")

##Average total cost against response_delay 
accident_copy <- accident[accident$response_delay > 0, ] #removed 0s because breaks not unique
q1 <- round(quantile(accident_copy$response_delay, 0.25, na.rm = TRUE), 2)
q2 <- round(quantile(accident_copy$response_delay, 0.5, na.rm = TRUE), 2)
q3 <- round(quantile(accident_copy$response_delay, 0.75, na.rm = TRUE), 2)

labels <- c(paste0("(0, ", q1, "]"),
            paste0("(", q1, ", ", q2, "]"),
            paste0("(", q2, ", ", q3, "]"),
            paste0("(", q3, ", Inf)"))

accident_copy$response_delay_category <- cut(accident_copy$response_delay, 
                                             breaks = c(0, q1, q2, q3, Inf), 
                                             labels = labels, 
                                             include.lowest = FALSE)

accident_copy$response_delay_category <- as.factor(accident_copy$response_delay_category)

head(accident_copy$response_delay_category)

ggplot(accident_copy, aes(x = `response_delay_category`)) + geom_bar(fill = "darkseagreen") + 
  labs(x = "Response delay", y = "Number of accidents", title = "No. of accidents by Response Delay") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_copy, aes(x=`response_delay_category`, y=`TOTAL_COST`)) + 
  stat_summary(geom = "bar", fun = "mean", fill = "darkseagreen") +
  labs(title="Average Total Cost by Response Delay",
       x="Response Delay",
       y="Average Total Cost")

# ================================================================
#Bi-variate EDA between two predictor variables
#=================================================================
continuous_vars <- c("UNINTENTIONAL_RELEASE_BBLS", "INTENTIONAL_RELEASE_BBLS", "RECOVERED_BBLS", 
                     "shutdown_duration", "response_delay")

categorical_vars <- c("LOCATION_TYPE", "INCIDENT_AREA_TYPE", "SYSTEM_PART_INVOLVED",
                      "CAUSE", "COMMODITY_RELEASED_TYPE", "RELEASE_TYPE")

#For shutdown_duration, the null values have been removed just for graph plotting
for (cat_var in categorical_vars) {
  for (cont_var in continuous_vars) {
    data_to_plot <- accident
    if (cont_var == "shutdown_duration") {
      data_to_plot <- na.omit(data_to_plot, cols = "shutdown_duration")
    }
    
    p <- ggplot(data_to_plot, aes(x=reorder(get(cat_var), get(cont_var), FUN=mean), y=get(cont_var))) + 
      stat_summary(fun = "mean", geom = "bar", fill = "darkseagreen") +
      labs(title=paste("Average", cont_var, "by", cat_var),
           x=cat_var,
           y=paste("Average", cont_var)) +
      theme_minimal() +
      coord_flip()
    
    print(p)
  }
}

# ================================================================
#Multi-variate EDA
#=================================================================
# UNINTENTIONAL_RELEASE_BBLS
q1 <- quantile(accident$UNINTENTIONAL_RELEASE_BBLS, 0.25, na.rm = TRUE)
q2 <- quantile(accident$UNINTENTIONAL_RELEASE_BBLS, 0.5, na.rm = TRUE)
q3 <- quantile(accident$UNINTENTIONAL_RELEASE_BBLS, 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", round(q1,3), "]"),
            paste0("(", round(q1,3), ", ", round(q2,3), "]"),
            paste0("(", round(q2,3), ", ", round(q3,3), "]"),
            paste0("(", round(q3,3), ", Inf)"))

accident$UNINTENTIONAL_RELEASE_BBLS_category <- cut(accident$UNINTENTIONAL_RELEASE_BBLS, 
                                                    breaks = c(0, round(q1,3), round(q2,3), round(q3,3), Inf), 
                                                    labels = labels, 
                                                    include.lowest = TRUE)

accident$UNINTENTIONAL_RELEASE_BBLS_category <- as.factor(accident$UNINTENTIONAL_RELEASE_BBLS_category)
levels(accident$UNINTENTIONAL_RELEASE_BBLS_category) <- c(levels(accident$UNINTENTIONAL_RELEASE_BBLS_category))

# INTENTIONAL_RELEASE_BBLS

q1 <- quantile(accident$INTENTIONAL_RELEASE_BBLS[accident$INTENTIONAL_RELEASE_BBLS>0], 0.25, na.rm = TRUE)
q2 <- quantile(accident$INTENTIONAL_RELEASE_BBLS[accident$INTENTIONAL_RELEASE_BBLS>0], 0.5, na.rm = TRUE)
q3 <- quantile(accident$INTENTIONAL_RELEASE_BBLS[accident$INTENTIONAL_RELEASE_BBLS>0], 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", round(q1,3), "]"),
            paste0("(", round(q1,3), ", ", round(q2,3), "]"),
            paste0("(", round(q2,3), ", ", round(q3,3), "]"),
            paste0("(", round(q3,3), ", Inf)"))

accident$INTENTIONAL_RELEASE_BBLS_category <- cut(accident$INTENTIONAL_RELEASE_BBLS, 
                                                  breaks = c(0, round(q1,3), round(q2,3), round(q3,3), Inf), 
                                                  labels = labels, 
                                                  include.lowest = TRUE)

accident$INTENTIONAL_RELEASE_BBLS_category <- as.factor(accident$INTENTIONAL_RELEASE_BBLS_category)
levels(accident$INTENTIONAL_RELEASE_BBLS_category) <- c(levels(accident$INTENTIONAL_RELEASE_BBLS_category))

# RECOVERED_BBLS

q1 <- quantile(accident$RECOVERED_BBLS, 0.25, na.rm = TRUE)
q2 <- quantile(accident$RECOVERED_BBLS, 0.5, na.rm = TRUE)
q3 <- quantile(accident$RECOVERED_BBLS, 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", round(q1,3), "]"),
            paste0("(", round(q1,3), ", ", round(q2,3), "]"),
            paste0("(", round(q2,3), ", ", round(q3,3), "]"),
            paste0("(", round(q3,3), ", Inf)"))

accident$RECOVERED_BBLS_category <- cut(accident$RECOVERED_BBLS, 
                                        breaks = c(0, round(q1,3), round(q2,3), round(q3,3), Inf),
                                        labels = labels, 
                                        include.lowest = TRUE)

accident$RECOVERED_BBLS_category <- as.factor(accident$RECOVERED_BBLS_category)
levels(accident$RECOVERED_BBLS_category) <- c(levels(accident$RECOVERED_BBLS_category))

# shutdown_duration

q1 <- quantile(accident$shutdown_duration[accident$shutdown_duration>0], 0.25, na.rm = TRUE)
q2 <- quantile(accident$shutdown_duration[accident$shutdown_duration>0], 0.5, na.rm = TRUE)
q3 <- quantile(accident$shutdown_duration[accident$shutdown_duration>0], 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", round(q1,3), "]"),
            paste0("(", round(q1,3), ", ", round(q2,3), "]"),
            paste0("(", round(q2,3), ", ", round(q3,3), "]"),
            paste0("(", round(q3,3), ", Inf)"))



accident$shutdown_duration_category <- cut(accident$shutdown_duration, 
                                           breaks = c(0, round(q1,3), round(q2,3), round(q3,3), Inf), 
                                           labels = labels, 
                                           include.lowest = TRUE)

accident$shutdown_duration_category[accident$shutdown_duration > q3 | 
                                      is.na(accident$shutdown_duration)] <- paste0("(", round(q3,3), ", Inf)")

accident$shutdown_duration_category <- as.factor(accident$shutdown_duration_category)
levels(accident$shutdown_duration_category) <- c(levels(accident$shutdown_duration_category))

# response_delay

q1 <- quantile(accident$response_delay[accident$response_delay>0], 0.25, na.rm = TRUE)
q2 <- quantile(accident$response_delay[accident$response_delay>0], 0.5, na.rm = TRUE)
q3 <- quantile(accident$response_delay[accident$response_delay>0], 0.75, na.rm = TRUE)

labels <- c(paste0("[0, ", round(q1,3), "]"),
            paste0("(", round(q1,3), ", ", round(q2,3), "]"),
            paste0("(", round(q2,3), ", ", round(q3,3), "]"),
            paste0("(", round(q3,3), ", Inf)"))



accident$response_delay_category <- cut(accident$response_delay, 
                                        breaks = c(0, round(q1,3), round(q2,3), round(q3,3), Inf), 
                                        labels = labels, 
                                        include.lowest = TRUE)

accident$response_delay_category <- as.factor(accident$response_delay_category)
levels(accident$response_delay_category) <- c(levels(accident$response_delay_category))

labels=c("Emergency", "Environmental", "Gas Released", "Operator", "Other", "Property Damage")


# Characteristics of accident 

# Average & Total Cost against Cost Type and Cause

long_data <- accident %>%
  select(`CAUSE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -`CAUSE`)

summed_data <- long_data %>%
  group_by(`CAUSE`, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

avg_data <- long_data %>%
  group_by(`CAUSE`, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = CAUSE, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Cause", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Cost Type and Cause") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(labels = labels)

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and Cause") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels)

# Average & Total Cost against Cost Type and Shutdown duration

long_data <- accident %>%
  select(`shutdown_duration_category`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -`shutdown_duration_category`)

summed_data <- long_data %>%
  group_by(`shutdown_duration_category`, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

avg_data <- long_data %>%
  group_by(`shutdown_duration_category`, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = shutdown_duration_category, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Shutdown Duration", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Shutdown Duration and Cost Type") +
  scale_fill_discrete(labels = labels)

ggplot(avg_data, aes(x = shutdown_duration_category, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Shutdown Duration", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Shutdown Duration and Cost Type") +
  scale_fill_discrete(labels = labels)

# Average & Total Cost against Cost Type and Response Delay

long_data <- accident %>%
  select(`response_delay_category`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -`response_delay_category`)

summed_data <- long_data %>%
  group_by(`response_delay_category`, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

avg_data <- long_data %>%
  group_by(`response_delay_category`, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = response_delay_category, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Response Delay", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Response Delay and Cost Type") +
  scale_fill_discrete(labels = labels)

ggplot(avg_data, aes(x = response_delay_category, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Response Delay", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Response Delay and Cost Type") +
  scale_fill_discrete(labels = labels)

# Average & Total Cost against Cost Type and Location Type

long_data <- accident %>%
  select(LOCATION_TYPE, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -LOCATION_TYPE)

summed_data <- long_data %>%
  group_by(LOCATION_TYPE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

avg_data <- long_data %>%
  group_by(`LOCATION_TYPE`, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = LOCATION_TYPE, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Location Type", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Location Type and Cost Type" ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(labels = labels)

ggplot(avg_data, aes(x = LOCATION_TYPE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Location Type", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Location Type and Cost Type" ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(labels = labels)

# Total Cost against Cost Type and Incident Area Type

long_data <- accident %>%
  select(INCIDENT_AREA_TYPE, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -INCIDENT_AREA_TYPE)

summed_data <- long_data %>%
  group_by(INCIDENT_AREA_TYPE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = INCIDENT_AREA_TYPE, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Incident Area Type", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Location Type and Cost Type" ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(labels = labels)

ggplot(summed_data, aes(x = INCIDENT_AREA_TYPE, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") + coord_flip() +
  labs(x = "Incident Area Type", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Location Type and Cost Type" ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(labels = labels)

# Total Cost against Cost Type and System Part Involved

long_data <- accident %>%
  select(`SYSTEM_PART_INVOLVED`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -`SYSTEM_PART_INVOLVED`)

summed_data <- long_data %>%
  group_by(`SYSTEM_PART_INVOLVED`, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = SYSTEM_PART_INVOLVED, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "System Part", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Cost Type and System Part Involved") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_discrete(labels = labels)

ggplot(summed_data, aes(x = SYSTEM_PART_INVOLVED, y = Total_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") + coord_flip() +
  labs(x = "System Part", y = "Total Cost", fill = "Cost Type") +
  ggtitle("Total Cost against Cost Type and System Part Involved") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_discrete(labels = labels)

# Total Cost against Shutdown Duration and Response Delay

long_data <- accident %>%
  select(`response_delay_category`, `shutdown_duration_category`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`response_delay_category`, shutdown_duration_category))

summed_data <- long_data %>%
  group_by(`response_delay_category`, shutdown_duration_category, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = response_delay_category, y = Total_Cost, fill = shutdown_duration_category)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Response Delay", y = "Total Cost", fill = "Shutdown Duration") +
  ggtitle("Total Cost against Response Delay and Shutdown Duration")

# Average & Total Cost against Location Type and Cause

long_data <- accident %>%
  select(`LOCATION_TYPE`, `CAUSE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`LOCATION_TYPE`, CAUSE))

summed_data <- long_data %>%
  group_by(`LOCATION_TYPE`, CAUSE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

avg_data <- long_data %>%
  group_by(`LOCATION_TYPE`, CAUSE, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = LOCATION_TYPE, y = Total_Cost, fill = CAUSE)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Location Type", y = "Total Cost", fill = "Cause") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Total Cost against Location Type and Cause")

ggplot(avg_data[avg_data$LOCATION_TYPE != "OFFSHORE", ], aes(x = LOCATION_TYPE, y = Avg_Cost, fill = CAUSE)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Location Type", y = "Total Cost", fill = "Cause") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Average Cost against Location Type and Cause")

# Total Cost against Release Type and Cause

long_data <- accident %>%
  select(`RELEASE_TYPE`, `CAUSE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`RELEASE_TYPE`, CAUSE))

summed_data <- long_data %>%
  group_by(`RELEASE_TYPE`, CAUSE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = RELEASE_TYPE, y = Total_Cost, fill = CAUSE)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Release Type", y = "Total Cost", fill = "Cause") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Total Cost against Release Type and Cause")


# Total Cost against Location Type and Release Type

long_data <- accident %>%
  select(`LOCATION_TYPE`, `RELEASE_TYPE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`LOCATION_TYPE`, RELEASE_TYPE))

summed_data <- long_data %>%
  group_by(`LOCATION_TYPE`, RELEASE_TYPE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = LOCATION_TYPE, y = Total_Cost, fill = RELEASE_TYPE)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Location Type", y = "Total Cost", fill = "Release Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Total Cost against Location Type and Release Type")

# Total Cost against System Part and Release Type

long_data <- accident %>%
  select(`SYSTEM_PART_INVOLVED`, `RELEASE_TYPE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`SYSTEM_PART_INVOLVED`, RELEASE_TYPE))

summed_data <- long_data %>%
  group_by(`SYSTEM_PART_INVOLVED`, RELEASE_TYPE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = SYSTEM_PART_INVOLVED, y = Total_Cost, fill = RELEASE_TYPE)) +
  geom_bar(stat = "identity", position = "fill") + coord_flip() +
  labs(x = "System Part", y = "Total Cost", fill = "Release Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 5)) +
  ggtitle("Total Cost against System Part Involved and Release Type")

# Total Cost against System Part and Location Type

long_data <- accident %>%
  select(`SYSTEM_PART_INVOLVED`, `LOCATION_TYPE`, starts_with("EST_COST")) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`SYSTEM_PART_INVOLVED`, LOCATION_TYPE))

summed_data <- long_data %>%
  group_by(`SYSTEM_PART_INVOLVED`, LOCATION_TYPE, Cost_Type) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(summed_data, aes(x = SYSTEM_PART_INVOLVED, y = Total_Cost, fill = LOCATION_TYPE)) +
  geom_bar(stat = "identity", position = "fill") + coord_flip() +
  labs(x = "System Part", y = "Total Cost", fill = "Location Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Total Cost against System Part Involved and Location Type")

# Avg Cost against Cost Type, Commodity Released type 

long_data <- accident %>%
  select(`CAUSE`, COMMODITY_RELEASED_TYPE, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, COMMODITY_RELEASED_TYPE))

avg_data <- long_data %>%
  group_by(`CAUSE`, COMMODITY_RELEASED_TYPE, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = COMMODITY_RELEASED_TYPE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and Commodity Released Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_discrete(labels = labels)

#-------------------------------------------------------------------------------------------------------
# Consequences of accident
# Avg Cost against Cost Type, Cause and Evacuation

long_data <- accident %>%
  select(`CAUSE`, EVACUATION, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, EVACUATION))

avg_data <- long_data %>%
  group_by(`CAUSE`, EVACUATION, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Evacuation") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ EVACUATION)

# Total Cost against Cost Type, Cause and Shutdown Indicator

long_data <- accident %>%
  select(`CAUSE`, SHUTDOWN_DUE_ACCIDENT_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, SHUTDOWN_DUE_ACCIDENT_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, SHUTDOWN_DUE_ACCIDENT_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Shutdown") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ SHUTDOWN_DUE_ACCIDENT_IND)

# Average Cost against Cost Type, Cause and Still Shutdown Indicator

long_data <- accident %>%
  select(`CAUSE`, STILL_SHUTDOWN_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, STILL_SHUTDOWN_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, STILL_SHUTDOWN_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Still Shutdown") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ STILL_SHUTDOWN_IND)

# Avg Cost against Cost Type, Cause and Ignition Indicator

long_data <- accident %>%
  select(`CAUSE`, IGNITE_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, IGNITE_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, IGNITE_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Ignition Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ IGNITE_IND)

# Avg Cost against Cost Type, Cause and Explosion Indicator

long_data <- accident %>%
  select(`CAUSE`, EXPLODE_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, EXPLODE_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, EXPLODE_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Explosion Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ EXPLODE_IND)

# Avg Cost against Cost Type, Cause and On or Off Shore

long_data <- accident %>%
  select(`CAUSE`, ON_OFF_SHORE, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, ON_OFF_SHORE))

avg_data <- long_data %>%
  group_by(`CAUSE`, ON_OFF_SHORE, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and On or Off Shore") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ ON_OFF_SHORE)

# Avg Cost against Cost Type, Cause and Water Contamination Indicator 

long_data <- accident %>%
  select(`CAUSE`, WATER_CONTAM_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, WATER_CONTAM_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, WATER_CONTAM_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Water Contamination Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ WATER_CONTAM_IND)

# Avg Cost against Cost Type, Cause and Surface Water Contamination Indicator 

long_data <- accident %>%
  select(`CAUSE`, SURFACE_CONTAM_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, SURFACE_CONTAM_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, SURFACE_CONTAM_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Surface Water Contamination Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ SURFACE_CONTAM_IND)

# Avg Cost against Cost Type, Cause and Fatalities Indicator 

long_data <- accident %>%
  select(`CAUSE`, FATALITY_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, FATALITY_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, FATALITY_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Fatality Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ FATALITY_IND)

# Avg Cost against Cost Type, Cause and Soil Contamination Indicator 

long_data <- accident %>%
  select(`CAUSE`, SOIL_CONTAMINATION, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, SOIL_CONTAMINATION))

avg_data <- long_data %>%
  group_by(`CAUSE`, SOIL_CONTAMINATION, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Soil contamination Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ SOIL_CONTAMINATION)

# Avg Cost against Cost Type, Cause and Wildlife Impact Indicator 

long_data <- accident %>%
  select(`CAUSE`, WILDLIFE_IMPACT_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, WILDLIFE_IMPACT_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, WILDLIFE_IMPACT_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Wildlife Impact Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ WILDLIFE_IMPACT_IND)

# Avg Cost against Cost Type, Cause and Aquatic Impact Indicator 

long_data <- accident %>%
  select(`CAUSE`, FISH_AQUATIC_IMPACT_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, FISH_AQUATIC_IMPACT_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, FISH_AQUATIC_IMPACT_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Aquatic Impact Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ FISH_AQUATIC_IMPACT_IND)

# Avg Cost against Cost Type, Cause and Bird Impact Indicator 

long_data <- accident %>%
  select(`CAUSE`, BIRDS_IMPACT_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, BIRDS_IMPACT_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, BIRDS_IMPACT_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Bird Impact Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ BIRDS_IMPACT_IND)

# Avg Cost against Cost Type, Cause and Terrestrial Impact Indicator 

long_data <- accident %>%
  select(`CAUSE`, TERRESTRIAL_IMPACT_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, TERRESTRIAL_IMPACT_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, TERRESTRIAL_IMPACT_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Terrestrial Impact Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ TERRESTRIAL_IMPACT_IND)

# Avg Cost against Cost Type and Unintentional Release

long_data <- accident %>%
  select(UNINTENTIONAL_RELEASE_BBLS_category, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(UNINTENTIONAL_RELEASE_BBLS_category))

avg_data <- long_data %>%
  group_by(UNINTENTIONAL_RELEASE_BBLS_category, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = UNINTENTIONAL_RELEASE_BBLS_category, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Unintentional Release", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and Unintentional Release") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) 

# Avg Cost against Cost Type and Intentional Release

long_data <- accident %>%
  select(INTENTIONAL_RELEASE_BBLS_category, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(INTENTIONAL_RELEASE_BBLS_category))

avg_data <- long_data %>%
  group_by(INTENTIONAL_RELEASE_BBLS_category, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = INTENTIONAL_RELEASE_BBLS_category, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Unintentional Release", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and Intentional Release") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) 

# Avg Cost against Cost Type and Recovered 

long_data <- accident %>%
  select(RECOVERED_BBLS_category, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(RECOVERED_BBLS_category))

avg_data <- long_data %>%
  group_by(RECOVERED_BBLS_category, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = RECOVERED_BBLS_category, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Unintentional Release", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and Recovered Commodity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) 

# Avg Cost against Cost Type and COMMODITY_REACHED_HCA 

long_data <- accident %>%
  select(COMMODITY_REACHED_HCA, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(COMMODITY_REACHED_HCA))

avg_data <- long_data %>%
  group_by(COMMODITY_REACHED_HCA, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = COMMODITY_REACHED_HCA, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "COMMODITY_REACHED_HCA", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and COMMODITY_REACHED_HCA") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) 

# Avg Cost against Cause and COMMODITY_REACHED_HCA 

long_data <- accident %>%
  select(COMMODITY_REACHED_HCA, CAUSE, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(COMMODITY_REACHED_HCA, CAUSE))

avg_data <- long_data %>%
  group_by(COMMODITY_REACHED_HCA, CAUSE, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = COMMODITY_REACHED_HCA, y = Avg_Cost, fill = CAUSE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "COMMODITY_REACHED_HCA", y = "Average Cost", fill = "Cause") +
  ggtitle("Average Cost against Cost Type and COMMODITY_REACHED_HCA") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

# Avg Cost against Cost Type and COULD_BE_HCA 

long_data <- accident %>%
  select(COULD_BE_HCA, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(COULD_BE_HCA))

avg_data <- long_data %>%
  group_by(COULD_BE_HCA, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = COULD_BE_HCA, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "COULD_BE_HCA", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type and COULD_BE_HCA") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) 

# Avg Cost against Cost Type, Cause and Remediation Indicator 

long_data <- accident %>%
  select(`CAUSE`, REMEDIATION_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, REMEDIATION_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, REMEDIATION_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Remediation Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ REMEDIATION_IND)

# Avg Cost against Cost Type, Cause and Vegetation Remediation Indicator 

long_data <- accident %>%
  select(`CAUSE`, VEGETATION_REMED_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, VEGETATION_REMED_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, VEGETATION_REMED_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Vegetation Remediation Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ VEGETATION_REMED_IND)

# Avg Cost against Cost Type, Cause and Groundwater Remediation Indicator 

long_data <- accident %>%
  select(`CAUSE`, GROUNDWATER_REMED_IND, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, GROUNDWATER_REMED_IND))

avg_data <- long_data %>%
  group_by(`CAUSE`, GROUNDWATER_REMED_IND, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Groundwater Remediation Indicator") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ GROUNDWATER_REMED_IND)

# Avg Cost against Cost Type, Cause and Long Term Assessment

long_data <- accident %>%
  select(`CAUSE`, LONG_TERM_ASSESSMENT, starts_with("EST_COST", )) %>%
  gather(key = "Cost_Type", value = "Cost", -c(`CAUSE`, LONG_TERM_ASSESSMENT))

avg_data <- long_data %>%
  group_by(`CAUSE`, LONG_TERM_ASSESSMENT, Cost_Type) %>%
  summarise(Avg_Cost = mean(Cost, na.rm = TRUE))

ggplot(avg_data, aes(x = CAUSE, y = Avg_Cost, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Cause", y = "Average Cost", fill = "Cost Type") +
  ggtitle("Average Cost against Cost Type, Cause and Long Term Assessment") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_discrete(labels = labels) + 
  theme(axis.text.x = element_text(size=7, angle = 90)) +
  facet_grid(.~ LONG_TERM_ASSESSMENT)


#---------------------------------------End of Phase 2------------------------------------



# ++=======================++
# ||   Phase 3: Modelling  ||
# ++=======================++

# +----------------------------+
# | 3.1 Cost Prediction Models |
# +----------------------------+

##-----------------------Train-Test Split---------------------------------------
# createDataPartition() is used because some levels of SYSTEM_PART_INVOLVED have very few or even single observation and
# we have to make sure all levels are split into both train and testset.
set.seed(1)
train.index <- createDataPartition(accident$SYSTEM_PART_INVOLVED, p = 0.7, list = FALSE)
trainset <- accident[ train.index,]
testset  <- accident[-train.index,]

dim(trainset) #2712 rows
dim(testset) #1158 rows

#-------------------------------------------------------------------------------

# +-------------------------------+
# | 3.1.1 Linear Regression Model |
# +-------------------------------+

#Step 0: to get some initial insights from full model, i.e., with all relevant X variables
m.full <- lm(TOTAL_COST ~ IYEAR+ON_OFF_SHORE+LOCATION_LATITUDE+LOCATION_LONGITUDE+LOCATION_TYPE+INCIDENT_AREA_TYPE+
               INCIDENT_AREA_SUBTYPE+SYSTEM_PART_INVOLVED+CAUSE+COMMODITY_RELEASED_TYPE+COMMODITY_SUBTYPE+
               UNINTENTIONAL_RELEASE_BBLS+INTENTIONAL_RELEASE_BBLS+RECOVERED_BBLS+RELEASE_TYPE+IGNITE_IND+EXPLODE_IND+
               SHUTDOWN_DUE_ACCIDENT_IND+NUM_PUB_EVACUATED+INJURY_IND+NUM_EMP_INJURIES+NUM_CONTR_INJURIES+NUM_WORKER_INJURIES+
               NUM_GP_INJURIES+INJURE+FATALITY_IND+NUM_EMP_FATALITIES+NUM_CONTR_FATALITIES+NUM_WORKER_FATALITIES+NUM_GP_FATALITIES+
               FATAL+WILDLIFE_IMPACT_IND+FISH_AQUATIC_IMPACT_IND+TERRESTRIAL_IMPACT_IND+SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+
               REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+SOIL_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+
               WATER_CONTAM_IND+OCEAN_SEAWATER_IND+SURFACE_CONTAM_IND+GROUNDWATER_CONTAM_IND+COMMODITY_REACHED_HCA+response_delay, 
             data = trainset)

summary(m.full)
#summary(m.full)$coefficients[,4]  #p-value


#Step 1: Select variables based on p-value, i.e., lower than 5%
m1 <- lm(TOTAL_COST ~ 
           ON_OFF_SHORE+
           LOCATION_LONGITUDE+
           LOCATION_TYPE+
           INCIDENT_AREA_SUBTYPE+
           SYSTEM_PART_INVOLVED+
           CAUSE+
           UNINTENTIONAL_RELEASE_BBLS+ 
           INTENTIONAL_RELEASE_BBLS +
           RECOVERED_BBLS+
           RELEASE_TYPE+
           IGNITE_IND+
           SHUTDOWN_DUE_ACCIDENT_IND+
           FATALITY_IND+
           NUM_CONTR_FATALITIES+ 
           WILDLIFE_IMPACT_IND+
           FISH_AQUATIC_IMPACT_IND+
           LONG_TERM_ASSESSMENT+
           SURFACE_WATER_REMED_IND+
           GROUNDWATER_REMED_IND+
           VEGETATION_REMED_IND+
           WATER_CONTAM_IND+
           SURFACE_CONTAM_IND+
           COMMODITY_REACHED_HCA+
           response_delay
         ,data = trainset)

summary(m1)


#Step 2: Further elimination of variables using Automatic Selection Algorithm
##2.1 Stepwise AIC Regression
ols_step_both_aic(m1, details = TRUE)  #response_delay and ON_OFF_SHORE are removed

##2.2 Backward Elimination 
m_back <- step(m1)
summary(m_back) #Similarly, response_delay and ON_OFF_SHORE are removed


#Step 3: Removing variable that has no practical significant, i.e., not applicable to Aramco
#LOCATION_LONGITUDE is removed because this geographical information is for pipelines in the US
m2 <- lm(TOTAL_COST ~ 
           INCIDENT_AREA_SUBTYPE+        
           UNINTENTIONAL_RELEASE_BBLS+ 
           INTENTIONAL_RELEASE_BBLS+  
           RECOVERED_BBLS+     
           CAUSE+                         
           LONG_TERM_ASSESSMENT+           
           SYSTEM_PART_INVOLVED+          
           VEGETATION_REMED_IND+           
           COMMODITY_REACHED_HCA+          
           WILDLIFE_IMPACT_IND+            
           LOCATION_TYPE+                 
           GROUNDWATER_REMED_IND+         
           FATALITY_IND+                  
           RELEASE_TYPE+                   
           SHUTDOWN_DUE_ACCIDENT_IND+      
           NUM_CONTR_FATALITIES+         
           SURFACE_WATER_REMED_IND+        
           SURFACE_CONTAM_IND+            
           WATER_CONTAM_IND+              
           FISH_AQUATIC_IMPACT_IND+        
           IGNITE_IND   
         ,data = trainset)

summary(m2)


#Step 4: Remove statistically insignificant variables based on p-value again
#IGNITE_IND is removed
m3 <- lm(TOTAL_COST ~ 
           SYSTEM_PART_INVOLVED+
           CAUSE+
           COMMODITY_RELEASED_TYPE+
           UNINTENTIONAL_RELEASE_BBLS +
           INTENTIONAL_RELEASE_BBLS+
           RECOVERED_BBLS +
           RELEASE_TYPE+
           SHUTDOWN_DUE_ACCIDENT_IND+
           NUM_CONTR_FATALITIES+
           FATALITY_IND+
           WILDLIFE_IMPACT_IND+
           FISH_AQUATIC_IMPACT_IND+
           LONG_TERM_ASSESSMENT+
           SURFACE_WATER_REMED_IND+
           GROUNDWATER_REMED_IND+
           VEGETATION_REMED_IND+
           WATER_CONTAM_IND+
           SURFACE_CONTAM_IND+
           COMMODITY_REACHED_HCA
         ,data = trainset)

summary(m3)


#Step 5: Check VIF to detect multicollinearity and redundancy in our model
#No multicollinearity -> VIF (continuous Xs) <10 / adj-GVIF(categorical Xs) <2
vif(m3)

#Multicollinearity exists as adj-GVIF for WILDLIFE_IMPACT_IND, FISH_AQUATIC_IMPACT_IND, WATER_CONTAM_IND and SURFACE_CONTAM_IND are higher than 2
# WILDLIFE_IMPACT_IND:     2.670667
# FISH_AQUATIC_IMPACT_IND: 2.709521
# WATER_CONTAM_IND:        4.038616
# SURFACE_CONTAM_IND:      3.615152

# Based on domain knowledge and our investigation:
# WILDLIFE_IMPACT_IND and FISH_AQUATIC_IMPACT_IND are correlated
table(WILDLIFE_IMPACT_IND=accident$WILDLIFE_IMPACT_IND,FISH_AQUATIC_IMPACT_IND=accident$FISH_AQUATIC_IMPACT_IND)
mean(accident$WILDLIFE_IMPACT_IND==accident$FISH_AQUATIC_IMPACT_IND) #percentage of agreement: 0.9997416

# WATER_CONTAM_IND and SURFACE_CONTAM_IND are correalted.
table(WATER_CONTAM_IND=accident$WATER_CONTAM_IND,SURFACE_CONTAM_IND=accident$SURFACE_CONTAM_IND)
mean(accident$WATER_CONTAM_IND==accident$SURFACE_CONTAM_IND) #percentage of agreement: 0.9868217

# FISH_AQUATIC_IMPACT_IND and WATER_CONTAM_IND are removed due to their higher p-value, suggesting 
# smaller predictive power as compared to WILDLIFE_IMPACT_IND and SURFACE_CONTAM_IND respectively
m4 <- lm(TOTAL_COST ~ 
           SYSTEM_PART_INVOLVED+
           CAUSE+
           COMMODITY_RELEASED_TYPE+
           UNINTENTIONAL_RELEASE_BBLS +
           INTENTIONAL_RELEASE_BBLS+
           RECOVERED_BBLS +
           RELEASE_TYPE+
           SHUTDOWN_DUE_ACCIDENT_IND+
           NUM_CONTR_FATALITIES+
           FATALITY_IND+
           WILDLIFE_IMPACT_IND+
           LONG_TERM_ASSESSMENT+
           SURFACE_WATER_REMED_IND+
           GROUNDWATER_REMED_IND+
           VEGETATION_REMED_IND+
           SURFACE_CONTAM_IND+
           COMMODITY_REACHED_HCA
         ,data = trainset)

vif(m4) #No more multicollinearity issue

summary(m4) #Multiple R-squared:  0.4625,	Adjusted R-squared:  0.4557 
#summary(m4)$coefficients[,1] #check coefficients


#Step 6: Compute trainset and testset errors
# Residuals = Error = Actual TOTAL_COST - Model Predicted TOTAL_COST
# Trainset Errors
RMSE.m4.train <- sqrt(mean(residuals(m4)^2)) 

# Testset Errors
predict.m4.test <- predict(m4, newdata = testset) 
testset.error <- testset$TOTAL_COST - predict.m4.test
RMSE.m4.test <- sqrt(mean(testset.error^2))

RMSE.m4.train #148406.4
RMSE.m4.test #151716.3


#Step 7: Diagnostic Checks
par(mfrow = c(2,2)) 
plot(m4)  # Plot model 4 diagnostics
par(mfrow = c(1,1))

#-------------------------------------------------------------------------------


# +-----------------------------+
# | 3.1.2 CART: Regression Tree |
# +-----------------------------+


#The CART algorithm works to find the independent variables that create the best homogeneous group when splitting the data. 
#Thus, there is no need to specify important independent variable and CART will handle it automatically
set.seed(2406) #for 10-fold CV

#Step 1: Grow Tree to max by including all relevant variables and set cp to 0
cart1 <- rpart(TOTAL_COST ~ IYEAR+ON_OFF_SHORE+LOCATION_LATITUDE+LOCATION_LONGITUDE+LOCATION_TYPE+
                 INCIDENT_AREA_TYPE+INCIDENT_AREA_SUBTYPE+SYSTEM_PART_INVOLVED+CAUSE+COMMODITY_RELEASED_TYPE+
                 COMMODITY_SUBTYPE+UNINTENTIONAL_RELEASE_BBLS+INTENTIONAL_RELEASE_BBLS+RECOVERED_BBLS+
                 RELEASE_TYPE+IGNITE_IND+EXPLODE_IND+SHUTDOWN_DUE_ACCIDENT_IND+STILL_SHUTDOWN_IND+NUM_PUB_EVACUATED+
                 INJURY_IND+NUM_EMP_INJURIES+NUM_CONTR_INJURIES+NUM_WORKER_INJURIES+NUM_GP_INJURIES+INJURE+
                 FATALITY_IND+NUM_EMP_FATALITIES+NUM_CONTR_FATALITIES+NUM_WORKER_FATALITIES+NUM_GP_FATALITIES+
                 FATAL+WILDLIFE_IMPACT_IND+FISH_AQUATIC_IMPACT_IND+BIRDS_IMPACT_IND+TERRESTRIAL_IMPACT_IND+
                 SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+
                 SOIL_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+WATER_CONTAM_IND+OCEAN_SEAWATER_IND+
                 SURFACE_CONTAM_IND+GROUNDWATER_CONTAM_IND+COMMODITY_REACHED_HCA+EVACUATION+response_delay
               ,data = trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart1)
plotcp(cart1)
print(cart1)


#Step 2: Prune Tree to min
##2.1: Find Optimal cp:
#store the cptable
dt<-data.table(cart1$cptable)
#number the sequence of the trees
dt[,index:=1:nrow(dt)]
#find out minimum index where xerror is min
min_cp_index<-min(dt[(xerror+xstd)==min(xerror+xstd),index])
#find the errorcap
errorcap<-dt[min_cp_index,xerror+xstd]
#find out the optimal index for the cp
optimal_cp_index <- min(dt[(xerror < errorcap), index])
#Find the geometric mean of the cp for that index and one cp appearing before it
optimal.cp=sqrt(dt[index==optimal_cp_index,CP]*dt[index==optimal_cp_index-1,CP]) #0.09585533


##2.2: Prune the max tree using optimal.cp
cartOptimal <- prune(cart1, cp = optimal.cp)

printcp(cartOptimal, digits = 3)

print(cartOptimal)

rpart.plot(cartOptimal, nn = T, main = "Optimal Tree in Accident")
## The number inside each node represent the mean value of Y.

cartOptimal$variable.importance

summary(cartOptimal)

# Step 3: Evaluate the CART Model
##3.1: create the evaluation metrics function
eval_CART <- function(true_value, predicted_value, data) {
  SSE <- sum((predicted_value - true_value)^2) #Explained Sum of Squares / Sum of Squares Error
  SST <- sum((true_value - mean(true_value))^2) #Total Sum of Squares
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(data))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

##3.2: predicting and evaluating the model on train data
predictions_train_cart = predict(cartOptimal, data = trainset)
eval_CART(trainset$TOTAL_COST, predictions_train_cart, trainset) #RMSE: 153717.5, Rsquare: 0.4233723

##3.3: predicting and evaluating the model on test data
predictions_test_cart = predict(cartOptimal, newdata = testset)
eval_CART(testset$TOTAL_COST, predictions_test_cart, testset) #RMSE: 167133.9, Rsquare: 0.1558794

#-------------------------------------------------------------------------------


# +----------------------------+
# | 3.2 High Cost Alert Models |
# +----------------------------+

#To build high cost alert models, we would use the original dataset which also include
#the 21 rows with extreme high total_cost values. These rows were previously removed from
#accident in Phase 2 for EDA and building cost prediction models.

#Read in the outputed original dataframe in phase 1 and store it to accident_full
accident_full <- fread("accident_clean.csv")

#Investigate on records with high TOTAL_COST
#function to count the number of outliers
count_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], na.rm = TRUE, probs = c(0.25, 0.75))
  IQR_val <- IQR(data[[column_name]],na.rm=TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_val #Lower whisker
  upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker
  
  outliers_count <- sum(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, na.rm = TRUE) #IQR Method to identify outliers
  
  return(outliers_count)
}

count_outliers(accident_full,"TOTAL_COST") #566 outliers

# ================================================================
# Data Processing: convert to nominal categorical var
#=================================================================
accident_full$`NAME` <- factor(accident_full$`NAME`)
accident_full$`PIPE_FAC_NAME` <- factor(accident_full$`PIPE_FAC_NAME`)
accident_full$`ON_OFF_SHORE` <- factor(accident_full$`ON_OFF_SHORE`)
accident_full$`ON_OFF_SHORE` <- factor(accident_full$`ON_OFF_SHORE`)
accident_full$`ONSHORE_STATE_ABBREVIATION` <- factor(accident_full$`ONSHORE_STATE_ABBREVIATION`)
accident_full$`ONSHORE_CITY_NAME` <- factor(accident_full$`ONSHORE_CITY_NAME`)
accident_full$`ONSHORE_COUNTY_NAME` <- factor(accident_full$`ONSHORE_COUNTY_NAME`)
accident_full$`LOCATION_TYPE` <- factor(accident_full$`LOCATION_TYPE`)
accident_full$`INCIDENT_AREA_TYPE` <- factor(accident_full$`INCIDENT_AREA_TYPE`)
accident_full$`INCIDENT_AREA_SUBTYPE` <- factor(accident_full$`INCIDENT_AREA_SUBTYPE`)
accident_full$`INCIDENT_AREA_DETAILS` <- factor(accident_full$`INCIDENT_AREA_DETAILS`)
accident_full$`DEPTH_OF_COVER` <- factor(accident_full$`DEPTH_OF_COVER`) 
accident_full$`SYSTEM_PART_INVOLVED` <- factor(accident_full$`SYSTEM_PART_INVOLVED`)
accident_full$`CAUSE` <- factor(accident_full$`CAUSE`)
accident_full$`CAUSE_DETAILS` <- factor(accident_full$`CAUSE_DETAILS`)
accident_full$`COMMODITY_RELEASED_TYPE` <- factor(accident_full$`COMMODITY_RELEASED_TYPE`)
accident_full$`COMMODITY_SUBTYPE` <- factor(accident_full$`COMMODITY_SUBTYPE`) 
accident_full$`COMMODITY_DETAILS` <- factor(accident_full$`COMMODITY_DETAILS`) 
accident_full$`PIPE_DIAMETER` <- factor(accident_full$`PIPE_DIAMETER`) 
accident_full$`PIPE_WALL_THICKNESS` <- factor(accident_full$`PIPE_WALL_THICKNESS`) 
accident_full$`PIPE_SMYS` <- factor(accident_full$`PIPE_SMYS`) 
accident_full$`PIPE_SPECIFICATION` <- factor(accident_full$`PIPE_SPECIFICATION`) 
accident_full$`INSTALLATION_YEAR` <- factor(accident_full$`INSTALLATION_YEAR`) 
accident_full$`MATERIAL_INVOLVED` <- factor(accident_full$`MATERIAL_INVOLVED`) 
accident_full$`MATERIAL_DETAILS` <- factor(accident_full$`MATERIAL_DETAILS`) 
accident_full$`RELEASE_TYPE` <- factor(accident_full$`RELEASE_TYPE`)
accident_full$`IGNITE_IND` <- factor(accident_full$`IGNITE_IND`)
accident_full$`EXPLODE_IND` <- factor(accident_full$`EXPLODE_IND`)
accident_full$`SHUTDOWN_DUE_accident_full_IND` <- factor(accident_full$`SHUTDOWN_DUE_accident_full_IND`)
accident_full$`STILL_SHUTDOWN_IND` <- factor(accident_full$`STILL_SHUTDOWN_IND`)
accident_full$`INJURY_IND` <- factor(accident_full$`INJURY_IND`)
accident_full$`FATALITY_IND` <- factor(accident_full$`FATALITY_IND`)
accident_full$`WILDLIFE_IMPACT_IND` <- factor(accident_full$`WILDLIFE_IMPACT_IND`) 
accident_full$`FISH_AQUATIC_IMPACT_IND` <- factor(accident_full$`FISH_AQUATIC_IMPACT_IND`) 
accident_full$`BIRDS_IMPACT_IND` <- factor(accident_full$`BIRDS_IMPACT_IND`) 
accident_full$`TERRESTRIAL_IMPACT_IND` <- factor(accident_full$`TERRESTRIAL_IMPACT_IND`) 
accident_full$`SOIL_CONTAMINATION` <- factor(accident_full$`SOIL_CONTAMINATION`) 
accident_full$`LONG_TERM_ASSESSMENT` <- factor(accident_full$`LONG_TERM_ASSESSMENT`) 
accident_full$`REMEDIATION_IND` <- factor(accident_full$`REMEDIATION_IND`) 
accident_full$`SURFACE_WATER_REMED_IND` <- factor(accident_full$`SURFACE_WATER_REMED_IND`) 
accident_full$`GROUNDWATER_REMED_IND` <- factor(accident_full$`GROUNDWATER_REMED_IND`) 
accident_full$`SOIL_REMED_IND` <- factor(accident_full$`SOIL_REMED_IND`) 
accident_full$`VEGETATION_REMED_IND` <- factor(accident_full$`VEGETATION_REMED_IND`) 
accident_full$`WILDLIFE_REMED_IND` <- factor(accident_full$`WILDLIFE_REMED_IND`) 
accident_full$`WATER_CONTAM_IND` <- factor(accident_full$`WATER_CONTAM_IND`) 
accident_full$`OCEAN_SEAWATER_IND` <- factor(accident_full$`OCEAN_SEAWATER_IND`) 
accident_full$`SURFACE_CONTAM_IND` <- factor(accident_full$`SURFACE_CONTAM_IND`) 
accident_full$`GROUNDWATER_CONTAM_IND` <- factor(accident_full$`GROUNDWATER_CONTAM_IND`) 
accident_full$`DRINKING_WATER_CONTAM_IND` <- factor(accident_full$`DRINKING_WATER_CONTAM_IND`) 
accident_full$`PRIVATE_WELL_CONTAM_IND` <- factor(accident_full$`PRIVATE_WELL_CONTAM_IND`) 
accident_full$`PUBLIC_WATER_CONTAM_IND` <- factor(accident_full$`PUBLIC_WATER_CONTAM_IND`)
accident_full$`EVACUATION` <- factor(accident_full$`EVACUATION`)


#------------------------------------ 
# In our solution, we define records with TOTAL_COST above the Upper Whisker, i.e., Q3+1.5*IQR, as high cost accident
quartiles <- quantile(accident_full[["TOTAL_COST"]], na.rm = TRUE, probs = c(0.25, 0.75))
IQR_val <- IQR(accident_full[["TOTAL_COST"]],na.rm=TRUE)
upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker for TOTAL_COST

# Create New Categorical Variable, HIGH_COST_ALERT
accident_full[, HIGH_COST_ALERT:=factor(ifelse((TOTAL_COST>=upper_bound),"YES","NO"))] #classify accidents based on their TOTAL_COST
summary(accident_full$HIGH_COST_ALERT)

# ----------------------------------
#Train-Test Split
set.seed(2406)
train <- sample.split(Y = accident_full$HIGH_COST_ALERT, SplitRatio = 0.7)
trainset <- subset(accident_full, train == T)
testset <- subset(accident_full, train == F)

summary(trainset$HIGH_COST_ALERT) #2328 NO, 396 YES
summary(testset$HIGH_COST_ALERT) #997 NO, 170 YES
#this is an imbalance data in the trainset, too many "NO", the recommended balance ratio is at most 2:1

#Balance the trainset
train.bal<-trainset[order(-HIGH_COST_ALERT),][1:1188,] 
#'YES' is smaller than 'NO' alphabetically, after arranging, 396 "YES" will be in front, so take the first 1188 rows, there be 396 "YES" and 792 "NO".

#Check trainset is balanced
summary(train.bal$HIGH_COST_ALERT) #792 NO, 396 YES

#-------------------------------------------------------------------------------


# +---------------------------------+
# | 3.2.1 Logistic Regression Model |
# +---------------------------------+

#Step 1: Fit the Logistic Regression Model using all relevant predictor variables
alert_m <- glm(HIGH_COST_ALERT ~ IYEAR+ON_OFF_SHORE+LOCATION_LATITUDE+LOCATION_LONGITUDE+LOCATION_TYPE+INCIDENT_AREA_TYPE+
                 INTENTIONAL_RELEASE_BBLS+CAUSE+COMMODITY_RELEASED_TYPE+IGNITE_IND+RECOVERED_BBLS+RELEASE_TYPE+EXPLODE_IND+
                 SHUTDOWN_DUE_ACCIDENT_IND+NUM_PUB_EVACUATED+INJURY_IND+NUM_EMP_INJURIES+NUM_CONTR_INJURIES+NUM_WORKER_INJURIES+
                 NUM_GP_INJURIES+INJURE+FATALITY_IND+NUM_EMP_FATALITIES+NUM_CONTR_FATALITIES+NUM_WORKER_FATALITIES+NUM_GP_FATALITIES+
                 FATAL+WILDLIFE_IMPACT_IND+FISH_AQUATIC_IMPACT_IND+BIRDS_IMPACT_IND+SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+
                 REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+
                 WATER_CONTAM_IND+SURFACE_CONTAM_IND+COMMODITY_REACHED_HCA+EVACUATION+UNINTENTIONAL_RELEASE_BBLS+TERRESTRIAL_IMPACT_IND+
                 SOIL_REMED_IND+OCEAN_SEAWATER_IND+GROUNDWATER_CONTAM_IND+response_delay
               ,family = binomial, data = train.bal)

# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 

summary(alert_m) #Abnormal pattern: p-value for all variables are smaller than the significance level of 0.1%

# Investigation:
# This warning may be due to: 
# 1. Complete separation (https://medium.com/geekculture/class-separation-cannot-be-overlooked-in-logistic-regression-f20e58b203eb)
# 2. Small sample with large number of predictor variables: there are rules of thumb: 2.5 observations per predictor (https://stats.stackexchange.com/questions/183320/logistic-glm-with-good-predictors-is-giving-p-values-1)


#Step 2: Find the predictor variable(s) causing complete separation to occur using detect_separation()
library("detectseparation") # https://www.rdocumentation.org/packages/brglm2/versions/0.7.1/topics/detect_separation

m_detect<-glm(HIGH_COST_ALERT ~ IYEAR+ON_OFF_SHORE+LOCATION_LATITUDE+LOCATION_LONGITUDE+LOCATION_TYPE+INCIDENT_AREA_TYPE+
                INTENTIONAL_RELEASE_BBLS+CAUSE+COMMODITY_RELEASED_TYPE+IGNITE_IND+RECOVERED_BBLS+RELEASE_TYPE+EXPLODE_IND+
                SHUTDOWN_DUE_ACCIDENT_IND+NUM_PUB_EVACUATED+INJURY_IND+NUM_EMP_INJURIES+NUM_CONTR_INJURIES+NUM_WORKER_INJURIES+
                NUM_GP_INJURIES+INJURE+FATALITY_IND+NUM_EMP_FATALITIES+NUM_CONTR_FATALITIES+NUM_WORKER_FATALITIES+NUM_GP_FATALITIES+
                FATAL+WILDLIFE_IMPACT_IND+FISH_AQUATIC_IMPACT_IND+BIRDS_IMPACT_IND+SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+
                REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+WATER_CONTAM_IND+
                SURFACE_CONTAM_IND+COMMODITY_REACHED_HCA+EVACUATION+UNINTENTIONAL_RELEASE_BBLS+TERRESTRIAL_IMPACT_IND+SOIL_REMED_IND+
                OCEAN_SEAWATER_IND+GROUNDWATER_CONTAM_IND+response_delay
              ,data=train.bal,family = binomial("logit"),method = "detect_separation")

m_detect
# These values indicate existence of the maximum likelihood estimates. 0 means they are finite (i.e. no complete separation), 
# whereas infinite values are for completely separated predictors (0 for -Inf and 1 for Inf respectively)

#Remove potential completely separated predictors (i.e., with value Inf or -Inf)
alert_m2 <- glm(HIGH_COST_ALERT ~ IYEAR+LOCATION_LATITUDE+LOCATION_LONGITUDE+ INTENTIONAL_RELEASE_BBLS+CAUSE+IGNITE_IND+
                  RECOVERED_BBLS+RELEASE_TYPE+SHUTDOWN_DUE_ACCIDENT_IND+INJURY_IND+SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+
                  REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+
                  COMMODITY_REACHED_HCA+UNINTENTIONAL_RELEASE_BBLS+SOIL_REMED_IND+response_delay, 
                family = binomial, data = train.bal)

#Although we still receive the warning, but now the summary shows different p-values for each variable
summary(alert_m2) 


#Step 3: Remove statistically less significant predictor variable using backward elimination
#https://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
alert_m3 <- step(alert_m2)
summary(alert_m3)


#Step 4: Remove practically irrelevant variables: LOCATION_LATITUDE and LOCATION_LONGITUDE
alert_m4 <- glm(HIGH_COST_ALERT ~ IYEAR+INTENTIONAL_RELEASE_BBLS + CAUSE + IGNITE_IND + 
                  RECOVERED_BBLS + SHUTDOWN_DUE_ACCIDENT_IND + LONG_TERM_ASSESSMENT + 
                  GROUNDWATER_REMED_IND + VEGETATION_REMED_IND + UNINTENTIONAL_RELEASE_BBLS + 
                  response_delay,  
                family = binomial, data = train.bal)
summary(alert_m4)

OR.alert_m4<- exp(coef(alert_m4))
round(OR.alert_m4, 3)

OR.CI.alert_m4 <- exp(confint(alert_m4))
round(OR.CI.alert_m4,3)

#Step 5: Check VIF Values for multicollinearity
vif(alert_m4) #No multicollinearity


#Step 6: Model Evaluation and Interpretation (https://www.statology.org/logistic-regression-in-r/)
##6.1 Model Fit
# R2 value is not defined for logistic regression. Instead, we can compute a metric known as McFadden’s R2, which ranges from 0 to just under 1.
# Values close to 0 indicate that the model has no predictive power. In practice, values over 0.40 indicate that a model fits the data very well.
pscl::pR2(alert_m4)["McFadden"]
#A value of 0.4275519 is quite high for McFadden’s R2, which indicates that our model fits the data very well and has high predictive power.

##6.2Variable Importance
caret::varImp(alert_m4)
#Higher values indicate more importance. These results match up nicely with the p-values from the model. 
#Some important predictor variables are: CAUSE, LONG_TERM_ASSESSMENT, SHUTDOWN_DUE_ACCIDENT_IND, RECOVERED_BBLS and IGNITE_IND


#Step 7: Model Performance Metrics
# Make predictions on the test data
# Logistic Reg Confusion Matrix on Testset
threshold<-0.5
m.prob <- predict(alert_m4, newdata = testset, type = 'response')
m.predict <- ifelse(m.prob > threshold, "YES", "NO")

# Confusion matrix
table1 <- table(Testset.Actual = testset$HIGH_COST_ALERT, logreg.predict = m.predict, deparse.level = 2)
table1
round(prop.table(table1), 3)

# Overall Accuracy
mean(m.predict == testset$HIGH_COST_ALERT)

# ROC curve and AUC: higher the AUC, the better the model is at predicting (https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5)
library(pROC)
roc_obj <- roc(testset$HIGH_COST_ALERT, m.prob)
auc <- auc(roc_obj) #0.66: moderate predictive power
plot(roc_obj, main = paste("AUC =", round(auc, 2))) 

#-------------------------------------------------------------------------------


# +---------------------------------+
# | 3.2.2 CART: Classification Tree |
# +---------------------------------+

set.seed(1) #for 10-fold CV
# Step 1: Grow Tree to max and set cp to 0
alert_CART <- rpart(HIGH_COST_ALERT ~ IYEAR+ON_OFF_SHORE+LOCATION_LATITUDE+LOCATION_LONGITUDE+LOCATION_TYPE+INCIDENT_AREA_TYPE+
                      INTENTIONAL_RELEASE_BBLS+CAUSE+COMMODITY_RELEASED_TYPE+IGNITE_IND+RECOVERED_BBLS+RELEASE_TYPE+EXPLODE_IND+
                      SHUTDOWN_DUE_ACCIDENT_IND+NUM_PUB_EVACUATED+INJURY_IND+NUM_EMP_INJURIES+NUM_CONTR_INJURIES+NUM_WORKER_INJURIES+
                      NUM_GP_INJURIES+INJURE+FATALITY_IND+NUM_EMP_FATALITIES+NUM_CONTR_FATALITIES+NUM_WORKER_FATALITIES+NUM_GP_FATALITIES+
                      FATAL+WILDLIFE_IMPACT_IND+FISH_AQUATIC_IMPACT_IND+BIRDS_IMPACT_IND+SOIL_CONTAMINATION+LONG_TERM_ASSESSMENT+
                      REMEDIATION_IND+SURFACE_WATER_REMED_IND+GROUNDWATER_REMED_IND+VEGETATION_REMED_IND+VEGETATION_REMED_IND+WATER_CONTAM_IND+
                      SURFACE_CONTAM_IND+COMMODITY_REACHED_HCA+EVACUATION+UNINTENTIONAL_RELEASE_BBLS+TERRESTRIAL_IMPACT_IND+SOIL_REMED_IND+
                      OCEAN_SEAWATER_IND+GROUNDWATER_CONTAM_IND+response_delay, 
                    data = train.bal, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(alert_CART)
plotcp(alert_CART)


# Step 2: Prune Tree to min
# 2.1: Find Optimal cp:
#store the cptable
dt<-data.table(alert_CART$cptable)
#number the sequence of the trees
dt[,index:=1:nrow(dt)]
#find out minimum index where xerror is min
min_cp_index<-min(dt[(xerror+xstd)==min(xerror+xstd),index])
#find the errorcap
errorcap<-dt[min_cp_index,xerror+xstd]
#find out the optimal index for the cp
optimal_cp_index <- min(dt[(xerror < errorcap), index])
#Find the geometric mean of the cp for that index and one cp appearing before it
cp.opt=sqrt(dt[index==optimal_cp_index,CP]*dt[index==optimal_cp_index-1,CP]) #0.01115121


# 2.2: Prune the max tree using optimal.cp
alert_CART_optimal <- prune(alert_CART, cp = cp.opt)

print(alert_CART_optimal)
rpart.plot(alert_CART_optimal)
summary(alert_CART_optimal)
alert_CART_optimal$variable.importance


# Step 3: Evaluate the CART Model
cart.predict <- predict(alert_CART_optimal, newdata = testset, type = "class")
table2 <- table(Testset.Actual = testset$HIGH_COST_ALERT, cart.predict, deparse.level = 2)
table2
round(prop.table(table2), 3)

# Overall Accuracy
mean(cart.predict == testset$HIGH_COST_ALERT)


#----------------------------------------------Thank You-------------------------------------------------------
