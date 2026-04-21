##### 99P Tracking and Diving Data Pre-Processing ##### 

##### HOUSEKEEPING #####
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(readr)

setwd("~/Documents/PhD Documents/99P Analysis/99P_TrackingDiving_Analysis")

## LOADING IN ADULT FEMALE DATA ## 
adult_female_tracking <- read.csv("./Tracking/Track_Data/female_ad_tracks_all.csv", stringsAsFactors = FALSE)
adult_female_dives <- read.csv("./Diving/Dive_Data/female_ad_dives_all.csv", stringsAsFactors = FALSE)


##### HELPER FUNCTIONS #####

# Read and combine multiple CSV files, tagging each row with its source file.
read_and_bind_csvs <- function(path, pattern, col_types = NULL) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    stop(paste("No files found in", path, "matching", pattern))
  }
  
  map_dfr(files, function(f) {
    read_csv(
      f,
      col_types = col_types,
      na = c("", "NA", "NaN"),
      show_col_types = FALSE
    ) %>%
      mutate(source_file = basename(f))
  })
}

# Parse datetimes consistently in UTC.
parse_dt_utc <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  
  parse_date_time(
    x,
    orders = c(
      "Y-m-d H:M:S",
      "Y-m-d H:M",
      "Y-m-d",
      "m/d/y H:M:S",
      "m/d/y H:M",
      "m/d/y",
      "m/d/Y H:M:S",
      "m/d/Y H:M",
      "m/d/Y"
    ),
    tz = "UTC"
  )
}

# Standardize datetime and character columns across dataframes.
standardize_cols <- function(df, datetime_cols, character_cols) {
  dt_cols <- intersect(names(df), datetime_cols)
  char_cols <- intersect(names(df), character_cols)
  
  df[dt_cols] <- lapply(df[dt_cols], function(x) {
    if (inherits(x, c("POSIXct", "POSIXt"))) return(x)
    if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
    
    x_chr <- trimws(as.character(x))
    x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
    
    if (all(is.na(x_chr))) {
      return(as.POSIXct(rep(NA_character_, length(x_chr)), tz = "UTC"))
    }
    
    parse_dt_utc(x_chr)
  })
  
  df %>%
    mutate(across(any_of(char_cols), as.character))
}

# Drop columns that are entirely NA.
drop_all_na_cols <- function(df) {
  df %>% dplyr::select(where(~ !all(is.na(.))))
}

# Add metadata columns without overwriting columns already present.
add_metadata_columns <- function(obs_df, meta_df, key = "TOPPID") {
  obs_df[[key]]  <- as.character(obs_df[[key]])
  meta_df[[key]] <- as.character(meta_df[[key]])
  
  meta_df <- meta_df %>%
    distinct(.data[[key]], .keep_all = TRUE)
  
  new_cols <- setdiff(names(meta_df), names(obs_df))
  
  obs_df %>%
    left_join(
      meta_df %>% dplyr::select(all_of(c(key, new_cols))),
      by = key
    )
}


##### COMMON COLUMN GROUPS #####

datetime_cols <- c(
  "time",
  "date",
  "DateTime",
  "DeployDate",
  "DepartDate",
  "ArrivalDate",
  "ArriveDate",
  "Deployment_Departure_Datetime",
  "Deployment_Arrival_Datetime",
  "dep_time",
  "arr_time",
  "dive_time",
  "Time"
)

character_cols <- c(
  "TOPPID",
  "PTTID",
  "Animal_Sex",
  "Sex",
  "Deployment_Trip",
  "trip"
)

##### LOAD ADULT FEMALE DATA #####

adult_female_dives <- adult_female_dives %>%
  dplyr::select(
    -X,
    
    # duplicated dive metrics
    -Maxdepth,
    -Dduration,
    -Botttime,
    -DescTime,
    -AscTime,
    -PDI,
    -DWigglesDesc,
    -DWigglesBott,
    -DWigglesAsc,
    -BottRange,
    -Efficiency,
    -IDZ,
    
    # duplicated coordinates
    -Lat,
    -Lon,
    
    # redundant time components
    -Year,
    -Month,
    -Day,
    -Hour,
    -Min,
    -Sec
  ) %>%
  drop_all_na_cols()

##### LOAD ADULT MALE DATA #####

metadata_cols <- cols(
  .default = col_character(),
  DepartDate = col_double(),
  ArriveDate = col_double(),
  DepartLat = col_double(),
  DepartLon = col_double(),
  ArriveLat = col_double(),
  ArriveLon = col_double(),
  BirthYear = col_double()
)

adult_male_metadata <- read_and_bind_csvs(
  path = "./Tracking/Track_Data/Adult_Male_Data_csvs",
  pattern = "MetaData\\.csv$",
  col_types = metadata_cols
) %>%
  mutate(
    DepartDate = as.POSIXct(
      (DepartDate - 719529) * 86400,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    ArriveDate = as.POSIXct(
      (ArriveDate - 719529) * 86400,
      origin = "1970-01-01",
      tz = "UTC"
    )
  )

adult_male_tracking <- read_and_bind_csvs(
  path = "./Tracking/Track_Data",
  pattern = "Best_Interp\\.csv$"
)

adult_male_diving <- read_and_bind_csvs(
  path = "./Diving/Dive_Data",
  pattern = "DiveStat_Complete\\.csv$"
) %>%
  mutate(
    TOPPID = str_extract(source_file, "^\\d+")
  )

##### LOAD 99P DATA #####

j699_tracking <- read.csv(
  "./Tracking/Track_Data/2025033_116855_GPS_Argos_AniMotum_crw.csv",
  stringsAsFactors = FALSE
)

j699_diving <- read.csv(
  "./Diving/Dive_Data/2025033_25A0160_DAprep_full_iknos_DiveStat_QC.csv",
  stringsAsFactors = FALSE
)

j699_metadata <- read.csv("MetaDataAll.csv")
j699_foragingsuccess <- read.csv("foragingsuccess.csv")

##### MERGE 99P METADATA #####

j699_combined <- j699_metadata %>%
  filter(FieldID == "J699") %>%  # restrict metadata to 99P
  left_join(
    j699_foragingsuccess %>%
      dplyr::select(
        TOPPID,
        DaysAtSea,
        DeployMass,
        Deploy.SL,
        RecoverMass,
        Recover.SL
      ),
    by = "TOPPID"
  )

##### ENSURE TOPPID EXISTS IN 99P DATA #####

if (!"TOPPID" %in% names(j699_tracking)) {
  j699_tracking <- j699_tracking %>%
    mutate(TOPPID = as.character(j699_combined$TOPPID[1]))
}

if (!"TOPPID" %in% names(j699_diving)) {
  j699_diving <- j699_diving %>%
    mutate(TOPPID = as.character(j699_combined$TOPPID[1]))
}

##### ADD METADATA TO TRACKING AND DIVING DATA #####

adult_male_tracking <- add_metadata_columns(
  obs_df = adult_male_tracking,
  meta_df = adult_male_metadata,
  key = "TOPPID"
)

adult_male_diving <- add_metadata_columns(
  obs_df = adult_male_diving,
  meta_df = adult_male_metadata,
  key = "TOPPID"
)

j699_tracking <- add_metadata_columns(
  obs_df = j699_tracking,
  meta_df = j699_combined,
  key = "TOPPID"
)

j699_diving <- add_metadata_columns(
  obs_df = j699_diving,
  meta_df = j699_combined,
  key = "TOPPID"
)

##### ADD SOURCE LABELS #####

adult_female_dives    <- adult_female_dives %>% mutate(dataset_source = "female")
adult_male_diving     <- adult_male_diving %>% mutate(dataset_source = "male")
j699_diving           <- j699_diving %>% mutate(dataset_source = "99P")

adult_female_tracking <- adult_female_tracking %>% mutate(dataset_source = "female")
adult_male_tracking   <- adult_male_tracking %>% mutate(dataset_source = "male")
j699_tracking         <- j699_tracking %>% mutate(dataset_source = "99P")

##### STANDARDIZE COLUMN TYPES #####

adult_female_tracking <- standardize_cols(
  adult_female_tracking,
  datetime_cols,
  character_cols
)

adult_male_tracking <- standardize_cols(
  adult_male_tracking,
  datetime_cols,
  character_cols
)

j699_tracking <- standardize_cols(
  j699_tracking,
  datetime_cols,
  character_cols
)

adult_female_dives <- standardize_cols(
  adult_female_dives,
  datetime_cols,
  character_cols
)

adult_male_diving <- standardize_cols(
  adult_male_diving,
  datetime_cols,
  character_cols
)

j699_diving <- standardize_cols(
  j699_diving,
  datetime_cols,
  character_cols
)

##### MERGE FEMALE, MALE, AND 99P INTO CENTRALIZED DATASETS #####

all_tracking <- bind_rows(
  adult_female_tracking,
  adult_male_tracking,
  j699_tracking
)

all_diving <- bind_rows(
  adult_female_dives,
  adult_male_diving,
  j699_diving
)

##### CLEAN TRACKING DATA #####

all_tracking <- all_tracking %>%
  dplyr::select(
    -X,
    -Deployment_ManipulationType,
    -Deployment_Manipulation,
    -ResightData_LastObservationOfDeploymentSeason,
    -ResightData_FirstObservationOfRecoverySeason,
    -AnyWeirdness__Eg__BigWCHDataGaps_UnreasonablyDeepDives_,
    -ArgosData_LastOn_LandHitOfDeploymentSeason,
    -ArgosData_FirstOn_LandHitOfRecoverySeason_forRecoveredSealsOnly_,
    -WCHRecoveredData_FirstDive_forRecoveredSealsOnly_,
    -WCHRecoveredData_LastDive_forRecoveredSealsOnly_
  ) %>%
  mutate(across(any_of(character_cols), as.character))

all_tracking_clean <- all_tracking %>%
  mutate(
    depart_datetime = coalesce(Deployment_Departure_Datetime, DepartDate, dep_time),
    arrival_datetime = coalesce(Deployment_Arrival_Datetime, ArrivalDate, ArriveDate, arr_time),
    depart_loc = coalesce(Deployment_Departure_Loc, DepartLoc, depart_loc, depart_loc_all),
    arrive_loc = coalesce(Deployment_Arrival_Loc, ArriveLoc, arrive_loc, arrive_loc_all),
    animal_id = coalesce(Animal_ID, AnimalID, FieldID),
    birth_year = coalesce(Animal_BirthYear, BirthYear),
    sex = coalesce(Animal_Sex, Sex),
    trip = coalesce(Deployment_Trip, trip),
    deployment_year = coalesce(Deployment_Year, year, year_info),
    track_lat = coalesce(lat, Lat),
    track_lon = coalesce(lon, Lon),
    track_speed = coalesce(speed, s)
  ) %>%
  dplyr::select(
    # duplicate datetime columns
    -Deployment_Departure_Datetime,
    -DepartDate,
    -dep_time,
    -Deployment_Arrival_Datetime,
    -ArrivalDate,
    -ArriveDate,
    -arr_time,
    
    # duplicate location columns
    -Deployment_Departure_Loc,
    -DepartLoc,
    -depart_loc_all,
    -Deployment_Arrival_Loc,
    -ArriveLoc,
    -arrive_loc_all,
    -depart_loc,
    -arrive_loc,
    -lat,
    -lon,
    -Lat,
    -Lon,
    
    # duplicate ID / demographic columns
    -Animal_ID,
    -AnimalID,
    -FieldID,
    -Animal_BirthYear,
    -BirthYear,
    -Animal_Sex,
    -Sex,
    
    # duplicate trip / year columns
    -Deployment_Trip,
    -year,
    -year_info,
    -Deployment_Year,
    -Deployment_Argos_ResightDiff,
    -Deployment_Biologging_ResightDiff
  )

##### CLEAN DIVING DATA #####

all_diving <- all_diving %>%
  dplyr::select(
    -Deployment_ManipulationType,
    -Deployment_Manipulation,
    -ResightData_LastObservationOfDeploymentSeason,
    -ResightData_FirstObservationOfRecoverySeason,
    -AnyWeirdness__Eg__BigWCHDataGaps_UnreasonablyDeepDives_,
    -ArgosData_LastOn_LandHitOfDeploymentSeason,
    -ArgosData_FirstOn_LandHitOfRecoverySeason_forRecoveredSealsOnly_,
    -WCHRecoveredData_FirstDive_forRecoveredSealsOnly_,
    -WCHRecoveredData_LastDive_forRecoveredSealsOnly_,
    -Deployment_Argos_ResightDiff,
    -Deployment_Biologging_ResightDiff
  ) %>%
  standardize_cols(datetime_cols, character_cols)

all_diving_clean <- all_diving %>%
  mutate(
    # timing
    Deployment_Departure_Datetime = coalesce(Deployment_Departure_Datetime, DepartDate),
    Deployment_Arrival_Datetime = coalesce(Deployment_Arrival_Datetime, ArrivalDate, ArriveDate),
    dive_time = coalesce(dive_time, DateTime, Time),
    
    # locations
    Deployment_Departure_Loc = coalesce(Deployment_Departure_Loc, DepartLoc, depart_loc),
    Deployment_Arrival_Loc = coalesce(Deployment_Arrival_Loc, ArriveLoc, arrive_loc),
    
    # identity
    Animal_ID = coalesce(Animal_ID, AnimalID, FieldID),
    Animal_BirthYear = coalesce(Animal_BirthYear, BirthYear),
    Animal_Sex = coalesce(Animal_Sex, Sex),
    Deployment_Trip = coalesce(Deployment_Trip, trip),
    Deployment_Year = coalesce(Deployment_Year, year, year_info),
    
    # coordinates
    lat = coalesce(lat, Latitude),
    lon = coalesce(lon, Longitude),
    
    # dive metrics
    max_depth = coalesce(max_depth, Maxdepth),
    duration = coalesce(duration, Dduration),
    bott_time = coalesce(bott_time, Botttime),
    desc_time = coalesce(desc_time, DescTime),
    asc_time = coalesce(asc_time, AscTime),
    pdi = coalesce(pdi, PDI),
    wig_desc = coalesce(wig_desc, DWigglesDesc),
    wig_bott = coalesce(wig_bott, DWigglesBott),
    wig_asc = coalesce(wig_asc, DWigglesAsc),
    bott_range = coalesce(bott_range, BottRange),
    efficiency = coalesce(efficiency, Efficiency),
    idz = coalesce(idz, IDZ)
  ) %>%
  dplyr::select(
    # duplicate timing columns
    -DepartDate,
    -ArrivalDate,
    -ArriveDate,
    -DateTime,
    -Time,
    
    # duplicate location columns
    -DepartLoc,
    -ArriveLoc,
    -depart_loc,
    -arrive_loc,
    
    # duplicate identity columns
    -AnimalID,
    -FieldID,
    -BirthYear,
    -Sex,
    
    # duplicate trip/year columns
    -trip,
    -year,
    -year_info,
    
    # duplicate coordinate columns
    -Latitude,
    -Longitude,
    
    # duplicate dive metrics
    -Maxdepth,
    -Dduration,
    -Botttime,
    -DescTime,
    -AscTime,
    -PDI,
    -DWigglesDesc,
    -DWigglesBott,
    -DWigglesAsc,
    -BottRange,
    -Efficiency,
    -IDZ
  )

##### WRITE OUTPUTS #####

write.csv(all_tracking_clean, "./Raw_Data/all_groups_tracking.csv", row.names = FALSE)
write.csv(all_diving_clean, "./Raw_Data/all_groups_diving.csv", row.names = FALSE)