## Raw Data
Storage of all raw biologging data files for adult females, adult males, and 99P (intersex seal). Adult female data is in multiple storage modes (from multiple years), using both .nc files and .csv files.

Contents: 
2004-2020_AdultFemaleData 
- Contains folders for 2004 - 2020 processed data from Costa et al., 2024
- Every file is a single individual (file structure is "TOPPID_TrackTDR_Processed.nc")
- You need a wrapper in R to download the .nc files
- "NES_Tracking_Diving_Metadata.csv" contains all metadata information for these years (organized in main RawData) 

Adult_Male_Metadata
- Metadata file for these individuals is organized PER individual, see biologging curation code for merging

Dive_Data
- Contains all dive data for adult females from 2021 - 2023 ("21-23 ... AdultFemales.csv")
- All adult male data ("*_Complete.csv")
- Data for intersex seal ("2025033_...*_QC.csv)
- Data for one PM2025 female ("2025019...*_QC.csv")
- female_dives_all.csv is the aggregated adult female diving data for 2004 - 2023

Track_Data
- Contains all track data for adult females from 2021 - 2023 ("21-23 ... AdultFemales.csv")
- All adult male data ("*_Interp.csv")
- Data for intersex seal (Processed: "2025033_...*_aniMotum_crw.csv", aniMotum load in file: "2025033_...*_aniMotum.csv")
- Data for one PM2025 female (Processed: "2025019_...*_aniMotum_crw.csv", aniMotum load in file: "2025019_...*_aniMotum.csv")
- female_tracks_all.csv is the aggregated adult female tracking data for 2004 - 2023

