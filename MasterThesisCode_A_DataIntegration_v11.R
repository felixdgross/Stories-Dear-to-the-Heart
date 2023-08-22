## 1 SETUP
##


# data frame Glossary: 
# dat_phy = physiological data scraped from Acknowledge
# dat_que = questionnaire data from E1.robo_wide
# dat_combined = merged data frames dat_phy + dat_que (+ in a next step e2 data is included as well which contains among other transportation scale sub scales))
# dat_combined1 = dat_combined + fae_dat (fae = FocusAreaExtraction)

# dat_comprehensive = dat_combined1 but only for focus areas; it is the comprehensive data set used for all down-stream analyses 



## install and load packages 
install_and_load_packages <- function() 
{
  # List of packages
  packages <- c("readxl", "writexl", "tidyverse", "dplyr", "purrr", "fuzzyjoin", "rlang",
                "compare", "lme4", "lmerTest", "haven", "psych","lmtest", "car", "ggplot2")
  
  # Check if each package is installed, install if not
  for (pkg in packages) 
  {
    if (!requireNamespace(pkg, quietly = TRUE)) 
    {
      install.packages(pkg)
    }
  }
  
  # Load all the packages
  for (pkg in packages) 
  {
    if (requireNamespace(pkg, quietly = TRUE)) 
    {
      library(pkg, character.only = TRUE)
    }
  }
}

# error mitigation (remove all packages)
#for (pkg in packages) {
 # remove.packages(pkg)
#}

# Call the function to install and load packages
install_and_load_packages()


# Loaded packages
loaded_packages <- search()
loaded_package_names <- gsub("^package:", "", loaded_packages)
loaded_package_names

## 2 OPTAINING MAIN DATA FILE BY MERGING DIFFERENT DATA SOURCES 
##

# Specify the path to the directory containing the files
dir_path <- "C:/Users/Thinkpad/Documents/_MasterThesis/Auswertung"

# List all files in the directory
all_files <- list.files(path = dir_path, full.names = TRUE)

# Filter for the relevant files
edabpm_files <- sort(grep("EDABPM", all_files, value = TRUE))
ecg_files <- sort(grep("ECG", all_files, value = TRUE))

# Initialize an empty data frame
dat_phy <- data.frame()
names(edabpm_df)

# Define a custom match function
# Define tolerance
tolerance <- 1
close_match <- function(x, y) {
  abs(x - y) <= tolerance
} 

# Loop over the files
for (i in seq_along(edabpm_files)) {
  
  # Read the two files corresponding to the same person
  edabpm_df <- read_excel(edabpm_files[i], skip = 2)
  ecg_df <- read_excel(ecg_files[i], skip = 2)
  
  # Merge the two data frames by 'Timestamp'
  merged_df <- fuzzy_inner_join(edabpm_df, ecg_df, 
                             by = c("Time" = "Left Edge (seconds)"),
                             match_fun = list(close_match))
  
  # Get the VP number from the file name
  vp_number <- gsub("\\D", "", basename(edabpm_files[i]))
  vp_number
  
  # Add a new column to the merged data frame with the VP number
  merged_df$VP_number <- vp_number
  
  # Append the merged data frame to 'dat_phy'
  dat_phy <- bind_rows(dat_phy, merged_df)
}

View(dat_phy)

# Remove duplicate rows
dat_phy <- distinct(dat_phy)

# Order by VP_number
dat_phy <- arrange(dat_phy, VP_number)

# coalesce Mean_CH_7 and Mean_CH_8 
dat_phy <- dat_phy %>%
  mutate(`Mean (CH 7)` = ifelse(is.na(`Mean (CH 7)`), `Mean (CH 8)`, `Mean (CH 7)`))

# now remove the obsolete "Mean (CH 8)" column
dat_phy$`Mean (CH 8)` <- NULL

# as 'Time' equals 'left edge (seconds)' remove time
dat_phy$`Time` <- NULL

# rearrange columns in a meaningful order
View(dat_phy)
dat_phy <- dat_phy %>%
  dplyr::select("VP_number", "Epoch", "Right Edge (seconds)", "Mean (CH 6)", "Mean (CH 7)", everything()) 

# rename
dat_phy <- dat_phy %>%
  rename("Skin Conductance Rate" = `Mean (CH 6)`,
         "Heart Rate" = `Mean (CH 7)`)

# remove all doubling rows, created by mistake
dat_phy <- dat_phy %>%
  filter(!is.na(`Left Edge (seconds)`))

# View the combined data
View(dat_phy)
colnames(dat_phy)

# safe as CSV
write.csv(dat_phy, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Auswertung/dat_phy_raw.csv")

# Read in data
dat_phy <- read.csv("C:/Users/Thinkpad/Documents/_MasterThesis/Auswertung/dat_phy_raw.csv", header = TRUE, sep = ",")
dat_que <- read.csv("C:/Users/Thinkpad/Documents/_MasterThesis/Daten/E1.robo_wide.csv", header = TRUE, sep = ";")

# control
colnames(dat_que)
colnames(dat_phy)

str(dat_que)
str(dat_phy)

View(dat_que)
View(dat_phy)

# rename faultily imported columnheads 
colnames(dat_que)[colnames(dat_que) == "ï..subject"] <- "subject"

# Convert columns to character to make sure they match
dat_que$subject <- as.character(dat_que$subject)
dat_phy$VP_number <- as.character(dat_phy$VP_number)

# Identifying rows in dat_que that don't have matching VP_number in dat_phy
dat_que_not_in_phy <- dat_que[!dat_que$subject %in% dat_phy$VP_number,]
View(dat_que_not_in_phy)

# Identifying rows in dat_phy that don't have matching subject in dat_que
dat_phy_not_in_que <- dat_phy[!dat_phy$VP_number %in% dat_que$subject,]

# Print the identified rows 
print(dat_que_not_in_phy) #61,100,112
print(dat_phy_not_in_que)

# control
print(paste("Number of rows in dat_que not in dat_phy: ", nrow(dat_que_not_in_phy)))
print(paste("Number of rows in dat_phy not in dat_que: ", nrow(dat_phy_not_in_que)))

# Delete the rows in dat_que (61)
dat_que <- dat_que[dat_que$subject %in% dat_phy$VP_number,]

# Delete the rows in dat_phy
dat_phy <- dat_phy[dat_phy$VP_number %in% dat_que$subject,]

# Merge the two data frames, keeping all rows
dat_combined_unsorted <- merge(dat_que, dat_phy, by.x = "subject", by.y = "VP_number", all = TRUE)
dat_combined <- dat_combined_unsorted[order(dat_combined_unsorted$X), ]

# Remove row names
rownames(dat_combined) <- NULL

#add e2 data from ORF repository  with transportation sub scales 

# Read in data
# open SAV
e2 <- read_sav("C:/Users/Thinkpad/Documents/_MasterThesis/Daten/data_e2_wide.sav")

# control
count_of_ones <- sum(e2$inout == 1)
subject_values <- e2$subject[e2$inout == 1]
subject_values
count_of_ones

colnames(dat_combined)
colnames(e2)

str(dat_combined)
str(e2)

View(dat_combined)
View(e2)

# Convert columns to character to make sure they match
dat_combined$subject <- as.character(dat_combined$subject)
e2$subject <- as.character(e2$subject)

# Identifying rows in dat_combined that don't have matching subject in e2
dat_combined_not_in_phy <- dat_combined[!dat_combined$subject %in% e2$subject,]
View(dat_combined_not_in_phy)

# Identifying rows in e2 that don't have matching subject in dat_combined
e2_not_in_que <- e2[!e2$subject %in% dat_combined$subject,]
View(e2_not_in_que)

# Print the identified rows 
print(dat_combined_not_in_phy) #61,100,112
print(e2_not_in_que)

# control
print(paste("Number of rows in dat_combined not in e2: ", nrow(dat_combined_not_in_phy))) #120, 256, 269
print(paste("Number of rows in e2 not in dat_combined: ", nrow(e2_not_in_que)))

# Delete the rows in dat_combined 
dat_combined <- dat_combined[dat_combined$subject %in% e2$subject,]

# Delete the rows in e2 (120,256,269)
e2 <- e2[e2$subject %in% dat_combined$subject,]

# Merge the two data frames, keeping all rows
dat_combined_unsorted <- merge(dat_combined, e2, by.x = "subject", by.y = "subject", all = TRUE)
dat_combined <- dat_combined_unsorted[order(dat_combined_unsorted$X), ]


# Remove row names
rownames(dat_combined) <- NULL

# Move X column to front
dat_combined <- dat_combined[, c('X', setdiff(names(dat_combined), 'X'))]

# control
column_names <- colnames(dat_combined)
print(column_names)

View(dat_combined)

# Rename the falsely imported columheads
dat_combined <- rename(dat_combined, 
                       LeftSec = `Left.Edge..seconds.`, 
                       RightSec = `Right.Edge..seconds.`, 
                       SCR = `Skin.Conductance.Rate`, 
                       HeartRate = `Heart.Rate`, 
                       RMSSD = `RMSSD..msecs.`, 
                       SDSD = `SDSD..msecs.`, 
                       pNN50 = `pNN50....`)

# move them to the front for better handling
new_order <- c("X", "subject", "Epoch", "LeftSec", "RightSec", "SCR", "HeartRate", "RMSSD", "SDSD", "pNN50", 
               setdiff(names(dat_combined), c("LeftSec", "RightSec", "SCR", "HeartRate", "RMSSD", "SDSD", "pNN50")))

# reorder the dataframe
  dat_combined <- dat_combined[new_order]

# Remove columns that start with 'X', but keep the column 'X'
  # Save the 'X' column
  X_column <- dat_combined$X
  X_column
  
  # Remove columns that start with 'X'
  dat_combined <- dat_combined %>% 
    select(-starts_with('X'))
  
  # Add the 'X' column back to the dataframe
  dat_combined$X <- X_column
  
# Move X column to front
dat_combined <- dat_combined[, c('X', setdiff(names(dat_combined), 'X'))]  
  
# Write the merged data frames to csv
write.csv(dat_combined, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_combined.csv", row.names = TRUE)




##
#Adding begin and end of each story plus HRV values for each of the three story parts 
dat_combined <- read.csv("C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_combined.csv")


# Extract all file paths
all_files <- list.files(path = "C:/Users/Thinkpad/Documents/_MasterThesis/Auswertung", full.names = TRUE, recursive = TRUE)

# Filter for the relevant "FAE" files
fae_files <- sort(grep("FAE", all_files, value = TRUE))

# Function to process a single file and return a data frame with one row of extracted data
process_file <- function(file_path) {
  df <- read_excel(file_path, skip = 2)
  
  # Extract the desired information based on our previous discussions
  total_rows <- nrow(df)
  
  if (total_rows < 3) {
    return(data.frame(VP_number = NA, Pt_0_begin = NA, Pt_1_begin = NA, Pt_2_begin = NA, Pt_2_end = NA))
  }
  
  Pt_0_begin <- df[total_rows - 2, "Left Edge (seconds)"]
  Pt_1_begin <- df[total_rows - 1, "Left Edge (seconds)"]
  Pt_2_begin <- df[total_rows, "Left Edge (seconds)"]
  Pt_2_end <- df[total_rows, "Right Edge (seconds)"]
  
  RMSSD_mean_baseline <- df$`RMSSD (msecs)`[total_rows - 4]
  RMSSD_mean_Pt_0 <- df$`RMSSD (msecs)`[total_rows - 2]
  RMSSD_mean_Pt_1 <- df$`RMSSD (msecs)`[total_rows - 1]
  RMSSD_mean_Pt_2 <- df$`RMSSD (msecs)`[total_rows]
  
  
  
  # Extract VP number from the file name
  vp_number <- gsub("\\D", "", basename(file_path))
  
  return(data.frame(VP_number = vp_number, 
                    Pt_0_begin = Pt_0_begin, 
                    Pt_1_begin = Pt_1_begin, 
                    Pt_2_begin = Pt_2_begin, 
                    Pt_2_end = Pt_2_end,
                    
                    RMSSD_mean_baseline = RMSSD_mean_baseline,
                    RMSSD_mean_Pt_0 = RMSSD_mean_Pt_0,
                    RMSSD_mean_Pt_1 = RMSSD_mean_Pt_1,
                    RMSSD_mean_Pt_2 = RMSSD_mean_Pt_2
                    ))
  
}

# Apply the process_file function to each file and combine results into a single data frame
fae_df <- do.call(rbind, lapply(fae_files, process_file))

# Update column names starting from the second column (index 2)
colnames(fae_df)[2:5] <- c("Pt_0_begin", "Pt_1_begin", "Pt_2_begin", "Pt_2_end")
View(fae_df)

# remove leading zeros before VP
fae_df[[1]] <- sub("^0+", "", fae_df[[1]])
View(fae_df)

write.csv(fae_df, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Daten/FocusAreaExtraction.csv", row.names = TRUE)

# Convert columns to character to make sure they match
dat_combined$subject <- as.character(dat_combined$subject)
fae_df$VP_number <- as.character(fae_df$VP_number)

# Identifying rows in dat_combined that don't have matching VP_number in fae_df
dat_combined_not_in_fae_df <- dat_combined[!dat_combined$subject %in% fae_df$VP_number,]
View(dat_combined_not_in_fae_df)

# Identifying rows in fae_df that don't have matching subject in dat_combined
fae_df_not_in_dat_combined <- fae_df[!fae_df$VP_number %in% dat_combined$subject,]


# Print the identified rows 
View(dat_combined_not_in_fae_df) #25
View(fae_df_not_in_dat_combined)    #120,256,269

# control
print(paste("Number of rows in dat_combined not in fae_df: ", nrow(dat_combined_not_in_phy)))
print(paste("Number of rows in fae_df not in dat_combined: ", nrow(fae_df_not_in_que)))

# Delete the rows in dat_combined
dat_combined <- dat_combined[dat_combined$subject != 25, ]

# Delete the rows in fae_df
fae_df <- fae_df[!fae_df$VP_number %in% c(120, 256, 269), ]


# Merge the two data frames, keeping all rows
dat_combined1_unsorted <- merge(dat_combined, fae_df, by.x = "subject", by.y = "VP_number", all = TRUE)
dat_combined1 <- dat_combined1_unsorted[order(dat_combined1_unsorted$X), ]


# delete clutter
dat_combined1 <- dat_combined1[dat_combined1$subject != 25, ]
dat_combined1 <- subset(dat_combined1, select = -c( X))

View(dat_combined1)

# control
result_df <- dat_combined1 %>%
  group_by(subject) %>%
  summarise(Pt_2_end = first(Pt_2_end))

result_df$subject <- as.numeric(result_df$subject)
result_df <- result_df[order(result_df$subject), ]
View(result_df)

write.csv(dat_combined1, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_combined1.csv", row.names = FALSE)



dat_combined1 <- read.csv("C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_combined1.csv")


#get focus areas only
dat_comprehensive <- dat_combined1[!(dat_combined1$RightSec < dat_combined1$Pt_0_begin | dat_combined1$RightSec > dat_combined1$Pt_2_end), ]
View(dat_comprehensive)

#epoch count
epochs_count <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(epoch_count = n())
View(epochs_count)

# replace SCR through proper name
colnames(dat_comprehensive)[colnames(dat_comprehensive) == "SCR"] <- "SCL_sqrt_M"

#compute relevant TS subscales
dat_comprehensive <- dat_comprehensive %>%
  mutate(cognitive_ts = (ts.sf1 + ts.sf2) / 2,
         emotional_ts = ts.sf4, 
         imaginative_ts = (ts.sf5 + ts.sf6) / 2,
         general_ts = (ts.sf1 + ts.sf2 + ts.sf3 + ts.sf4 + ts.sf5 + ts.sf6) / 6
  )

#Data Transformation
#correct with subject Baseline
dat_comprehensive$HR.M.apf 

#HR
dat_comprehensive$HR.M.apf  <- as.character(as.character(dat_comprehensive$HR.M.apf ))
dat_comprehensive$HR.M.apf  <- gsub(",", ".", dat_comprehensive$HR.M.apf )
dat_comprehensive$HR.M.apf  <- as.numeric(as.character(dat_comprehensive$HR.M.apf ))

dat_comprehensive$HeartRate <- as.numeric(as.character(dat_comprehensive$HeartRate))


dat_comprehensive$HeartRate_corrected  <- dat_comprehensive$HeartRate - dat_comprehensive$HR.M.apf

#RMSSD
dat_comprehensive$RMSSD <- as.numeric(as.character(dat_comprehensive$RMSSD))
dat_comprehensive$RMSSD_mean_baseline <- as.numeric(as.character(dat_comprehensive$RMSSD_mean_baseline))

dat_comprehensive$RMSSD_corrected  <- dat_comprehensive$RMSSD - dat_comprehensive$RMSSD_mean_baseline

#standardize SCL
dat_comprehensive <- dat_comprehensive %>%
  group_by(subject) %>%
  mutate(subject_mean_SCL = mean(SCL_sqrt_M),
         centered_SCL = SCL_sqrt_M - subject_mean_SCL,
         sd_SCL = sd(SCL_sqrt_M),
         standardized_SCL = (SCL_sqrt_M - subject_mean_SCL) / sd_SCL) %>%
  ungroup()

dat_comprehensive$subject_mean_SCL

#delete all data not in e2
keep_ids <- c(2, 3, 5, 9, 10, 11, 12, 15, 19, 24, 25, 26, 31, 32, 33, 34, 35, 38, 42, 44, 
              45, 47, 55, 57, 58, 59, 60, 62, 66, 69, 70, 72, 73, 74, 75, 77, 82, 83, 84,
              85, 88, 91, 93, 94, 98, 100, 101, 103, 108, 110, 111, 114, 115, 116, 117, 
              119, 120, 121, 122, 123, 128, 129, 130, 132, 135, 137, 138, 139, 141, 142,
              202, 203, 205, 206, 214, 217, 219, 222, 225, 226, 227, 231, 232, 234, 235,
              238, 240, 242, 244, 248, 252, 256, 257, 258, 259, 261, 262, 263, 264, 265,
              266, 267, 268, 269, 270, 272, 275, 277)

# Subset the dataframe to only include rows with the subject IDs in the list
dat_comprehensive <- dat_comprehensive[dat_comprehensive$subject %in% keep_ids, ]

# Count the number of unique subject IDs left
num_unique_subjects <- length(unique(dat_comprehensive$subject))
print(num_unique_subjects)



#Complete data
write.csv(dat_comprehensive, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_comprehensive.csv", row.names = FALSE)
View(dat_comprehensive)

