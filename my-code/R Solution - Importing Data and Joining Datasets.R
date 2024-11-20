# Intermediate Assignment 1: Importing and Joining Data
# November 2024
# Your Name Here


#### Import packages ---------------------------------------------------------
library(tidyverse)      # Import tidyverse for data processing and more!
library(readxl)         # Import library that can read Excel spreadsheets from the web!
library(jsonlite)       # Convert JSON file to R dataframe


#### Part 1: Food Environment Atlas ---------------------------------------------------------
# Use paste() to concatenate the working directory filepath with the name of your file, separated by a forward slash
foodEnvAtlasFilepath <-  paste(getwd(),"Food Environment Atlas.xlsx",sep="/")
foodEnvAtlasFilepath


# Browse the names of the sheets in the Excel file
sheet_names <- excel_sheets(foodEnvAtlasFilepath)
sheet_names

# Save the names of the 9 sheets we will be importing
our_sheets <- sheet_names[5:length(sheet_names)]
length(our_sheets)
our_sheets



#### Load the first two sheets --------------------------------------------------------

# Load the first sheet: ACCESS
foodEnvAtlas <- read_excel(foodEnvAtlasFilepath, sheet='ACCESS')
names(foodEnvAtlas)
length(names(foodEnvAtlas))
View(foodEnvAtlas)
# there are 44 variables in the "ACCESS" sheet
# 3143 county observations



# Load the next sheet: STORES
our_sheets[2]
current_sheet <- read_excel(foodEnvAtlasFilepath, sheet = our_sheets[2])
names(current_sheet)
length(names(current_sheet))
# There are 39 variables in the "STORES" sheet



#### Merge two sheets --------------------------------------------------------

# I want to merge foodEnvAtlas with current_sheet to make EnvironmentAll

## Merge using dplyr
EnvironmentAll <- foodEnvAtlas %>%
  # use a full join so that we do not drop any rows/observations even if they don't match with the other sheet
  full_join(
    current_sheet, 
    # use FIPS codes to merge/join them
    by = "FIPS"
  )

# View the dataset
View(EnvironmentAll)    
# We see that the state and county columns are duplicated


## We don't want to keep duplicate variables that are found in both ACCESS and STORE, so only select the non-duplicate variables from STORES

# Get list of repeat vars so we know what to remove
?dplyr::intersect
dup_vars <- dplyr::intersect(names(current_sheet), names(foodEnvAtlas))
dup_vars

# We are going to merge on FIPS, so we need to keep it in both sheets-- remove it from our duplicates list
?which
dup_vars <- dup_vars[which(dup_vars != "FIPS")]
dup_vars

# Get rid of those duplicate vars from the new spreadsheet
current_sheet <- current_sheet %>%
  select(-all_of(dup_vars))
names(current_sheet)


# Merge the STORES sheet to the ACCESS sheet and name it EnvironmentAll
EnvironmentAll <- foodEnvAtlas %>%
  # use a full join so that we do not drop any rows/observations even if they don't match with the other sheet
  full_join(
    current_sheet, 
    # use FIPS codes to merge/join them
    by = "FIPS"
  )
names(EnvironmentAll)           # State and County are no longer duplicated
length(names(EnvironmentAll))   # 80 variables



#### Loop through the sheets 
# ----------------------------------------------

# Instead of manually merging each sheet with a lot of repetitive code, we can automate this process with a for-loop!
# Loop through the sheets to read them all in, and put them all into one sheet


# Check list of sheets
our_sheets
# We have already done our first two sheets, so we only want to loop through the remaining 7 sheets
our_sheets[3:9]


# Therefore, our for-loop should start at index 3 of our_sheets
for (i in 3:length(our_sheets)) {
  
  # Read in the i'th sheet
  sheet <- read_excel(foodEnvAtlasFilepath, sheet = our_sheets[i])
  
  # Get list of repeat vars
  dup_vars <- intersect(names(sheet), names(foodEnvAtlas))
  # Keep FIPS, so get it out of this list
  dup_vars <- dup_vars[which(dup_vars != "FIPS")]
  # Drop duplicate vars from the spreadsheet
  sheet <- sheet %>%
    select(-all_of(dup_vars))
  
  # Merge the sheet to the main file using a full_join
  EnvironmentAll <- EnvironmentAll %>%
    full_join(
      sheet, 
      # use FIPS codes to merge/join them
      by = "FIPS"
      )
}

## Check results - 284 variables
names(EnvironmentAll)
grep("\\.x",names(EnvironmentAll))    # No variables contain a ".x" in the name
grep("\\.y",names(EnvironmentAll))    # No variables contain a ".y" in the name
# No duplicate variables!

# -----
## Example grep command
#grep("\\.x",c("dksalj.x","y","xxx.x","aaaa"))
# -----




#### Query Census API --------------------------------------------------------

# Define string for Census API URL
api_url <- 'https://api.census.gov/data/2020/dec/pl?get=P1_001N&for=county:*'

# Load in the Census API data, which comes in JSON format and imports as a list of observations, not a data frame
api_obj <- jsonlite::fromJSON(api_url, simplifyDataFrame = TRUE)
# What is the output?
class(api_obj)
head(api_obj)


# Change matrix to a data frame
Census2020Pop<- as.data.frame(api_obj)
head(Census2020Pop)

# Clean up and give column names
Census2020Pop <- Census2020Pop %>%
  # Remove first observation which were the variable names
  filter(!(V1=="P1_001N")) %>%
  # Name it Census2020Pop with columns Pop2020, State, County
  rename(
    Pop2020 = V1,
    StateFIPS = V2,
    CountyFIPS = V3
  )

# Check now that it looks nice!
head(Census2020Pop)

# Create a FIPS code variable -- the Census dataset has county FIPS and state FIPS -- we must combine into one FIPS code so we can then merge on FIPS
# For example, if state="01", county="001", then FIPS="01001"
Census2020Pop <- Census2020Pop %>% mutate(
  FIPS = paste0(StateFIPS,CountyFIPS)
)
# It looks right!
head(Census2020Pop)

# Check that all FIPS codes are 5 digits
sum(nchar(Census2020Pop$FIPS) < 5)
sum(nchar(Census2020Pop$FIPS) == 5)

# View dataset
View(Census2020Pop)



#### Merge the datasets ------------------------------------------------------


# Merge data frames on FIPS
df_final <- EnvironmentAll %>%
  full_join(
    Census2020Pop,
    by = "FIPS"
  )


# View changing dimensions as proof of successful data merge
sprintf('Columns in Atlas Dataset: %i', ncol(EnvironmentAll))
sprintf('Columns in Census Dataset: %i', ncol(Census2020Pop))
sprintf('Columns in Final Dataset %i', ncol(df_final))



#### Bonus question ------------------------------------------------------

## Bonus: Figure out why the number of observations went from 3143 to 3226 in the final dataset -- which counties appeared in only one dataset and not the other?
setdiff(EnvironmentAll$FIPS, Census2020Pop$FIPS)
setdiff(Census2020Pop$FIPS, EnvironmentAll$FIPS)

different_FIPS_indices <- which(df_final$FIPS %in% setdiff(EnvironmentAll$FIPS, Census2020Pop$FIPS) )

paste(df_final$County[different_FIPS_indices],
      df_final$State[different_FIPS_indices], sep=", ")

sum(is.na(df_final$County))
sum(is.na(df_final$CountyFIPS))
nrow(df_final) - nrow(EnvironmentAll)
nrow(df_final) - nrow(Census2020Pop)

## Counties could be missing because there are name changes for various reasons and there are counties that are too small or merged together, etc., and we are using 2020 Census data while the Food Environment Atlas uses data prior to that.