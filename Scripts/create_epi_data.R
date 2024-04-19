# List of required packages
required_packages <- c("glamr", "gophr","janitor", "lubridate",
                       "readxl","tidyverse", "mozR", "goggledrive")

# todo:  how to handle packages that are installed on GitHub and not CRAN?
# Check if packages are installed
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# If any packages are missing, install them.  Looks at CRAN and USAID OHA repos
install.packages(missing_packages, repos = c("https://usaid-oha-si.r-universe.dev",
                                             "https://cloud.r-project.org"))



library(glamr) #for setting up the USAID way
library(gophr) # for reading MER data
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)
library(mozR)
library(googledrive)


#authentication for google

glamr::load_secrets()

glamr::folder_setup() # create standard folders if they do not exist


#VALUES AND PATHS (Updated by end user) --------------------------------------------------------------

SPECTRUM_PATH <- "1mNEv4Rz96g8wWTNAWe1OwDi4cKQ8rPag"
MAPPING_PATH <- "1HSKvJ8Tk2EbhXaxvtK5oFI9WPmAzt24G7ZCs_YVzzpE"
MER_PATH      <- "Data/Genie_SITE_IM_2023_2024.txt"
MILITARY_PSNU_PATH <- "1wTohWSk93xfOGZXxUoArXUIdGXqaZuaWJ5YR_hf_PAI"
START_DATE <- "2023 Q4"
END_DATE <- "2023 Q4"


DATA_PATH <- "Data/"  #location to download naomi spectrum dataset
EPI_OUTPUT_PATH <- "Dataout/epi.csv"  #save final dataset here

#All functions
source("Scripts/utilities.R")

# Create Datasets -----------------------------------------------------------------------

create_epi_data <- function(x){

  VAL_YEAR <- year(x)
  VAL_QUARTER <- paste0("qtr",quarter(x))
  VAL_YEAR_SPECTRUM <- VAL_YEAR - 1

  mer_original <- create_mer(MER_PATH, VAL_YEAR, VAL_QUARTER)

  #only TX_CURR for military_mozambique
  mer_military_df <- create_military_MER(mer_original, VAL_YEAR, VAL_QUARTER, MILITARY_PSNU_PATH)


  #TX_CURR and TX_PVLS
  mer_non_military_df <- mer_original %>%
    filter(snu1 != "_Military Mozambique")

  mer_all_df <- mer_non_military_df  %>%
    bind_rows(mer_military_df)

  spectrum_df <- create_spectrum(SPECTRUM_PATH, MAPPING_PATH, VAL_YEAR_SPECTRUM)


  TX_CURR_df   <- mozR::create_epi_model(
    mer_all_df,
    "TX_CURR",
    c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", "military"),
    num_dem = "N",
    label = "TX_CURR"
  )


  TX_PVLS_N_df <- mozR::create_epi_model(
    mer_all_df,
    "TX_PVLS",
    c(
      "Age/Sex/Indication/HIVStatus",
      "Age Aggregated/Sex/Indication/HIVStatus",
      "Age/Sex/HIVStatus",
      "Age Aggregated/Sex/HIVStatus",
      "military"
    ),
    num_dem = "N",
    label = "TX_PVLS_N"
  )

  TX_PVLS_D_df <- mozR::create_epi_model(
    mer_all_df,
    "TX_PVLS",
    c(
      "Age/Sex/Indication/HIVStatus",
      "Age Aggregated/Sex/Indication/HIVStatus",
      "Age/Sex/HIVStatus",
      "Age Aggregated/Sex/HIVStatus",
      "military"
    ),
    num_dem = "D",
    label = "TX_PVLS_D"
  )

  # CREATE EPI DATA --------------------------------------------------------------
  epi_df <- create_epi_df(spectrum_df, TX_CURR_df, TX_PVLS_D_df, TX_PVLS_N_df, VAL_YEAR, VAL_QUARTER)

  return(epi_df)
}


epi_df <- map_dfr(create_date(START_DATE, END_DATE), create_epi_data) %>%
  write_csv(EPI_OUTPUT_PATH)
