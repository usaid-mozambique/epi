library(glamr) #for setting up the USAID way
library(gophr) # for reading MER data
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)
library(mozR)

#VALUES AND PATHS (Updated by end user) --------------------------------------------------------------

SPECTRUM_PATH <- "Data/indicators.csv"
MAPPING_PATH  <- "Data/mapping.xlsx"  #TODO:  this needs to point to google
MER_PATH      <- "Data/Genie_SITE_IM_2023_2024.txt"
MER_PATH <- "Data/dg_ref.rds"
MILITARY_PSNU_PATH <- "Data/military_psnu_contribution.xlsx"   #TODO:  this needs to point to google
START_DATE <- "2024 Q1"
END_DATE <- "2024 Q1"

#location for epi dataset.  What should the right name be?
EPI_OUTPUT_PATH <- "Dataout/epi.csv"

#All functions
source("Scripts/utilities.R")


# Create Datasets -----------------------------------------------------------------------

create_epi_data <- function(x){

  VAL_YEAR <- year(x)
  VAL_QUARTER <- paste0("qtr",quarter(x))
  VAL_YEAR_SPECTRUM <- VAL_YEAR - 1

  #only TX_CURR for military_mozambique
  mer_military_df <- create_military_MER(MER_PATH, VAL_YEAR, VAL_QUARTER, MILITARY_PSNU_PATH)

  #TX_CURR and TX_PVLS
  mer_non_military_df <- create_mer(MER_PATH, VAL_YEAR, VAL_QUARTER) %>%
    filter(snu1 != "_Military Mozambique")
  mer_all_df <- mer_non_military_df %>%
    bind_rows(mer_military_df)

  spectrum_df <- create_spectrum(SPECTRUM_PATH, MAPPING_PATH, VAL_YEAR_SPECTRUM)

  TX_CURR_df   <- mozR::create_epi_model(
    mer_all_df,
    "TX_CURR",
    c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
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
      "Age Aggregated/Sex/HIVStatus"
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
      "Age Aggregated/Sex/HIVStatus"
    ),
    num_dem = "D",
    label = "TX_PVLS_D"
  )

  # CREATE EPI DATA --------------------------------------------------------------
  epi_df <- create_epi_df(spectrum_df, TX_CURR_df, TX_PVLS_D_df, TX_PVLS_N_df, VAL_YEAR, VAL_QUARTER)

  return(epi_df)

}


epi_df <- map_dfr(create_date(START_DATE, END_DATE), create_epi_data) %>%
  write_excel_csv2("Dataout/epi.csv")

# Check Values ----------------------------------------------------------------------------
#returns an empty df if no discrepencies exist.  Currently discrepencies due to military

mer_df <- create_mer(MER_PATH, VAL_YEAR, VAL_QUARTER) # original with minimal cleaning - needs to be wrapped in purrr for this to work

check_value_discrepencies <- check_totals(mer_df, epi_df)


