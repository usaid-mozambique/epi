library(glamr) #for setting up the USAID way
library(gophr) # for reading MER data
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)
library(mozR)


folder_setup(
  folder_list = list(
    "Data",
    "Images",
    "Scripts",
    "AI",
    "Dataout",
    "GIS",
    "Documents",
    "Graphics",
    "markdown",
    "Tableau"
  )
)



#VALUES AND PATHS --------------------------------------------------------------

VAL_QUARTER = "qtr2"
VAL_YEAR = 2023

SPECTRUM_RAW <- "Data/indicators.csv"
MAPPING_RAW  <- "Data/mapping.xlsx"
MER_RAW      <- "Data/dg_ref.rds"


folder_period <- glue::glue("{VAL_YEAR}_{VAL_QUARTER}")

#Local path
path_quarterly_epi_output_file <-
  glue::glue("Dataout/quarterly/epi_{folder_period}.csv")


#historic
path_historic_epi_output_file <- "Dataout/epi.csv"


#LOAD SPECTRUM DATA ------------------------------------------------------------

## End: only 2023, sex = male or female, only detailed age groups, only district, three indicators.
## include psnuUID and snuuidfor later joining
mapping <- read_excel(MAPPING_RAW) %>%
  select(area_id, MER_name, psnuuid, snu1uid, snu1, psnu)

spectrum_df <- read_csv(SPECTRUM_RAW)



# Load MER Data
mer_df <- read_psd(MER_RAW) %>%
  filter(fiscal_year == VAL_YEAR,
         ageasentered != "Unknown Age",
         snu1 != "_Military Mozambique") %>%
  select(
    snu1,
    snu1uid,
    psnu,
    psnuuid,
    fiscal_year,
    ageasentered,
    sex,
    indicator,
    numeratordenom,
    standardizeddisaggregate,
    all_of(VAL_QUARTER)
  )


#DATA MUNGING ------------------------------------------------------------------

# Spectrum Dataset
spectrum <- spectrum_df %>%
  left_join(mapping, join_by(area_id)) %>%

  mutate(
    name = case_when(is.na(MER_name) ~ area_name,
                     TRUE ~ MER_name),
    year = year(my(quarter_label)),
    sex = str_to_title(sex),
    age_group_label = case_when(
      age_group_label == "<1" ~ "<01",
      age_group_label == "1-4" ~ "01-04",
      age_group_label == "5-9" ~ "05-09",
      TRUE ~ age_group_label
    ),
    age_group_type = case_when(
      age_group_label %in% c("<01", "01-04", "05-09", "10-14", "<15") ~ "ped",
      TRUE ~ "adult"
    )
  ) %>%

  filter(
    sex != "Both",
    indicator %in% c("plhiv", "aware_plhiv_num"),
    area_level == 3,
    year == VAL_YEAR,
    age_group_label %in% c(
      "<01",
      "01-04",
      "05-09",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65+"
    )
  ) %>%
  select(
    -c(
      MER_name,
      area_name,
      quarter_label,
      calendar_quarter,
      median,
      mode,
      area_id,
      area_level_label,
      age_group,
      indicator,
      name,
      area_level,
      year,
      se,
      upper,
      lower
    )
  ) %>%
  rename(value = mean,
         indicator = indicator_label,
         age = age_group_label)


#MER Dataset
mer <- mer_df %>%
  mutate(
    psnuuid = case_when(
      psnu == "Chonguene" ~ "jDWCkBXYllV",
      psnu == "Mandlakaze" ~ "VjJPal9jqUi",
      TRUE ~ psnuuid
    ),
    age_group_type = case_when(
      ageasentered %in% c("<01", "01-04", "05-09", "10-14", "<15") ~ "ped",
      TRUE ~ "adult"
    )
  )

# CREATE EPI MODEL DATA --------------------------------------------------------

TX_CURR   <- mozR::create_epi_model(
  mer,
  "TX_CURR",
  c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
  num_dem = "N",
  label = "TX_CURR"
)

TX_PVLS_N <- mozR::create_epi_model(
  mer,
  "TX_PVLS",
  c(
    "Age/Sex/Indication/HIVStatus",
    "Age Aggregated/Sex/Indication/HIVStatus"
  ),
  num_dem = "N",
  label = "TX_PVLS_N"
)

TX_PVLS_D <- mozR::create_epi_model(
  mer,
  "TX_PVLS",
  c(
    "Age/Sex/Indication/HIVStatus",
    "Age Aggregated/Sex/Indication/HIVStatus"
  ),
  num_dem = "D",
  label = "TX_PVLS_D"
)

# CREATE EPI DATA --------------------------------------------------------------

epi <- spectrum %>%
  select(-age_group_type) %>%
  union(TX_CURR) %>%
  union(TX_PVLS_D) %>%
  union(TX_PVLS_N) %>%
  mutate(value = round(value, digits = 0)) %>%
  pivot_wider(
    names_from = "indicator",
    values_from = "value",
    values_fn = list(values = sum)
  ) %>%
  clean_names()  %>%
  mutate(
    tx_curr_adj = case_when(
      (tx_curr >= number_plhiv_aware) ~ number_plhiv_aware,
      TRUE ~ tx_curr
    ),
    period      = paste(VAL_YEAR, VAL_QUARTER)
  ) %>%

  pivot_longer(
    cols = c(
      tx_curr,
      tx_curr_adj,
      plhiv,
      number_plhiv_aware,
      tx_pvls_n,
      tx_pvls_d
    ),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(period = stringr::str_replace(period, "qtr", "")) %>%
  # remove all nulls in the dataset for percent calculation to work

  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate_all(~replace(., is.na(.), 0))  %>%
  mutate(tx_pvls_percent = case_when(tx_pvls_d == 0 ~ 0,
                                  .default = tx_pvls_n / tx_pvls_d),
         virally_suppressed = round(tx_curr * tx_pvls_percent, digits = 0)
         ) %>%
  select(-tx_pvls_percent)

# OUTPUT ----------------------------------------------------------------------------

# to disk
write_excel_csv2(epi, path_quarterly_epi_output_file, delim = ",")

# Create historical datasets
epi_historic_files <- dir("Dataout/quarterly/", pattern = "*.csv")
epi_historic <- epi_historic_files %>%
  map( ~ read_csv2(file.path("Dataout/quarterly/", .))) %>%
  reduce(rbind) %>%
  write_excel_csv2("Dataout/epi.csv")
