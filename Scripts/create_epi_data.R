library(glamr) #for setting up the USAID way
library(gophr) # for reading MER data
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)
library(mozR)




#VALUES AND PATHS --------------------------------------------------------------

VAL_QUARTER = "qtr4"
VAL_YEAR = 2023
VAL_YEAR_SPECTRUM = 2022

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

military_psnu <- read_xlsx("Data/military_psnu_contribution.xlsx",
                           sheet = "contribution")


# Load MER Data
mer_df <- read_psd(MER_RAW) %>%
  filter(fiscal_year == VAL_YEAR,
         ageasentered != "Unknown Age")%>%
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

mer_non_military_df <- mer_df %>%
  filter(snu1 != "_Military Mozambique")

mer_military_df <- mer_df %>%
  filter(snu1 == "_Military Mozambique") %>%
  select(-c(psnu, psnuuid, snu1, snu1uid))




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
    year == VAL_YEAR_SPECTRUM,
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
mer <- mer_non_military_df %>%
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


#military
military_df <- military_psnu %>%
  clean_names() %>%
  select(-tx_curr) %>%
  mutate(col_join = "join")

mer_military <- mer_military_df %>%
  mutate(age_group_type = case_when(
    ageasentered %in% c("<01", "01-04", "05-09", "10-14", "<15") ~ "ped",
    .default = "adult"),
    col_join = "join"
  )


# CREATE EPI MODEL DATA --------------------------------------------------------

TX_CURR_M <- mer_military %>%
  filter(indicator == "TX_CURR",
         numeratordenom == "N",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) %>%
  group_by(sex,
           ageasentered,
           numeratordenom,
           age_group_type,
           col_join,
           standardizeddisaggregate) %>%
  rename(value = VAL_QUARTER) %>%


  #how to summarize correctly by
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')

TX_CURR_military_total <- TX_CURR_M %>%
  summarise(value = sum(value)) %>%
  pull()

#calculate what the value will be based on the total percentage for TX_CURR
TX_CURR_military <- TX_CURR_M %>%
  left_join(military_df, by = "col_join", relationship = "many-to-many") %>%
  mutate(total_tx_curr = TX_CURR_military_total,
         sex_age_percent = value / total_tx_curr, na.rm = NULL,
         tx_curr_psnu = round(total_tx_curr * percent_total, 0),
         tx_curr_sex_age = round(tx_curr_psnu * sex_age_percent, 0),
         indicator = "TX_CURR",
         fiscal_year = VAL_YEAR
         ) %>%

  select(-c(percent_total, total_tx_curr, value, sex_age_percent, tx_curr_psnu,
            col_join)) %>%
rename(!!VAL_QUARTER == tx_curr_sex_age)

#Add military to Mer non-military - only needed for TX_CURR

# use rbind and not union to get all data
mer_all <- mer %>%
  rbind(TX_CURR_military)

TX_CURR   <- mozR::create_epi_model(
  mer_all,
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
    "Age Aggregated/Sex/Indication/HIVStatus",
    "Age/Sex/HIVStatus",
    "Age Aggregated/Sex/HIVStatus"
  ),
  num_dem = "N",
  label = "TX_PVLS_N"
)

TX_PVLS_D <- mozR::create_epi_model(
  mer,
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

  #create the virally suppressed proxy
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate_all(~replace(., is.na(.), 0))  %>%
  mutate(tx_pvls_percent = case_when(tx_pvls_d == 0 ~ 0,
                                  .default = tx_pvls_n / tx_pvls_d),
         virally_suppressed = round(tx_curr * tx_pvls_percent, digits = 0)
         ) %>%
  select(-tx_pvls_percent) %>%
  pivot_longer(cols = 8:14,
               names_to = "indicator",
               values_to = "value")


# OUTPUT ----------------------------------------------------------------------------

# to disk
write_excel_csv2(epi, path_quarterly_epi_output_file)

# Create historical datasets
epi_historic_files <- dir("Dataout/quarterly/", pattern = "*.csv")
epi_historic <- epi_historic_files %>%
  map( ~ read_csv2(file.path("Dataout/quarterly/", .))) %>%
  reduce(rbind) %>%
  write_excel_csv2("Dataout/epi.csv")



