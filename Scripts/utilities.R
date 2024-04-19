# All functions used in create_epi_data



#' Create all year-quarters between the start and end date.
#'
#' @param START_DATE Enter the start date for data
#' @param END_DATE Enter the end date for data
#'
#' @return All quarters between the start and end date
#' @export
#'
#' @examples
create_date <- function(START_DATE, END_DATE){
  START_DATE_CONVERT <- lubridate::yq(START_DATE)
  END_DATE_CONVERT <- lubridate::yq(END_DATE)
  DATE_RANGE <- seq(from = START_DATE_CONVERT, to = END_DATE_CONVERT, by = "quarter")

  # Define names for each element in the vector
  quarter_names <- paste0("quarter", 1:length(DATE_RANGE))

  # Convert the sequence into a named vector
  named_date_range <- setNames(DATE_RANGE, quarter_names)

  return(named_date_range)
}



#' Creates the spectrum dataset for use in epi. Only two indicators are needed and only age groups that match MER
#'
#' @param spectrum_file Spectrum dataset
#' @param mapping_file mapping dataset maps the SNU and PSNU between spectrum and MER
#' @param VAL_YEAR_SPECTRUM spectrum year is one year before the MER year
#'
#' @return a clean spectrum dataset with PLHIV, PLHIV aware at a district level for speficied age groups
#' @export
#'
#' @examples
create_spectrum <- function(spectrum_file, mapping_file, VAL_YEAR_SPECTRUM){

  # download from google drive
  mapping_temp_df <- googlesheets4::read_sheet(mapping_file) %>%
    select(area_id, MER_name, psnuuid, snu1uid, snu1, psnu)


  #download and read the indicators dataset from google drive
  drive_download(as_id(SPECTRUM_PATH), path = paste0(DATA_PATH,"spectrum_indicators.csv"), overwrite = TRUE)


  spectrum_temp_df <- read_csv(paste0(DATA_PATH,"spectrum_indicators.csv")) %>%
    left_join(mapping_temp_df, join_by(area_id)) %>%
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

  return(spectrum_temp_df)
}

#' Creates a clean MER dataset with only TX_CURR and TX_PVLS indicators
#'
#' @param MER_PATH Path to the MER dataset
#' @param VAL_YEAR Year for the data
#' @param VAL_QUARTER quarter for the data
#'
#' @return a cleaned MER dataset for a specific year-quarter.
#' @export
#'
#' @examples
create_mer <- function(MER_PATH, VAL_YEAR, VAL_QUARTER){

  temp_df <- read_psd(MER_PATH) %>%
    filter(fiscal_year == VAL_YEAR,
           ageasentered != "Unknown Age",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age Aggregated/Sex/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus",
                                           "Age Aggregated/Sex/Indication/HIVStatus",
                                           "Age/Sex/HIVStatus",
                                           "Age Aggregated/Sex/HIVStatus")

    )%>%
    select(
      snu1,
      snu1uid,
      snu2,
      snu2uid,
      fiscal_year,
      ageasentered,
      sex,
      indicator,
      numeratordenom,
      standardizeddisaggregate,
      all_of(VAL_QUARTER)
    ) %>%
    mutate(
      snu2uid = case_when(
        snu2 == "Chonguene" ~ "jDWCkBXYllV",
        snu2 == "Mandlakaze" ~ "VjJPal9jqUi",
        TRUE ~ snu2uid
      ),
      age_group_type = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14", "<15") ~ "ped",
        TRUE ~ "adult"
      )
    ) %>%
    select(-fiscal_year) %>%
    rename(value := !!VAL_QUARTER,
           psnu = snu2,
           psnuuid = snu2uid) %>%
    drop_na(value)
  return(temp_df)
}


#' Creates the military dataset using contributions from SISMA
#'
#' @param mer_df MER dataset
#' @param VAL_YEAR Year of data
#' @param VAL_QUARTER quarter of data
#' @param MILITARY_PSNU_PATH contribution for each PSNU from SISMA
#'
#' @return MER data for Military Mozambique based on SISMA contribution
#' @export
#'
#' @examples
create_military_MER <- function(mer_df, VAL_YEAR, VAL_QUARTER, MILITARY_PSNU_PATH){

  #SISMA file - contribution for each PSNU
  temp_contribution_df <- googlesheets4::read_sheet(MILITARY_PSNU_PATH) %>%
    clean_names() %>%
    select(-tx_curr) %>%
    mutate(col_join = "join")

  temp_military <- mer_df %>%
    filter(snu1 == "_Military Mozambique") %>%
    select(-c(psnu, psnuuid, snu1, snu1uid)) %>%
    mutate(
      col_join = "join",
      indicator = case_when(indicator == "TX_PVLS" ~ paste(indicator, numeratordenom, sep = "_"),
                            TRUE ~ indicator)
    ) %>%
    rename(military_value = value) %>%
    select(-standardizeddisaggregate) %>%
    group_by(ageasentered, sex, indicator, age_group_type, col_join) %>%
    summarise(military_value = sum(military_value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = military_value) %>%
    filter(TX_CURR > 0 | is.na(TX_CURR) |
             TX_PVLS_N > 0 | is.na(TX_PVLS_N) |
             TX_PVLS_D > 0 | is.na(TX_PVLS_D)) %>%

    #create the PVLS %
    mutate(pvls_percent = TX_PVLS_N / TX_PVLS_D)


  TX_military <- temp_military %>%
    left_join(temp_contribution_df, by = "col_join", relationship = "many-to-many") %>%
    mutate(TX_CURR = TX_CURR * percent_total,
           TX_PVLS_D = TX_PVLS_D * percent_total,  #created using the contribution of TX_CURR
           TX_PVLS_N = pvls_percent * TX_PVLS_D_new
    )%>%
    select(-c(pvls_percent, col_join, percent_total))

  #adjust the TX_CURR and TX_PVLS so it matches the expected total
  TX_military <- TX_military %>%
    mutate(total_TX_CURR = sum(temp_military$TX_CURR, na.rm = TRUE),
           total_TX_PVLS_N = sum(temp_military$TX_PVLS_N, na.rm = TRUE),
           total_TX_PVLS_D = sum(temp_military$TX_PVLS_D, na.rm = TRUE),
           scaling_factor_TX_CURR = total_TX_CURR / sum(TX_CURR),
           scaling_factor_TX_PVLS_N = total_TX_PVLS_N / sum(TX_PVLS_N),
           scaling_factor_TX_PVLS_D = total_TX_PVLS_D / sum(TX_PVLS_D),
           TX_CURR = TX_CURR * scaling_factor_TX_CURR,
           TX_PVLS_N = TX_PVLS_N * scaling_factor_TX_PVLS_N,
           TX_PVLS_D = TX_PVLS_D * scaling_factor_TX_PVLS_D,
    ) %>%

    #recreating all the columns to bind to the other SNU's
    pivot_longer(cols = starts_with("TX"),
                 names_to = "indicator",
                 values_to = "value") %>%
    mutate(standardizeddisaggregate = "military") %>%
    separate(indicator, into = c("indicator", "numeratordenom"), sep = "(?<=TX_PVLS)_", remove = FALSE) %>%
    mutate(numeratordenom = case_when(indicator == "TX_CURR" ~ "N",
                                      .default = numeratordenom))

  return(TX_military)

}



#' Creates the final epi dataset by combining spectrum and MER data
#'
#' @param spectrum_df  Cleaned spectrum dataset
#' @param TX_CURR TX_CURR model
#' @param TX_PVLS_N TX_PVLS_N model
#' @param TX_PVLS_D TX_PVLS_D model
#' @param VAL_YEAR Year of data
#' @param VAL_QUARTER quarter of data
#'
#' @return the epi dataset
#' @export
#'
#' @examples
create_epi_df <- function(spectrum_df, TX_CURR, TX_PVLS_N, TX_PVLS_D, VAL_YEAR, VAL_QUARTER){

  epi_temp <- spectrum_df %>%
    select(-age_group_type) %>%
    bind_rows(TX_CURR) %>%
    bind_rows(TX_PVLS_D) %>%
    bind_rows(TX_PVLS_N) %>%
    mutate(period = paste(VAL_YEAR, VAL_QUARTER) ,
           period = stringr::str_replace(period, "qtr", ""))

  return(epi_temp)

}
