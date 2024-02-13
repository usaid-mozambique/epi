# All functions used in create_epi_data



create_date <- function(START_DATE, END_DATE){
    START_DATE_CONVERT <- lubridate::yq(START_DATE)
    END_DATE_CONVERT <- lubridate::yq(END_DATE)
    DATE_RANGE <- seq(from = START_DATE_CONVERT, to = END_DATE_CONVERT, by = "quarter")

    # Define names for each element in the vector
    quarter_names <- paste0("quarter", 1:length(DATE_RANGE))

    # Convert the sequence into a named vector
    named_date_range <- setNames(DATE_RANGE, quarter_names)
}



create_spectrum <- function(spectrum_file, mapping_file, VAL_YEAR_SPECTRUM){

    mapping_temp_df <- read_excel(mapping_file) %>%
        select(area_id, MER_name, psnuuid, snu1uid, snu1, psnu)

    spectrum_temp_df <- read_csv(spectrum_file) %>%
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

create_mer <- function(MER_PATH, VAL_YEAR, VAL_QUARTER){

    #call military here to join everything

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
            psnu,
            psnuuid,
            fiscal_year,
            ageasentered,
            sex,
            indicator,
            numeratordenom,
            standardizeddisaggregate,
            all_of(VAL_QUARTER)
        ) %>%
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
        ) %>%
        select(-fiscal_year) %>%
        rename(value := !!VAL_QUARTER)
    return(temp_df)
}


create_military_MER <- function(MER_PATH, VAL_YEAR, VAL_QUARTER, MILITARY_PSNU_PATH){

    temp_contribution_df <- googlesheets4::read_sheet(MILITARY_PSNU_PATH) %>%
        clean_names() %>%
        select(-tx_curr) %>%
        mutate(col_join = "join")

    temp_military <- create_mer(MER_PATH, VAL_YEAR, VAL_QUARTER) %>%
        filter(snu1 == "_Military Mozambique",
               indicator == "TX_CURR",
               numeratordenom == "N",
               standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) %>%
        select(-c(psnu, psnuuid, snu1, snu1uid)) %>%
        mutate( col_join = "join"
        ) %>%
        group_by(sex,
                 ageasentered,
                 indicator,
                 numeratordenom,
                 age_group_type,
                 col_join,
                 standardizeddisaggregate) %>%
        rename(military_value = value) %>%
        #how to summarize correctly by
        dplyr::summarise(military_value = sum(military_value, na.rm = TRUE), .groups = 'drop')

    TX_CURR_military_total <- temp_military %>%
        summarise(military_value = sum(military_value)) %>%
        pull()


    #calculate what the value will be based on the total percentage for TX_CURR
    TX_CURR_military <- temp_military %>%
        left_join(temp_contribution_df, by = "col_join", relationship = "many-to-many") %>%
        mutate(total_tx_curr = TX_CURR_military_total,
               sex_age_percent = military_value / total_tx_curr, na.rm = NULL,
               tx_curr_psnu = round(total_tx_curr * percent_total, 0),
               tx_curr_sex_age = round(tx_curr_psnu * sex_age_percent, 0),
        ) %>%

        select(-c(percent_total, total_tx_curr,military_value, sex_age_percent, tx_curr_psnu,
                  col_join)) %>%
        rename(value = tx_curr_sex_age)

    return(TX_CURR_military)

}


create_epi_df <- function(spectrum_df, TX_CURR, TX_PVLS_N, TX_PVLS_D, VAL_YEAR, VAL_QUARTER){

    epi_temp <- spectrum_df %>%
        select(-age_group_type) %>%
        bind_rows(TX_CURR) %>%
        bind_rows(TX_PVLS_D) %>%
        bind_rows(TX_PVLS_N) %>%
        mutate(value = round(value, digits = 0)) %>%
        pivot_wider(
            names_from = "indicator",
            values_from = "value",
            values_fn = list(values = sum)
        ) %>%
        clean_names()  %>%
        pivot_longer(
            cols = c(
                tx_curr,
                plhiv,
                number_plhiv_aware,
                tx_pvls_n,
                tx_pvls_d
            ),
            names_to = "indicator",
            values_to = "value"
        ) %>%

        #create the virally suppressed proxy
        pivot_wider(names_from = indicator, values_from = value) %>%
        mutate_all(~replace(., is.na(.), 0))  %>%
        mutate(tx_pvls_percent = case_when(tx_pvls_d == 0 ~ 0,
                                           .default = tx_pvls_n / tx_pvls_d),
               virally_suppressed = round(tx_curr * tx_pvls_percent, digits = 0)
        ) %>%
        select(-tx_pvls_percent) %>%
        pivot_longer(cols = 7:12,
                     names_to = "indicator",
                     values_to = "value") %>%
        mutate(period = paste(VAL_YEAR, VAL_QUARTER) ,
                period = stringr::str_replace(period, "qtr", ""))

    return(epi_temp)

}

#province or snu level
check_totals <- function(mer_raw_df, epi_df){

    before <- mer_raw_df  %>%
        filter(indicator %in% c("TX_CURR", "TX_PVLS")) %>%
        group_by(indicator, numeratordenom) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop" ) %>%
        mutate(indicator = paste(indicator, numeratordenom, sep = "_")) %>%
        select(-numeratordenom) %>%
        pivot_wider(names_from = indicator) %>%
        rename(TX_CURR = TX_CURR_N) %>%
        clean_names() %>%
        rename(tx_curr_before = tx_curr,
           tx_pvls_n_before = tx_pvls_n,
           tx_pvls_d_before = tx_pvls_d)


    after <- epi_df  %>%
        filter(indicator %in% c("tx_curr", "tx_pvls_n", "tx_pvls_d")) %>%
        group_by(indicator) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop" ) %>%
        pivot_wider(names_from = indicator) %>%
        rename(tx_curr_after = tx_curr,
               tx_pvls_n_after = tx_pvls_n,
               tx_pvls_d_after = tx_pvls_d)

    diff <- before %>%
        cross_join(after) %>%
        mutate(tx_curr_diff = tx_curr_before - tx_curr_after,
               tx_pvls_n_diff = tx_pvls_n_before - tx_pvls_n_after,
               tx_pvls_d_diff = tx_pvls_d_before - tx_pvls_d_after) %>%
        select(-c(tx_curr_before, tx_curr_after, tx_pvls_n_before, tx_pvls_n_after,
                  tx_pvls_d_before, tx_pvls_d_after)) %>%
        #remove when there is no difference
        filter(tx_curr_diff != 0, tx_pvls_d_diff != 0, tx_pvls_n_diff != 0)

    return (diff)
}

