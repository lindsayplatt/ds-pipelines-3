# download data for each site
# packages needed: tidyverse, dataRetrieval
get_site_data <- function(state_info_file, parameter) {
  site_info <- readr::read_tsv(state_info_file, col_types='cccddcDDi')
  message(sprintf('  Retrieving data for %s-%s', site_info$state_cd, site_info$site_no))

  # simulate an unreliable web service or internet connection by causing random failures
  if(runif(1) < 0.5) {
    Sys.sleep(0.5)
    stop('Ugh, the internet data transfer failed! Try again.')
  }

  # actually pull the data
  site_data <- dataRetrieval::readNWISdv(
    siteNumbers=site_info$site_no, parameterCd=parameter) %>%
    dataRetrieval::renameNWISColumns(p00010="Value", p00060="Value", p00300="Value") %>%
    as_tibble()
  # do special handling for some sites with unusally-named value columns
  if(!('Value_cd' %in% names(site_data))) {
    if(parameter == '00010') {
      site_data <- switch(
        site_info$site_no[1],
        '01646500' = site_data %>%
          mutate(
            Value = `7.1.ft.from.riverbed..top._Value`,
            Value_cd = `7.1.ft.from.riverbed..top._Value_cd`
          ),
        '01389005' = site_data %>%
          mutate(
            Value = `.from.middle.intake_Value`,
            Value_cd = `.from.middle.intake_Value_cd`
          )
      )
    }
  }
  site_data_final <- site_data %>%
    rename(Site=site_no, Quality=Value_cd) %>%
    filter(!is.na(Value)) %>%
    mutate(State=site_info$state_cd, Parameter=c('00060'='Flow', '00010'='Wtemp', '00300'='DO')[parameter]) %>%
    select(-agency_cd) %>%
    select(State, Site, Date, Value, Quality, Parameter)

  return(site_data_final)
}
