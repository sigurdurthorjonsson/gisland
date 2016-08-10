# vmstools inspired functions

# ------------------------------------------------------------------------------
# clean date mess in data provided with the vmstools package
tidy_date <- function(date) {
  date <- if_else(stringr::str_sub(date, 7) == "1800",
                  stringr::str_replace(date, "1800", "1803"),
                  stringr::str_replace(date, "1801", "1804"))
  return(date)
}


# ------------------------------------------------------------------------------
# tacsat

#' tidy tacsat formatted data
#'
#' @param d dataframe
#'
#' @return dataframe
#' @export
#'
tidy_tacsat <- function(d) {
  
  d <-
    d %>%
    dplyr::tbl_df() %>%
    # correct for leap-year problems in coded data
    #  should really not be part of the code
    dplyr::mutate(SI_DATE = tidy_date(SI_DATE),
                  SI_DATIM = lubridate::dmy_hms(paste(SI_DATE, SI_TIME)),
                  year = lubridate::year(SI_DATIM),
                  pid = 1:n()) %>%
    dplyr::select(pid, VE_COU, VE_REF, SI_DATIM, year,
                  SI_LATI, SI_LONG, SI_SP, SI_HE) %>%
    dplyr::distinct()
  
  return(d)
  
}

#' tidy eflalo types of data
#'
#' @param d eflalo type of dataframe
#' @param drop_columns drop columns used for deriving time
#' @param wide TRUE (default) returns orginal wide format 
#' @param overlaps FALSE (default) trips interval overlap. If set to TRUE
#' will ...
#'
#' @return dataframe or list
#' @export
#'
tidy_eflalo <- function(d,
                        drop_columns = FALSE,
                        wide = TRUE,
                        overlaps = FALSE) {
  
  d <-
    d %>%
    dplyr::mutate(FT_DDAT = tidy_date(FT_DDAT),
                  FT_LDAT = tidy_date(FT_LDAT),
                  LE_CDAT = tidy_date(LE_CDAT),
                  FT_DDATIM = lubridate::dmy_hms(paste(FT_DDAT, FT_DTIME)),
                  FT_LDATIM = lubridate::dmy_hms(paste(FT_LDAT, FT_LTIME)),
                  LE_CDAT = lubridate::dmy(LE_CDAT),
                  year = lubridate::year(FT_DDATIM),
                  LE_UNIT = as.character(LE_UNIT)) %>%
    dplyr::distinct() %>%
    dplyr::tbl_df()
  
  if(drop_columns) {
    d <-
      d %>%
      select(-c(FT_DDAT, FT_DTIME, FT_LDAT, FT_LTIME))
  }
  
  if(wide) return(d)
  
  vessel <-
    d %>%
    dplyr::select(starts_with("VE_")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(vid = 1:n()) %>%
    dplyr::select(vid, dplyr::starts_with("VE_"))
  
  # Note that trip does not contain a gear column
  trip <-
    d %>%
    dplyr::select(VE_REF, VE_COU, FT_REF:FT_DHAR, FT_DDATIM, FT_LCOU, FT_LHAR, FT_LDATIM) %>%
    dplyr::distinct()
  
  if(!overlaps) {
    trip <-
      trip %>%
      dplyr::arrange(VE_REF, FT_DDATIM) %>%
      dplyr::group_by(VE_REF) %>%
      # there is one case where this occurs
      dplyr::mutate(FT_DDATIM = if_else(FT_DDATIM == FT_LDATIM, FT_DDATIM - lubridate::minutes(1), FT_DDATIM),
                    # cases where landing time is the same as the next trip departure time
                    FT_LDATIM = dplyr::if_else(FT_LDATIM == dplyr::lead(FT_DDATIM),
                                               FT_LDATIM - lubridate::minutes(1),
                                               FT_LDATIM,
                                               FT_LDATIM)) %>%
      ungroup()
    
  }
  
  station <-
    d %>%
    dplyr::select(VE_REF, FT_REF, LE_ID:LE_EFF_VMS) %>%
    dplyr::distinct()
  
  # THE CATCH
  x1 <-
    d %>%
    dplyr::select(LE_ID, contains("_KG_")) %>%
    tidyr::gather(species, catch, -LE_ID) %>%
    dplyr::filter(catch != 0) %>%
    dplyr::mutate(species = stringr::str_sub(species, 7)) %>%
    dplyr::distinct()
  
  x2 <-
    d %>%
    dplyr::select(LE_ID, dplyr::contains("_EURO_")) %>%
    tidyr::gather(species, value, -LE_ID) %>%
    dplyr::filter(value != 0) %>%
    dplyr::mutate(species = stringr::str_sub(species, 9))
  
  catch <-
    dplyr::full_join(x1, x2, by = c("LE_ID", "species"))
  
  return(list(vessel  = vessel,
              trip    = trip,
              station = station,
              catch   = catch))
  
}



#' calc_interval
#'
#' Calculate time difference (minutes) between events
#'
#' @param x A date vector (class "POSIXct")
#' @param w1 weight 1
#' @param w2 weight 2
#'
#' @return a vector
#' @export
#'
#' @examples
#' SI_DATIM <- c(lubridate::ymd_hms("2016-01-01 12:00:00") +
#'                 lubridate::minutes(round(rnorm(12,
#'                                     seq(60, 720, length.out = 12),
#'                                     20))))
#' calc_interval(SI_DATIM)
calc_interval <- function(x, w1 = 0.5, w2 = 0.5) {
  
  w1 <- w1/(w1 + w2)
  w2 <- w2/(w1 + w2)
  
  x1 <- (dplyr::lead(x) - x)/lubridate::dminutes(1)
  x1 <- ifelse(is.na(x1), dplyr::lag(x1), x1)
  x2 <- w1 * x1 + w2 * dplyr::lag(x1)
  x2 <- ifelse(is.na(x2), x1, x2)
  
  return(x2)
}
