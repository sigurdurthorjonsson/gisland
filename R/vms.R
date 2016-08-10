# functions that help in working with logbooks and vms data

#' Lookup an id of an event that fall within an invertal given an additional variable
#' 
#' Given a dataframe that contains vectors of start and end (breakpoints) as 
#' well as an additional (co)variable find the interval
#' containing each element of x and the (co)variable and return a id 
#' variable from the dataframe (df)
#' 
#' The inspiration for this function was to match vms pings (x) and vessel id
#' (variable) to a data frame (trip, fishing event, ...) that contains an
#' intveral, ie. a start and end time, an additional (co)variable 
#' (e.g. vessel id) and then some "id" that is returned (fishing trip
#' id, fishing event id, ...). It may though be of use in other cases that may
#' not necessarily be related to time.
#' 
#' The function should be analogous to vmstools::mergeEflalo2Tacsat, albeit
#' more generic.
#' 
#' @param x A vector containing an event
#' @param variable Additional variable that also resides in df
#' @param df a data.frame containing as the first four columns: start time (t1),
#' end time(t2), the additional variable and id (FT_REF, visir, ...) to be 
#' returned.
#' @param cn The column names in df that contain that contain the start and
#' end of the breakpoints, the (co)variable and the variable to return.
#' 
#' @import data.table
#' 
#' @return a vector
#' @export
#'
lookup_interval_ids <- function(x, variable, df, cn = c("start", "end", "variable", "id")) {
  
  vms <- data.table::data.table(event = x,
                                variable = variable)
  
  df <- 
    df %>% 
    dplyr::rename_("start" = cn[1],
                   "end"   = cn[2],
                   "variable"   = cn[3],
                   "id"    = cn[4]) %>% 
    data.table::data.table()
  
  ans <- vms[df, on=.(event >= start, event < end, variable==variable), id:=id]
  return(ans$id)
}