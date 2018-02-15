#' Reads in a file
#'
#' Reads in the specified filename in df using readr. Messages are suppressed.
#' If the filame doesn not exist, a message is displayed.
#'
#' @param filename A character string giving the filename to be read in
#'
#' @return This function returns a dataframe containing the data in the filename if it existst. In case filename
#' doesn't exist, an error is displayed.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read(accident_2013.csv.bz2)
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Makes filename
#'
#' Given a year number or string, this function will make a file name consisting of accident_number.csv.bz2
#'
#' @param year A number or chracter string with numbers
#'
#' @return This function returns a file name consisting of accident_number.csv.bz2
#'
#' @examples
#' \dontrun{
#' make_filename("2015")
#' make_filename(2015)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Reads in multiple files
#'
#' From given years in numbers, this function tries to read in the corresponding data sets,
#' and selects months and year
#'
#' @param years A vector of numbers or character string with numbers
#'
#' @return For each element of the input vector, This function returns the month and year column of
#' dataframe containing the data in the filename if it existst.
#' In case filename doesn't exist, an error is displayed.
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014))
#' }
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}



#' Summarizes multiple files
#'
#' From given years in numbers, this function makes one dataframe containing data from all years.
#' From this data frame a table is made from the number of casualties with months as rows and years as columns
#'
#' @param years A vector of numbers or character string with numbers
#'
#' @return a data frame containing the number of casualties with months as rows and years as columns
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}





#' Plots map of number of fatalities
#'
#' From given statenumber and year, a map is plotted with the number of fatalities.
#' Longitude must be more than 900 and latitude more than 90.
#' When there are no fatalities, nothing is returned.
#'
#'
#' @param state.num a state number, when invalid an error is shown
#' @param year a number or character string containing numbers
#'
#' @return a tmap containing the number of casualties with months as rows and years as columns
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
