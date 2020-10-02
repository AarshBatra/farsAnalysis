#' read data file
#' 
#' Reads in data file (if the file exists) and then coerces
#' the data object into a tibble.
#' 
#' @param filename (or filepath) name of the file to be read in.
#' 
#' @return a tibble which contains the read in data.
#' 
#' @examples 
#' fars_read("farsDataFile")
#' fars_read("path/to/file")
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a file name string
#' 
#' Create a file name by specifying a specific year.
#' 
#' @param year an integer value for the year in question
#' 
#' @return a string for the file name that contains the year
#'         inputted by the user in the function.
#'         
#' @examples 
#' make_filename(2012)
#' make_filename(1988)
#' 
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Add a year column to data files
#' 
#' There exists a single data file for each year. Not all years have 
#' data files. For those years that do have a data file, this function
#' adds a year column to that data file. For those years that don't have
#' a data file this function returns NULL.
#' 
#' @param years a list/vector containing the \code{years}, for which we need to
#'         append the data file with a year column.
#'         
#' @return Returns a list (if the year is valid, if invalid, returns NULL)
#'         whose each element is a tibble (with a year column added to it)
#'         corresponding to each year specified in the input \code{years}
#'         list/vector to the function.
#'         
#' @examples 
#' fars_read_years(years = list(2012, 1988, 2013, 2015))
#' fars_read_years(years = c(2012,  2013, 1964, 2000))
#'         
#' @export                        

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

#' combine all data files and summarize the result
#' 
#' Combine all data files (note that there is one file for each valid
#' year in the \code{years} vector/list) and then get a count for how many
#' values (the count) exists for each \code{month}, \code{year} combination.
#' The values represent the number of fatal injuries suffered in motor vehicle
#' traffic crashes.
#' 
#' @param years years a list/vector containing the \code{years}, for which we need to
#'              append the data file with a year column.
#' 
#' @return a summary tibble where each row represents a month and each column 
#'         represents a year. Each cell, represents the number of values 
#'         (the count) under each \code{month}, \code{year} combination. 
#' 
#' @export                                                    

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Create a state map for the state for a single data file 
#' 
#' Map the fatalities (where the accident occured) for the 'state' given 
#' the dataset corresponding to the \code{year}.
#' 
#' @param state.num state number, an integer value, this is the state for which data
#'                  will be mapped.
#' @param year an integer value for the year for which we want to plot
#'             the location of the fatalities on the map.
#'             
#' @return If the state number as mentioned in the parameter \code{state.num} is
#'         invalid, this function returns an error. But, if the \code(state.num)
#'         is valid, this function plots  the locations (using
#'         latitude and longitude variables) of the fatalities.
#'         
#' @examples 
#' fars_map_state(state.num = 12, year = 2012)
#' 
#' @note This function creates a state map for a single state in the United states.
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
