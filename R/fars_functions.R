#' Read a CSV file and create it a data frame tbl
#'
#' @description  This is a simple function that reads a csv(common separated values) file and create a data frame from it,
#' with cases corresponding to rows(lines) and variables to columns(fields) in the file. tbl objects only print
#' a few rows and all the columns that fit on one screen, describing the rest of it as text.
#'
#' @param filename A charater string of the name of file to be imported or a relative path to a file.
#'                 If file does not exist in the working directory or specific directory, function will be stopped with an error message.
#'
#' @return Only prints the first 10 rows and the columns that fit on screen. The rest of columns which were not shown in tbl are listed below tbl as text.
#'
#' @section Warning:
#' Function will skip year if file with data for year does not exists
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("extdata/accident_2013.csv")
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
fars_read("extdata/accident_2013.csv")



#' Write file name attaching desired year
#'
#' @description  This is a simple function that makes file name as pre-defined format attaching a specific year
#'               parameter of an argument
#'
#' @param year A non-negative integer specifying the desired year.
#'
#' @return A format string including a year, such as accident_2017.csv.bz2
#'
#' @examples
#' make_filename(2017)
#' make_filename("2017")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read a specific year file and Select MONTH, year columns of it.
#'
#' @description Read a desired year's pre-named file that was made by \link{make_filename} function
#'              and Select only MONTH, year two columns matching year value in the tbl with the your input year value.
#'              If you input unexist year, it will be error due to there is no file to import
#'
#' @param years A non-negative interger specifying the desired year. This year's data will be loaded in a tbl by \link{fars_read}
#'
#' @return A specific columns of loaded data. The MONTH, YEAR columns will be selected
#'
#' @importFrom dplyr mutate, select
#'
#' @examples
#' \dontrun{fars_read_years(2013)}
#' \dontrun{fars_read_years("2013")}
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


#' Sum count by month of the year
#'
#' @description \code{fars_summarize_year} reads a desired years pre-named file by using \code{\link{fars_read_years}} function and group by year, MONTH.
#'              And then summarize counting by month of the year by \code{\link[dplyr]{summarize}}
#'
#' @param years  A vector of years values (A non-negative interger specifying the desired year)
#'
#' @return A tbl including MONTH and it's count value. Total 12 rows and month and count columns will be shown.
#'
#'
#' @importFrom dplyr bind_rows, group_by, summarize
#' @importFrom tidyr spread
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#' \dontrun{fars_summarize_years("2013")}
#' \dontrun{fars_summarize_years(c("2013","2014")}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' Map a specific state's accidents in a specific year selected.
#'
#' @param state.num A non-negative interger specifying the desired state
#' @param year A non-negative interger specifying the desired year.
#'
#' @return A map of a state marked accidents as circle
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#'
#' @seealso \code{\link{fars_read}} \code{\link{make_filename}} \code{\link{make_filename}} \code{\link{make_filename}}
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#'
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
