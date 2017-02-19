#'Read FARS data
#'
#'Reads data into data.frame from working directory.
#'.csv file should be already downloaded or error message will throw out
#'
#'@param filename Integer or string name of file for reading
#'
#'@return data.frame is output of the function
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Make filename for specific year
#'
#'Takes year and returns filename
#'
#'@param year Four digits
#'
#'@return filename for specific year
#'
#'@examples
#'
#' \dontrun{
#' make_filename("2015")
#' }
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#'Make list of data.frames for each year
#'
#'Takes years and makes list of data.frames. Each elements is data.frames with month and year
#'
#'@param years Vector of years. If there are no file with specific year NULL is returned
#'#'
#'@importFrom dplyr mutate select
#'@import magrittr
#'
#' @examples
#'
#' \dontrun{
#'  fars_read_years(c(2013, 2014, 2015))
#'  }


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

#'Summarise FARS data for given years
#'
#'@param years Vector of years
#'
#'@return data.frame with summarized data for every month and year
#'
#' @import magrittr
#' @importFrom dplyr bind_rows group_by summarise n
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'Plot data of accidents in specific state and year
#'
#'Takes state number and yeasr and make plot with map. If there are no state throw message "invalid STATE number: "
#'If there no accidents show message "no accidents to plot"
#'@param state.num number of state
#'@param year specific year
#'
#'@return plot with map or message
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'
#' \dontrun{
#'  fars_map_state(1, 2013)
#'  }
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
