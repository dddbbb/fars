## ----fars_read, eval=FALSE-----------------------------------------------
#    fars_read <- function(filename) {
#    if(!file.exists(filename))
#      stop("file '", filename, "' does not exist")
#    data <- suppressMessages({
#      readr::read_csv(filename, progress = FALSE)
#    })
#    dplyr::tbl_df(data)
#  }
#  

## ----fars_map_state, eval = FALSE----------------------------------------
#  
#    fars_map_state <- function(state.num, year) {
#      filename <- make_filename(year)
#      data <- fars_read(filename)
#      state.num <- as.integer(state.num)
#  
#      if(!(state.num %in% unique(data$STATE)))
#        stop("invalid STATE number: ", state.num)
#      data.sub <- dplyr::filter(data, STATE == state.num)
#      if(nrow(data.sub) == 0L) {
#        message("no accidents to plot")
#        return(invisible(NULL))
#      }
#      is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
#      is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
#      with(data.sub, {
#        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
#                  xlim = range(LONGITUD, na.rm = TRUE))
#        graphics::points(LONGITUD, LATITUDE, pch = 46)
#      })
#    }

