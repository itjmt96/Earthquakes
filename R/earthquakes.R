utils::globalVariables(c("YEAR", "MONTH", "DAY", "DT", "DATE", "LATITUDE", "LONGITUDE"))
#'
#' Function to read National Oceanic and Atmospheric Administration's (NOAA) file.
#'
#' This function uses readr and dplyr packages.
#' 1.   Read file
#'      Takes a TAB delimited file as an input filename parameter.
#'      If the file exists, this function reads it into dataframe object.
#' 2.   Error handling
#'      If the file does not exist in the working directory, this function stops with an output message that the file does not exist.
#' 3.   Suppressions
#'      It suppresses messages and progress bar while reading the file.
#'
#' @importFrom readr  read_delim
#'
#' @param filename A filename as input string.
#'
#' @return File data is read into a dataframe object.
#'
#' @examples
#' \dontrun{
#' read_noaa_file("signif.txt")
#' read_noaa_file(filename="signif.txt")
#' }
#'
#' @export
read_noaa_file <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_delim(filename, delim = "\t", col_names = TRUE, progress = FALSE)
        })
}

#' Function to clean LOCATION_NAME field from National Oceanic and Atmospheric Administration's (NOAA) file.
#'
#' This function uses dplyr and tools packages.
#' Cleans LOCATION_NAME field data by stripping of the country name, extra whitespaces.
#' Cleaned LOCATION_NAME field data has city name or locaton names in the country.
#' Converts cleaned LOCATION_NAME field data to toTitleCase.
#'
#' @importFrom dplyr filter mutate
#' @importFrom tools toTitleCase
#' @importFrom magrittr %>%
#'
#' @param data Data as dataframe object.
#'
#' @return Dataframe object with clean LOCATION_NAME fields data.
#'
#' @examples
#' \dontrun{
#' eq_location_clean(data=eq_clean)
#' }
#'
#' @export
eq_location_clean <- function(data) {
        data <- data %>%
                dplyr::mutate(LOCATION_NAME = tools::toTitleCase(tolower(trimws(
                                              gsub(".*:", "", data$LOCATION_NAME)
        ))))

        data
}

#' Function to clean National Oceanic and Atmospheric Administration's (NOAA) file.
#'
#' This function uses dplyr, lubridate, tidyr,  and  tools packages.
#' 1.   Read file
#'      Takes a TAB delimited file as an input filename parameter.
#' 2.   Create DATE field
#'      Filters out NA or 0 YEAR field data, NA MONTH field data, NA DAY field data.
#'      Combines YEAR, MONTH and DAY fields from the file and converts it into a date field and drops NA dates.
#' 3.   LATITUDE and LONGITUDE fields data
#'      Mutates LATITUDE and LONGITUDE fields data to numeric.
#' 4.   LOCATION_NAME field data
#'      Cleans LOCATION_NAME field data by stripping of the country name, extra whitespaces.
#'      Cleaned LOCATION_NAME field data has city name or locaton names in the country.
#'      Converts cleaned LOCATION_NAME field data to toTitleCase.
#'
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#'
#' @param filename A filename as input string.
#'
#' @return File data is read into a dataframe object with clean data in DATE, LATITUDE, LONGITUDE and LOCATION_NAME fields.
#'
#' @examples
#' \dontrun{
#' eq_clean_data("signif.txt")
#' eq_clean_data(filename="signif.txt")
#' }
#'
#' @export
eq_clean_data <- function(filename) {
        data <- read_noaa_file(filename) %>%

                dplyr::filter(!is.na(YEAR), YEAR > 0, !is.na(MONTH), !is.na(DAY)) %>%
                dplyr::mutate(DT=paste(YEAR, MONTH, DAY, sep = '-')) %>%
                dplyr::mutate(DATE=substring(lubridate::ymd(DT),1,10)) %>%
                tidyr::drop_na(DATE) %>%

                dplyr::mutate(LATITUDE=as.numeric(LATITUDE)) %>%
                dplyr::mutate(LONGITUDE=as.numeric(LONGITUDE)) %>%
                tidyr::drop_na(LATITUDE) %>%
                tidyr::drop_na(LONGITUDE)
        data <- eq_location_clean(data)
        data
}

#' Custom Geom to plot earthquakes by date and magnitude.
#'
#' This Geom uses ggplot2, grid packages.
#'
#' @importFrom ggplot2 Geom
#' @importFrom grid pointsGrob gpar
#'
#' @return Returns ggplot2 scatterplot
#'
#' @export
GeomTimeLine <- ggplot2::ggproto(
        "GeomTimeLine",
        ggplot2::GeomPoint,
        required_aes = c("x"),
        non_missing_aes = c("size", "shape", "colour"),
        default_aes = ggplot2::aes_(
                shape = 19, colour = "black",
                size = 2, fill = NA,
                alpha = 0.5, stroke = 0.5,
                y = NULL
        ),
        setup_data = function(data, params) {
                data <- subset(data, data$DATE > params$xmin &
                                       data$DATE < params$xmax)
        },
        draw_key = ggplot2::draw_key_point,
        draw_panel = function(data,
                              panel_params,
                              coord,
                              xmin,
                              xmax) {
                coords <- coord$transform(data, panel_params)
                grid::pointsGrob(
                        coords$x, coords$y,
                        pch = coords$shape,
                        gp = grid::gpar(
                                col = alpha(coords$colour, coords$alpha),
                                fill = alpha(coords$fill, coords$alpha),
                                fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                lwd = coords$stroke * .stroke / 2
                        )
                )
        }
)

#' Geom to plot earthquakes by date and magnitude
#'
#' This Geom uses ggplot2 package.
#'
#' @importFrom ggplot2 layer
#'
#' @param data Tidy earthquake data, as dataframe
#' @param mapping Mapping asethetics
#' @param stat Value for Statistical transformation, as string
#' @param position Position adjustment for the layer, as string
#' @param na.rm Remove missing values
#' @param show.legend Weather to show legend or not, as NA/TRUE/FALSE/
#' @param inherit.aes Weather to show legend or not, TRUE/FALSE/
#' @param xmin Minimum Date to plot
#' @param xmax Maximum Date to plot
#' @param ... Other asethetics/parameters for plotting
#'
#' @return A Geom object that is used within the \code{geom_timeline_label} function.
#'
#' @examples
#' \dontrun{
#' geom_timeline(read_noaa_file(filename="signif.txt"),
#'                              xmindate = "2000-01-01",
#'                              xmaxdate = "2017-12-31",
#'                              country = c("JAPAN"))
#' }
#'
#' @return Returns ggplot2 labels
#'
#' @export
geom_timeline <-
        function(data = NULL,
                 mapping = NULL,
                 stat = "identity",
                 position = "identity",
                 na.rm = FALSE,
                 show.legend = NA,
                 inherit.aes = TRUE,
                 xmin = NULL ,
                 xmax = NULL,
                 ...) {
                ggplot2::layer(
                        data = data,
                        mapping = mapping,
                        stat = stat,
                        geom = GeomTimeLine,
                        position = position,
                        show.legend = show.legend,
                        inherit.aes = inherit.aes,
                        params = list(
                                na.rm = na.rm,
                                xmin = xmin ,
                                xmax = xmax,
                                ...
                        )
                )
        }

#' Custom Geom to plot earthquake labels for date or location, magnitude, deaths.
#'
#' This Geom uses ggplot2, grid packages.
#'
#' @importFrom ggplot2 Geom
#' @importFrom grid pointsGrob grobTree segmentsGrob textGrob gpar
#'
#' @return Returns ggplot2 labels
#'
#' @export
GeomTimeLineLabel <- ggplot2::ggproto(
        "GeomTimeLineLabel",
        ggplot2::Geom,
        required_aes = c("x", "y"),
        default_aes = ggplot2::aes(
                countries = NULL,
                size = 1.5,
                location_name = NULL
        ),
        setup_data = function(data, params) {
                data <-subset(data, data$DATE > params$xmin &
                              data$DATE < params$xmax)
                data <- data[order(data$COUNTRIES,-data$SIZE), ]
                data <- by(data, data["COUNTRIES"], head, n = params$n_max)
                data <- Reduce(rbind, data)
        },
        draw_panel = function(data,
                              panel_scales,
                              coord,
                              xmin,
                              xmax,
                              n_max) {
                coords <- coord$transform(data, panel_scales)
                coords$y2 = coords$y + 0.1
                grid::grobTree(
                        grid::segmentsGrob(
                                x0 = coords$x,
                                y0 = coords$y,
                                x1 = coords$x,
                                y1 = coords$y2,
                                gp = grid::gpar(col = "grey")
                        ),
                        grid::textGrob(
                                x = coords$x,
                                y = coords$y2,
                                label = coords$LOCATION_NAME,
                                hjust = -0.1,
                                vjust = -0.1,
                                rot = 45,
                                gp = grid::gpar(col = coords$colour, fontsize = 10)
                        )
                )
        }
)

#' Geom to plot earthquake labels for date or location, magnitude, deaths.
#'
#' This Geom uses ggplot2 package.
#'
#' @importFrom ggplot2 layer
#'
#' @param data Tidy earthquake data, as dataframe
#' @param mapping Mapping asethetics
#' @param stat Value for Statistical transformation, as string
#' @param position Position adjustment for the layer, as string
#' @param na.rm Remove missing values
#' @param show.legend Weather to show legend or not, as NA/TRUE/FALSE/
#' @param inherit.aes Weather to show legend or not, TRUE/FALSE/
#' @param xmin Minimum Date to plot
#' @param xmax Maximum Date to plot
#' @param n_max Number of labels to plot
#' @param ... Other asethetics/parameters for plotting
#'
#' @examples
#' \dontrun{
#' geom_timeline_label(read_noaa_file(filename="signif.txt"),
#'                     xmindate = "2000-01-01",
#'                     xmaxdate = "2017-12-31",
#'                     country = c("JAPAN"),
#'                     n_max = 7.2)
#' }
#'
#' @return Returns ggplot2 labels
#'
#' @export
geom_timeline_label <-
        function(data = NULL,
                 mapping = NULL,
                 stat = "identity",
                 position = "identity",
                 na.rm = FALSE,
                 show.legend = FALSE,
                 inherit.aes = TRUE,
                 xmin = NULL ,
                 xmax = NULL,
                 n_max = 5,
                 ...) {
                ggplot2::layer(
                        geom = GeomTimeLineLabel,
                        mapping = mapping,
                        data = data,
                        stat = stat,
                        position = position,
                        show.legend = show.legend,
                        inherit.aes = inherit.aes,
                        params = list(
                                na.rm = na.rm,
                                xmin = xmin,
                                xmax = xmax,
                                n_max = n_max,
                                ...
                        )

                )
        }

#' Function to create label for loation, magnitude and deaths.
#'
#' This function uses shiny package.
#'
#' @param eq_data Tidy earthquake data
#'
#' @examples
#' \dontrun{
#' eq_create_label(data=eq_data)
#' }
#'
#' @return HTML label with location, magnitude and deaths
#'
#' @export
eq_create_label <- function(eq_data) {
        location_name <- ifelse(is.na(eq_data$LOCATION_NAME), "", paste("<b>Location:</b>", eq_data$LOCATION_NAME))
        magnitude <- ifelse(is.na(eq_data$EQ_PRIMARY), "",  paste("<br/><b>Magnitude:</b>", eq_data$EQ_PRIMARY))
        deaths <- ifelse(is.na(eq_data$TOTAL_DEATHS), "", paste("<br/><b>Total deaths:</b>", eq_data$TOTAL_DEATHS))

        paste(location_name, magnitude, deaths)
}

#' Function plots map based on data and annot_col.
#'
#' This function uses leaflet package.
#' 1.   Read file
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @param eq_data Tidy earthquake data
#' @param annot_col Column name from data used for the annotation in the pop-up
#'
#' @return ggplot2 plot
#'
#' @examples
#' \dontrun{
#' eq_map(annot_col = "DATE")
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_map <- function(eq_data, annot_col) {
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
                data = eq_data,
                radius = eq_data$EQ_PRIMARY,
                lng = eq_data$LONGITUDE,
                lat = eq_data$LATITUDE,
                fillOpacity = 0.5,
                stroke = FALSE,
                weight = 1,
                popup  = eq_data[[annot_col]]
        )
}
