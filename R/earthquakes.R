utils::globalVariables(c("YEAR", "MONTH", "DAY", "DT", "DATE", "LATITUDE", "LONGITUDE",
                         "EQ_PRIMARY", "COUNTRY", "LOCATION_NAME", "DEATHS", "element_rect",
                         "element_blank", "element_line", "aes"))
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
#' @param data Data as dataframe object.
#'
#' @return Dataframe object with clean LOCATION_NAME fields data.
#'
#' @examples
#' \dontrun{
#' eq_location_clean(data=eq_clean)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom tools toTitleCase
#' @importFrom magrittr %>%
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
#' 5.   EQ_PRIMARY field data
#'      Mutates EQ_PRIMARY field data to numeric.
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
#' @importFrom dplyr filter mutate
#' @importFrom lubridate ymd
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
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
                dplyr::mutate(EQ_PRIMARY=as.numeric(EQ_PRIMARY)) %>%
                tidyr::drop_na(LATITUDE) %>%
                tidyr::drop_na(LONGITUDE)
        data <- eq_location_clean(data)
        data
}

#' Geom to plot earthquakes by date and magnitude
#'
#' This Geom uses ggplot2 package.
#'
#' @importFrom ggplot2 ggplot geom_point theme labs
#'
#' @param data Tidy earthquake data, as dataframe
#' @param xmindate Minimum Date to plot
#' @param xmaxdate Maximum Date to plot
#' @param country Character vector of Country/Countries to plot
#'
#' @examples
#' \dontrun{
#' geom_timeline(data = eq_clean_data('signif.txt'),
#'               xmindate = "2001-01-01",
#'               xmaxdate = "2017-12-31",
#'               country = c("MEXICO"))
#' }
#'
#' @return Returns ggplot
#'
#' @export
geom_timeline = function(data, xmindate, xmaxdate, country) {

        data <- subset(data,
                       data$DATE > xmindate &
                               data$DATE < xmaxdate &
                               data$COUNTRY %in% country)

        ggplot2::ggplot(data, aes(x = data$DATE,
                                  y = data$COUNTRY,
                                  size = data$EQ_PRIMARY,
                                  colour = data$TOTAL_DEATHS)) +
                ggplot2::geom_point(shape=21) +
                ggplot2::theme(legend.position = "bottom",
                               legend.direction = "horizontal",
                               legend.box = "horizontal",
                               legend.key = element_rect(fill = "white"),
                               axis.title.y = element_blank(),
                               panel.background = element_rect(fill = "white", colour = NA),
                               axis.line = element_line(colour = "black"),
                               axis.line.y = element_line(colour = "white"),
                               axis.ticks.y = element_line(colour = "white"))+
                ggplot2::labs(size = "Richter scale value", colour = "# deaths")
}

#' Geom to plot earthquake labels for date or location, magnitude, deaths.
#'
#' This Geom uses ggplot2 package.
#'
#' @importFrom ggplot2 ggplot geom_point theme labs geom_text
#'
#' @param data Tidy earthquake data, as dataframe
#' @param xmindate Minimum Date to plot
#' @param xmaxdate Maximum Date to plot
#' @param country Character vector of Country/Countries to plot
#' @param n_max Number of labels to plot
#'
#' @examples
#' \dontrun{
#' geom_timeline_label(data = read_noaa_file(filename="signif.txt"),
#'                     xmindate = "2000-01-01",
#'                     xmaxdate = "2017-12-31",
#'                     country = c("JAPAN"),
#'                     n_max = 7)
#' }
#'
#' @return Returns ggplot
#'
#' @export
geom_timeline_label = function(data, xmindate, xmaxdate, country, n_max){

        data <- subset(data,
                       data$DATE > xmindate &
                               data$DATE < xmaxdate &
                               data$COUNTRY %in% country)

        ggplot2::ggplot(data, aes(x = data$DATE,
                                  y = data$COUNTRY,
                                  size = data$EQ_PRIMARY,
                                  colour = data$TOTAL_DEATHS,
                                  label = data$LOCATION_NAME)) +
                ggplot2::geom_point(shape=21) +
                ggplot2::theme(legend.position = "bottom",
                               legend.direction = "horizontal",
                               legend.box = "horizontal",
                               legend.key = element_rect(fill = "white"),
                               axis.title.y = element_blank(),
                               panel.background = element_rect(fill = "white", colour = NA),
                               axis.line = element_line(colour = "black"),
                               axis.line.y = element_line(colour = "white"),
                               axis.ticks.y = element_line(colour = "white"))+
                ggplot2::labs(size = "Richter scale value", colour = "# deaths")+
                ggplot2::geom_text(data = data[data$EQ_PRIMARY > n_max,],
                                   aes(x = data$DATE,y = data$COUNTRY, angle = 45, size = 5),
                                   nudge_y = 0.1, hjust = 0, show.legend  = FALSE)
}

#' Function to create label for loation, magnitude and deaths.
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
                        fillOpacity = 0.2,
                        weight = 1,
                        popup  = eq_data[[annot_col]]
                )
}
