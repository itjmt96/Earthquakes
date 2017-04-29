read_noaa_file <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_delim(filename, delim = "\t", col_names = TRUE, progress = FALSE)
        })
}

eq_location_clean <- function(data) {
        data <- data %>%
                dplyr::mutate(LOCATION_NAME = tools::toTitleCase(tolower(trimws(
                        gsub(".*:", "", data$LOCATION_NAME)
                ))))

        data
}

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

eq_create_label <- function(eq_data) {
        location_name <- ifelse(is.na(eq_data$LOCATION_NAME), "", paste("<b>Location:</b>", eq_data$LOCATION_NAME))
        magnitude <- ifelse(is.na(eq_data$EQ_PRIMARY), "",  paste("<br/><b>Magnitude:</b>", eq_data$EQ_PRIMARY))
        deaths <- ifelse(is.na(eq_data$TOTAL_DEATHS), "", paste("<br/><b>Total deaths:</b>", eq_data$TOTAL_DEATHS))

        paste(location_name, magnitude, deaths)
}

filter_date_and_country = function(data, xmindate, xmaxdate, country){
        xmindate = as.Date(xmindate)
        xmaxdate = as.Date(xmaxdate)

        NOAA_filtered = data %>%
                dplyr::filter(DATE > xmindate, DATE < xmaxdate) %>%
                dplyr::filter(COUNTRY %in% country) %>%
                dplyr::filter(!is.na(EQ_PRIMARY))
}

geom_timeline = function(data, xmindate, xmaxdate, country) {
        #data_filtered = filter_date_and_country(data, xmindate, xmaxdate, country)

        data <- subset(data,
                       data$DATE > xmindate &
                       data$DATE < xmaxdate &
                       data$COUNTRY %in% country)

        ggplot2::ggplot(data, aes(x = DATE, y = COUNTRY,
                                           size = EQ_PRIMARY, colour = DEATHS)) +
                ggplot2::geom_point(shape=21)+
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

geom_timeline_label = function(data, xmindate, xmaxdate, country, n_max){

        #data_filtered = filter_date_and_country(data, xmindate, xmaxdate, country)

        data <- subset(data,
                       data$DATE > xmindate &
                       data$DATE < xmaxdate &
                       data$COUNTRY %in% country)

        ggplot2::ggplot(data, aes(x = DATE, y = COUNTRY,
                                           size = EQ_PRIMARY,
                                           colour = DEATHS,
                                           label=LOCATION_NAME)) +
                ggplot2::geom_point(shape=21)+
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
                ggplot2::geom_text(data=data[data$EQ_PRIMARY > n_max,],
                                   aes(x = DATE,y = COUNTRY, angle = 45, size = 5),
                                   nudge_y = 0.1, hjust = 0, show.legend  = FALSE)
}

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
