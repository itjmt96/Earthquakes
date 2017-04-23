context("Test Earthquakes Package functions")

library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(testthat)
library(Earthquakes)
setwd(system.file("extdata", package = "Earthquakes"))

test_that("read_naoo_file(filename) does not find file", {
        #print("read_naoo_file")
        expect_error(read_naoo_file('abcd'), "file 'abcd' does not exist", fixed=TRUE)
})

test_that("eq_clean_data(filename) reads dataframe", {
        data <- eq_clean_data("signif.txt")
        expect_that(data, is_a("data.frame"))
})

test_that("eq_create_label(eq_data) creates label", {
        nodata <- data.frame(LOCATION_NAME= character(0), EQ_PRIMARY= character(0), TOTAL_DEATHS = integer(0))
        eq_data <- rbind(nodata, data.frame(LOCATION_NAME = "Some Location", EQ_PRIMARY = "5.7", TOTAL_DEATHS = 5))
        expect_match(eq_create_label(eq_data), "<b>Location:</b> Some Location <br/><b>Magnitude:</b> 5.7 <br/><b>Total deaths:</b> 5")
})

test_that("eq_map(eq_data, annot_col) creates a leaflet", {
        expect_is(eq_clean_data("signif.txt") %>%
                  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                  eq_map(annot_col = "DATE"), "leaflet")
})

test_that("GeomTimeline returns GeomTimeline", {
        expect_is({eq_data <- eq_clean_data("signif.txt") %>%
                dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "NEW ZELAND") %>%
                ggplot(aes( x = DATE,
                            y = COUNTRY,
                            colour = TOTAL_DEATHS,
                            size = EQ_PRIMARY,
                            date = DATE,
                            countries = COUNTRY,
                            location_name = LOCATION_NAME))  %>%
                geom_timeLine()
        eq_data$layers[[1]]$geom}, "GeomTimeLine")
})

test_that("GeomTimelineLabel returns GeomTimeline", {
        expect_is({eq_data <- eq_clean_data("signif.txt") %>%
                dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "NEW ZELAND") %>%
                ggplot(aes( x = DATE,
                            y = COUNTRY,
                            colour = TOTAL_DEATHS,
                            size = EQ_PRIMARY,
                            date = DATE,
                            countries = COUNTRY,
                            location_name = LOCATION_NAME))  %>%
                geom_timeLine() %>%
                geom_timeline_label()
        eq_data$layers[[1]]$geom}, "geom_timeline_label")
})
