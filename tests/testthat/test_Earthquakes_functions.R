context("Test Earthquakes Package functions")

library(readr)
library(dplyr)
library(ggplot2)
library(testthat)
library(Earthquakes)
setwd(system.file("extdata", package = "Earthquakes"))

test_that("read_noaa_file(filename) does not find file", {
        expect_error(read_noaa_file('abcd'), "file 'abcd' does not exist", fixed=TRUE)
})

test_that("eq_location_clean(data) cleans LOCATION_NAME field", {
        nodata <- data.frame(LOCATION_NAME= character(0))
        data <- rbind(nodata, data.frame(LOCATION_NAME = "COUNTRY : SOME LOCATION, SOME OTHER LOCATION"))
        expect_that(eq_location_clean(data), is_a("data.frame"))
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

test_that("geom_timeline returns htmlwidget", {
        data <- eq_clean_data("signif.txt")
        expect_that(geom_timeline(data, xmindate = "2001-01-01", xmaxdate = "2017-12-31", country = c("MEXICO")), is_a("ggplot"))
        expect_that(data %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% eq_map(annot_col = "DATE"), is_a("htmlwidget"))
        })

test_that("geom_timeline_label returns ggplot",{
        data <- eq_clean_data("signif.txt")
        expect_that(geom_timeline_label(data, xmindate = "2001-01-01", xmaxdate = "2017-12-31", country = c("MEXICO"), n_max = 5.2), is_a("ggplot"))
})


test_that("eq_map(eq_data, annot_col) creates a leaflet", {
        expect_is(eq_clean_data("signif.txt") %>%
                  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                          eq_map(annot_col = "DATE"), "leaflet")
})
