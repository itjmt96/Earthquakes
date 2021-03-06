% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquakes.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{Geom to plot earthquake labels for date or location, magnitude, deaths.}
\usage{
geom_timeline_label(data, xmindate, xmaxdate, country, n_max)
}
\arguments{
\item{data}{Tidy earthquake data, as dataframe}

\item{xmindate}{Minimum Date to plot}

\item{xmaxdate}{Maximum Date to plot}

\item{country}{Character vector of Country/Countries to plot}

\item{n_max}{Number of labels to plot}
}
\value{
Returns ggplot
}
\description{
This Geom uses ggplot2 package.
}
\examples{
\dontrun{
geom_timeline_label(data = read_noaa_file(filename="signif.txt"),
                    xmindate = "2000-01-01",
                    xmaxdate = "2017-12-31",
                    country = c("JAPAN"),
                    n_max = 7)
}

}

