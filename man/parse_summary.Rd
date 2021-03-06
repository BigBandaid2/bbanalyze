% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_all_vr.R
\name{parse_summary}
\alias{parse_summary}
\title{parse_summary}
\usage{
parse_summary(conn, filepath)
}
\arguments{
\item{conn}{A character string identifying the database location}

\item{filepath}{A character string giving the filepath to be parsed}
}
\value{
dataframe containing the parsed results
        This function parses the summary verification reports for later comparison to the synthetic invoice derived
        in parse_detail. This function contains the following sub-functions:
                check_db() - checks the database to see if the file has been processed already
}
\description{
parse_summary
}
