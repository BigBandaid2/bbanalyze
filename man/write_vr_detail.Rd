% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_all_vr.R
\name{write_vr_detail}
\alias{write_vr_detail}
\title{Write the parsed VR detail file contents to DB}
\usage{
write_vr_detail(
  this_usage,
  conn,
  args_id,
  target_account,
  target_month,
  drop = F
)
}
\arguments{
\item{this_usage}{dataframe of the parsed VR detail}

\item{conn}{DB connection SQLite object}

\item{args_id}{client_id as it currently appears in the DB}

\item{target_account}{account number for this Bloomberg account}

\item{target_month}{billing month in 'MMYYYY'}

\item{drop}{logical if T then existing table will be dropped}
}
\value{
dataframe representing the whole table just written to DB
}
\description{
Write the parsed VR detail file contents to DB
}
