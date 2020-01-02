#' parse_all_vr
#'
#' @param conn A character string identifying the database location
#' @param dir_vr A character string identifying the directory where verification reports are held
#' @param target_account The account to be parsed
#' @param target_month The month of verification reports to be parsed
#'
#' @return list(filepaths, # of rows)
#'         This function reads/cleans/writes parsed verification files to a db for later use. The function
#'         is designed to process the verification reports of each month of each account. Nested functions include:
#'         parse_detail(), parse_summary(), parse_refresh()
#'
#' @importFrom  plyr rbind.fill
#' @export
parse_all_vr <- function(conn,
                         dir_vr,
                         target_account,
                         target_month) {
  ### get all VR files associated with this target month and account
  files = grep(paste(target_account,target_month,sep ="_"),dir(dir_vr), value = T)
  usage_detail = data.frame()

  for (file in files) {
    filepath <- file.path(dir_vr,file)

    ### if usage detail file of ad hoc , scheduled or cashbval, send to parse_detail()
    if (grepl("usage_detail|scheduled_securities", filepath)) {
      this_usage <- parse_detail(filepath)
      usage_detail = plyr::rbind.fill(usage_detail, this_usage)
    }
  }

  return(usage_detail)
}

### ----------------------------------------------------------------------------

#' parse_detail
#'
#' @param filepath A character string giving the filepath to be parsed
#'
#' @return dataframe containing the parsed results
#'         This function takes a given file and reads/clean/write the contents for later analysis.
#'         This function contains the following sub-functions:
#'                 check_db() - checks the database to see if the file has been processed already
#'                 YNtoTF() - converts Y/N entries to T/F for better performance and numerical processing
#'                 cluster_times() - clusters rows of the verification reports by their date/account
#'                 product_split()
#' @import magrittr
#' @import tibble
#' @import readr
#' @import dplyr
#' @export
parse_detail <- function(filepath) {
  stopifnot(file.exists(filepath))
  this_usage = data.frame()

  ### read the usage detail file
  if (grepl("usage_detail|scheduled_securities", filepath) &
      !grepl("_usage_detail", filepath)) {
    this_usage <- readr::read_delim(
      filepath,
      delim = "|",
      na = "N/A",
      # col_types = 'ffffcfcfcccccccc', ### sets the col types but commented out because super slow
      col_types = readr::cols(.default = "c"),
      trim_ws = T
    )
  } else {
    return(this_usage)
  }

  this_usage %<>% dplyr::mutate_at(dplyr::vars(dplyr::matches("^CTRB|INIT|BO_OPT")), dplyr::funs(. == "Y"))
  this_usage %<>% dplyr::mutate(tz = substr(this_usage$PROCESSED_TIME, 21, 23))
  # table(this_usage$tz)

  this_usage$PROCESSED_TIME <- as.POSIXct(paste0(
    substr(this_usage$PROCESSED_TIME, 1, 20),
    substr(this_usage$PROCESSED_TIME, 24, 28)
  ), format = "%c")
  ### BST timezone is not allowed, I've not checked if different timezones are used in various places

  ### if there are zero rows, then return the empty dataframe
  if (nrow(this_usage) == 0) {
    return(this_usage)
  }

  ### special treatment for scheduled usage detail
  if (grepl("scheduled_securities", filepath)) {
    this_usage$PRODUCT_TYPE <- "Sched"
  }

  ### special treatment for ad-hoc usage detail
  if (grepl("usage_detail", filepath) &
      !grepl("_usage_detail", filepath)) {
    this_usage$PRODUCT_TYPE <- "Adhoc"
    names(this_usage) <-
      gsub("QUALIFER", "QUALIFIER", names(this_usage))

    if (!this_usage %>% tibble::has_name("CTRB_BAC")) {
      this_usage %<>% mutate(CTRB_BAC = BO_OPTIMIZED * CTRB_BILLING)
    }

    # table(is.na(this_usage$QUALIFIER))
  }

  ### special treatment for BVAL usage detail, excluded for now
  # if(grepl('cashbval_usage_detail', filepath)) {
  #   names(this_usage) = gsub('MATERIAL_DESC', 'BILLABLE_PRODUCT', names(this_usage)) this_usage$QUALIFIER = ' '
  # }

  # this_usage$BILLABLE_PRODUCT <- as.character(this_usage$BILLABLE_PRODUCT)
  return(this_usage)
}

### ----------------------------------------------------------------------------

#' parse_summary
#'
#' @param conn A character string identifying the database location
#' @param filepath A character string giving the filepath to be parsed
#'
#' @return dataframe containing the parsed results
#'         This function parses the summary verification reports for later comparison to the synthetic invoice derived
#'         in parse_detail. This function contains the following sub-functions:
#'                 check_db() - checks the database to see if the file has been processed already
#'
#' @export
parse_summary <- function(conn, filepath) {
  return(T)
}

### ----------------------------------------------------------------------------

# parse_refresh <- function(conn, filepath) { }

### ----------------------------------------------------------------------------

#' check_db
#'
#' @param conn A character string identifying the database location
#'
#' @return True/False. This function checks the database to see that the given entry has been processed.
#'
#' @export
check_db <- function(conn) {
  return(T)
}

### ----------------------------------------------------------------------------

#' cluster_times
#'
#' @param filepath a good description
#'
#' @return This function takes the date/time and account of each row and clusters those rows with matching info
#'         together to be treated as part of a single request event.
#'
#' @export
cluster_times <- function(filepath) {
  return(T)
}

### ----------------------------------------------------------------------------

#' product_split
#'
#' @param filepath a very good description
#'
#' @return something
#'
#' @export
product_split <- function(filepath) {
  return(T)
}
