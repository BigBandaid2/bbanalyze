#' Print "Hello world"
#'
#' This is a simple function that, by default, prints "Hello world". You can
#' customize the text to print (using the \code{to_print} argument) and add
#' an exclamation point (\code{excited = TRUE}).
#'
#' @param to_print A character string giving the text the function will print
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns a phrase to print, with or without an
#'    exclamation point added. As a side effect, this function also prints out
#'    the phrase.
#'
#' @examples
#' hello_world()
#' hello_world(excited = TRUE)
#' hello_world(to_print = "Hi world")
#'
#' @export
hello_world <- function(to_print = "Hello world", excited = FALSE){
  if(excited) to_print <- paste0(to_print, "!")
  print(to_print)
}




#' parse_all_vr
#'
#' @param db A character string identifying the database location
#' @param dir_vr A character string identifying the directory where verification reports are held
#' @param target_account The account to be parsed
#' @param target_month The month of verification reports to be parsed
#'
#' @return list(filenames, # of rows)
#'         This function reads/cleans/writes parsed verification files to a db for later use. The function
#'         is designed to process the verification reports of each month of each account. Nested functions include:
#'         parse_detail(), parse_summary(), parse_refresh()
#'
#' @examples
#' @export
parse_all_vr = function(conn,
                        dir_vr,
                        target_account,
                        target_month) {

  ### if usage detail file, send to parse_detail()
  filename = paste("usage_detail", target_account, target_month, sep = "_")
  if (filename %in% dir(dir_vr)) {
    this.usage = parse_detail(conn, filename)
  }
}





#' parse_detail
#'
#' @param db A character string identifying the database location
#' @param filename A character string giving the filename to be parsed
#'
#' @return dataframe containing the parsed results
#'         This function takes a given file and reads/clean/write the contents for later analysis.
#'         This function contains the following sub-functions:
#'                 check_db_detail() - checks the database to see if the file has been processed already
#'                 YNtoTF() - converts Y/N entries to T/F for better performance and numerical processing
#'                 cluster_times() - clusters rows of the verification reports by their date/account
#'                 product_split()
#' @examples
#' parse_detail()
#'
#' @export
parse_detail = function(db, filename) {

  ### read the usage detail file
  if(grepl("^usage_detail",filename)) {
    this.usage = readr::read_delim(
      file.path(dir_vr, filename),
      delim = "|",
      na = "N/A",
      # col_types = "ffffcfcfcccccccc",
      col_types = readr::cols(.default = "c"),
      trim_ws = T
    )
  }

  this.usage %<>% mutate_at(vars(matches('^CTRB|INIT|BO_OPT')), funs(. == 'Y'))
  this.usage %<>% mutate(tz = substr(this.usage$PROCESSED_TIME, 21,23))
  # table(this.usage$tz)

  this.usage$PROCESSED_TIME = as.POSIXct(paste0(
    substr(this.usage$PROCESSED_TIME, 1, 20),
    substr(this.usage$PROCESSED_TIME, 24, 28)
  ), format = "%c")
  ### BST timezone is not allowed, I've not checked if different timezones are used in various places

  # ?read_csv
  names(this.usage)

  ### if there are zero rows, then return the empty dataframe
  if(nrow(this.usage) == 0) {
    return(this.usage)
  }

  hist(table(this.usage$OUTPUT_ID))
  head(this.usage)

  ### special treatment for scheduled usage detail
  if(grepl("scheduled_securities",filename)) {
    this.usage$PRODUCT_TYPE = "Sched"
  }

  ### special treatment for ad-hoc usage detail
  if(grepl("^usage_detail",filename)) {
    this.usage$PRODUCT_TYPE = "Adhoc"
    names(this.usage) = gsub("QUALIFER","QUALIFIER",names(this.usage))

    table(is.na(this.usage$QUALIFIER))
    # this.usage$QUALIFIER[is.na(this.usage$QUALIFIER)] = " "
  }

  ### special treatment for BVAL usage detail
  # if(grepl("cashbval_usage_detail",filename)) {
  #   names(this.usage) = gsub("MATERIAL_DESC","BILLABLE_PRODUCT",names(this.usage))
  #   this.usage$QUALIFIER = " "
  # }

  # this.usage$BILLABLE_PRODUCT <- as.character(this.usage$BILLABLE_PRODUCT)
  return(this.usage)
}

#' parse_summary
#'
#' @param db A character string identifying the database location
#' @param filename A character string giving the filename to be parsed
#'
#' @return dataframe containing the parsed results
#'         This function parses the summary verification reports for later comparison to the synthetic invoice derived
#'         in parse_detail. This function contains the following sub-functions:
#'                 check_db_detail() - checks the database to see if the file has been processed already
#'
#' @examples
#' parse_summary()
#'
#' @export
parse_summary = function(db, filename) {

}

# parse_refresh = function(conn, filename) {
#
# }



#' check_db_detail
#'
#' @param db A character string identifying the database location
#'
#' @return True/False. This function checks the database to see that the given entry has been processed.
#'
#' @examples
#' check_db_detail()
#'
#' @export
check_db_detail = function(db) {

}

#' YntoTF
#'
#' @param filename A character string giving the filename to be parsed
#'
#' @return This function converts Y/N character entries to True/False [T/F] and returns a dataframe containing the
#'         modified output.
#'
#' @examples
#' YNtoTF()
#'
#' @export
YNtoTF = function(filename) {

}


#' cluster_times
#'
#' @param
#'
#' @return This function takes the date/time and account of each row and clusters those rows with matching info
#'         together to be treated as part of a single request event.
#'
#' @examples
#' cluster_times()
#'
#' @export
cluster_times = function(filename) {

}



#' product_split
#'
#' @param
#'
#' @return
#'
#' @examples
#' product_split()
#'
#' @export
product_split = function(filename) {

}
