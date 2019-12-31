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

#' Title
#'
#' @param conn
#' @param dir_vr
#' @param target_account
#' @param target_month
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param conn
#' @param filename
#'
#' @return
#' @import magrittr
#' @export
#'
#' @examples
parse_detail = function(conn, filename) {

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

#' Title
#'
#' @param conn
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
parse_summary = function(conn, filename) {

}

# parse_refresh = function(conn, filename) {
#
# }

#' Title
#'
#' @param conn
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
check_db = function(conn, ...) {

}

#' Title
#'
#' @param df
#' @param true
#' @param false
#'
#' @return
#' @export
#'
#' @examples
to_logical_df = function(df, true, false) {

}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
cluster_times = function(df) {

}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
dl_product_split = function(df) {

}

