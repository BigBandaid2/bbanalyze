#' parse_all_vr
#'
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
parse_all_vr <- function(dir_vr,
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

# check_db <- function(conn) {
#   return(T)
# }

### ----------------------------------------------------------------------------

#' cluster_times
#'
#' @param this_usage data.frame from parse_detail()
#' @param cluster logical. Defaults to F.
#'
#' @return This function takes the date/time and account of each row and clusters those rows with matching info
#'         together to be treated as part of a single request event.
#'
#' @importFrom  stats aggregate
#' @example \dontrun{
#'   filepath %>% parse_detail() %>% cluster_times()
#'   }
#' @export
cluster_times = function(this_usage, cluster = F) {

  ### find all output files that occur over multiple days and cluster them into 60 second groups
  outs = aggregate(this_usage$PROCESSED_TIME,
                   by = list(OUTPUT_ID = this_usage$OUTPUT_ID),
                   FUN = function(x) {difftime(range(x)[2], range(x)[1], units = "hours")})

  outs = outs[outs$x > 0,]

  data = data.frame()

  if(nrow(outs) > 0) {
    i = 1
    if(cluster == F) {
      for(i in 1:nrow(outs)) {
        single_output = this_usage[this_usage$OUTPUT_ID == outs[i,"OUTPUT_ID"],]
        # plot(single_output$PROCESSED_TIME)
        print(outs[i,"OUTPUT_ID"])
        print(length(single_output$PROCESSED_TIME))

        ### no more fancy h-cluster :(
        # # Ward Hierarchical Clustering
        # d <- dist(single_output$PROCESSED_TIME, method = "euclidean") # distance matrix
        # fit <- hclust(d, method="ward")
        # # plot(fit) # display dendogram
        # groups <- cutree(fit, h = 10) # cut tree at 10 second mark
        # # draw dendogram with red borders around the 5 clusters
        # # rect.hclust(fit, k=5, border="red")

        groups = single_output$PROCESSED_TIME

        print(table(groups))
        print(paste0(i," of ",nrow(outs)))
        single_output$OUTPUT_ID_cluster = paste0(single_output$OUTPUT_ID," (", groups,")")

        data = plyr::rbind.fill(data,single_output)
      }
    }

    ### incomplete, F cluster option just using unique PROCESSED TIMES

    if(cluster == T) {
      for(i in 1:nrow(outs)) {
        single_output = this_usage[this_usage$OUTPUT_ID == outs[i,"OUTPUT_ID"],]
        # plot(single_output$PROCESSED_TIME)
        print(outs[i,"OUTPUT_ID"])
        print(length(single_output$PROCESSED_TIME))
        print(length(unique(single_output$PROCESSED_TIME)))

        ### build groups just by unique times
        single_reqs = split(single_output,single_output$PROCESSED_TIME)
        for (j in 1:length(single_reqs)) {
          single_reqs[[j]]$OUTPUT_ID_cluster = paste0(single_reqs[[j]]$OUTPUT_ID," (", j,")")
        }

        gc()
        single_output = unsplit(single_reqs, single_output$PROCESSED_TIME, drop = FALSE)

        data = plyr::rbind.fill(data,single_output)
        print(table(single_output$OUTPUT_ID_cluster))
        print(paste0(i," of ",nrow(outs)))
      }
    }
  }


  ### add back all the non-repeating outs
  outs = aggregate(this_usage$PROCESSED_TIME,
                   by = list(OUTPUT_ID = this_usage$OUTPUT_ID),
                   FUN = function(x) {difftime(range(x)[2], range(x)[1], units = "hours")})

  outs = outs[outs$x == 0,]
  single_output = this_usage[this_usage$OUTPUT_ID %in% outs$OUTPUT_ID,]
  single_output$OUTPUT_ID_cluster = single_output$OUTPUT_ID

  data = plyr::rbind.fill(data,single_output)
  length(unique(data$OUTPUT_ID_cluster))
  length(unique(data$OUTPUT_ID))

  ### now i've seperated all the requests that went out with the same output ID, this will help me match to .req files

  ### Let's find the largest requests and cross them against DL Categroies

  ### this shows me all 3000 .out files and the distribution of DL categories asked for in each
  names(data)
  # write.csv(table(data$OUTPUT_ID_cluster,data$BILLABLE_PRODUCT)[,order(table(data$BILLABLE_PRODUCT), decreasing = T)], file = "dusseldorf_reqs.csv")

  return(data)
}

### ----------------------------------------------------------------------------

# product_split <- function(filepath) {
#   return(T)
# }
