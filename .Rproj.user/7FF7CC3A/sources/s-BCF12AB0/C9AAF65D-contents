# rm(list = ls())

filepath <- system.file("extdata", "usage_detail_30225811_201811", package = "bbanalyze")

test_that("sample usage_detail file reads and parses correctly", {
  expect_equal(
    filepath %>%
      parse_detail() %>%
      names(),
    c(
      "OUTPUT_ID",
      "SECURITY",
      "ID_BB_UNIQUE",
      "QUALIFIER",
      "PROCESSED_TIME",
      "DOWNLOAD_PRODUCT",
      "BILLABLE_PRODUCT",
      "SERVICE",
      "CTRB_BILLING",
      "CTRB_UNIQ",
      "CTRB_AC",
      "CTRB_MAIN",
      "INIT_CHARGE",
      "CTRB_PD",
      "CAPS",
      "BO_OPTIMIZED",
      "tz",
      "PRODUCT_TYPE",
      "CTRB_BAC"
    ),
    failure_message = "column names not matching expected"
  )
  expect_equal(
    system.file("extdata", "usage_detail_30225811_201811", package = "bbanalyze") %>%
      parse_detail() %>%
      nrow(),
    3608,
    failure_message = "number of rows not as expected"
  )
})

### ----------------------------------------------------------------------------

# this_usage = system.file("extdata", "scheduled_securities_30358785_201811", package = "bbanalyze") %>% parse_detail()

test_that("sample scheduled_securities file reads and parses correctly", {
  expect(
    all(system.file("extdata", "scheduled_securities_30358785_201811", package = "bbanalyze") %>%
      parse_detail() %>%
      names() %in%
    c(
      "OUTPUT_ID",
      "SECURITY",
      "ID_BB_UNIQUE",
      "QUALIFIER",
      "PROCESSED_TIME",
      "DOWNLOAD_PRODUCT",
      "BILLABLE_PRODUCT",
      "SERVICE",
      "CTRB_BILLING",
      "CTRB_UNIQ",
      "CTRB_AC",
      "CTRB_MAIN",
      "INIT_CHARGE",
      "CTRB_PD",
      "CAPS",
      "BO_OPTIMIZED",
      "tz",
      "PRODUCT_TYPE",
      "CTRB_BAC"
    )),
    failure_message = "column names not matching expected"
  )
  expect(
    all(
      (
        system.file("extdata", "scheduled_securities_30358785_201811", package = "bbanalyze") %>%
          parse_detail() %>%
          names()
      ) %in%
        (
          system.file("extdata", "usage_detail_30225811_201811", package = "bbanalyze") %>%
            parse_detail() %>%
            names()
        )
    ),
    failure_message = "column names for scheduled securities not fully contained in the superset of usage_detail"
  )
  expect_equal(
    system.file("extdata", "scheduled_securities_30358785_201811", package = "bbanalyze") %>%
      parse_detail() %>%
      nrow(),
    11801,
    failure_message = "number of rows not as expected"
  )
})

### ----------------------------------------------------------------------------

filepath <- system.file("extdata", "cashbval_usage_detail_30358785_201811", package = "bbanalyze")
# filepath %>% parse_detail()

test_that("sample cashbval_usage_detail file reads and parses correctly", {
  expect_equal(
    filepath %>%
      parse_detail() %>%
      nrow(),
    0,
    failure_message = "parse_detail() should ignore cashbval files and return empty data frame for now"
  )
})

### ----------------------------------------------------------------------------

conn = "placeholder"
dir_vr <- system.file("extdata", package = "bbanalyze")
target_account <- 30225811
target_month <- 201811

test_that("parse_all_vr is looping through the files", {
  expect_equal(
    parse_all_vr(conn,dir_vr,target_account,target_month) %>% nrow(),
    3608,
    failure_message = "parse_all_vr() should not getting correct row count"
  )
  expect_equal(
    parse_all_vr(conn,dir_vr,30358785,target_month) %>% nrow(),
    11801,
    failure_message = "parse_all_vr() should not getting correct row count"
  )
})

### ----------------------------------------------------------------------------

this_usage = system.file("extdata", "usage_detail_30225811_201811", package = "bbanalyze") %>% parse_detail()
