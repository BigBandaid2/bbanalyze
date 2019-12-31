### general set up
### wipes the global environment clean from memory
rm(list = ls())

# readClipboard()
# this is where you hardcode you "working directory"
#######################################################
# set_ws = function(wd = "C:\\Users\\Ethan Shen\\Documents\\GitHub\\bbanalyze") {
set_ws = function(wd = file.path("C:","Users","Ethan Shen","Downloads","temp-aim","cost-analysis")) {
  setwd(wd)
  print(getwd())
}
scripts_dir = file.path("C:","Users","Ethan Shen","Documents","GitHub","DataPortal")
#######################################################

### enter a string here which is your working directory
# you can also copy the dir_path from windows explorer and execute below
# set_ws(readClipboard())

set_ws()
# target_client = "mizuho"
# dir_vr = file.path(".",paste0(target_client,"-cost-analysis"),"inputs","verification_reports")
# target_account = 30358785
# target_month = 201809

############################

# library(RSQLite)
conn <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(".","app","db.sqlite"))
# dbDisconnect(conn)
