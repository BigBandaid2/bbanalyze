### code to prepare `pub_rate_card` dataset goes here
require(magrittr)
require(usethis)

rc_by_asset = readr::read_csv(file.path(".","data-raw","sched-by-asset-type.csv"))
rc_by_asset %<>% tidyr::gather(key = "Asset.Type", value = "List.Price",-Band,-Data.Category)
rc_by_asset %<>% dplyr::mutate(Request.Type = "Unique", Fee.Type = "SCHEDULED")

rc_by_dl = readr::read_csv(file.path(".","data-raw","sched-by-dl.csv"))
rc_by_dl %<>% tidyr::gather(key = "Data.Category", value = "List.Price",-Band)
rc_by_dl %<>% dplyr::mutate(Request.Type = "Unique",
                               Fee.Type = "SCHEDULED",
                               Asset.Type = "unknown")

rc = rbind(rc_by_asset, rc_by_dl)
rc$Band = gsub("25-Jan","1-25",rc$Band)

# list_rate_card
df = list_rate_card %>% dplyr::filter(Fee.Type == "SCHEDULED" & Request.Type == "Unique")

### ok, I'm going to loop through each of the entries pulled from real rate cards
### then use that to assign missing info to by espionage rate card

i = 1
for (i in 1:nrow(df)) {
  print(df[i,])
  print(rc %>% dplyr::filter(List.Price == df[i,"List.Price"]))
}

df = readr::read_csv(file.path(".","data-raw","ad-hoc-rc.csv"))
df %<>% tidyr::gather(key = "Data.Category", value = "List.Price",-Asset.Type)
df %<>% dplyr::mutate(Request.Type = "Unique", Fee.Type = "AD-HOC", Band = NA)

rc = rbind(rc,df)
rc %<>% dplyr::filter(!is.na(List.Price))
pub_rate_card = rc

usethis::use_data(pub_rate_card)

################################################################################

### code to read in basic reference sheets

### get the Data License categories

dl_cats = readr::read_csv(file.path(".","data-raw","fields.csv"), col_types = readr::cols(.default = "c"))
usethis::use_data(dl_cats)

dl_products = read.csv(file.path(".","data-raw","DLContentSets.csv"))
sort(unique(dl_products$ModuleLevel2))
usethis::use_data(dl_products)

# bbg_mapping = readLines(file.path(".","data-raw","BbgMapping.conf"))
# usethis::use_data(bbg_mapping)

# setwd("C:/Users/EthanShen/Documents/GitHub/gain_shiny/bbanalyze")
# ?read.csv
asset_class_mapping = read.csv(file.path(".","data-raw","vr_asset_class_mapping.csv"), stringsAsFactors = F)
head(asset_class_mapping)
usethis::use_data(asset_class_mapping, overwrite = T)

product_mapping = read.csv(file.path(".","data-raw","vr_product_dl_cat_mapping.csv"), stringsAsFactors = F)
head(product_mapping)
usethis::use_data(product_mapping)

################################################################################

### code to read in bulk_fields.xlsx

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

### reads bulk_fields.xlsx - i don't currently use any data out of that file

filename = file.path(".","data-raw","bulk_fields.xlsx")
sheets <- readxl::excel_sheets(filename)
sheet = as.data.frame(readxl::read_excel(filename, sheet = sheets[1]))

data = read_excel_allsheets(filename)

### here I'm looking for columns that look like field mnemonics
head(sheet[,1],10)
grepl('^[A-Za-z0-9_]*$',head(sheet[,1],10))
as.vector(sheet[grep('Field Mnemonic',head(sheet[,1],10)),])
# ?grepl

data$`871(m)`[!grepl('^[A-Za-z0-9_]*$',data$`871(m)`[,1]),1]
fields = data$`871(m)`[grepl('^[A-Za-z0-9_]*$',data$`871(m)`[,1]),]

head(fields)
names(fields) = as.vector(sheet[grep('Field Mnemonic',head(sheet[,1],10)),])
fields$`Field Mnemonic`
fields$`Field Mnemonic`[!duplicated(fields$`Field Mnemonic`)]

bulk_fields = data.frame()
data_sections = data.frame()
bulk_fields_by_product = data.frame()

sheet = data[[1]]
length(data)
names(data)

i = 1
for (i in 1:length(data)) {
  print(names(data)[i])
  sheet = data[[i]]

  this.fields = sheet[grepl('^[A-Z0-9_&%#//+.]*$',sheet[,1]),]
  if(nrow(this.fields) == 0) {
    next()
  }
  names(this.fields) = gsub(" ",".", as.vector(sheet[grep('Field Mnemonic',sheet[,1])[1],]))

  this.sections = sheet[grepl('^All|.out|.px|.csv|.trr|.hdc',sheet[,1]),1]

  this.sections = data.frame(sheet = rep(sheet[1,1],length(this.sections)), section = this.sections)

  this.fields$product = names(data)[i]

  if(nrow(this.sections) > 0) {
    this.sections$product = names(data)[i]
    data_sections = plyr::rbind.fill(data_sections,this.sections)
  }

  bulk_fields = plyr::rbind.fill(bulk_fields,this.fields)

  # for(mnemonic in this.fields$Field.Mnemonic) {
  #   bulk_fields_by_product[mnemonic, "mnemonic"] = mnemonic
  #   bulk_fields_by_product[mnemonic, sheet[1,1]] = T
  # }

}

bulk_fields = bulk_fields[,1:8]

usethis::use_data(data_sections)
usethis::use_data(bulk_fields, overwrite = T)
####### END construction of bulk_fields_by_product #############################################################
