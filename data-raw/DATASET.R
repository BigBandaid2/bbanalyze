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

# ?read.csv
asset_class_mapping = read.csv(file.path(".","data-raw","vr_asset_class_mapping.csv"), stringsAsFactors = F)
head(asset_class_mapping)
usethis::use_data(asset_class_mapping)

product_mapping = read.csv(file.path(".","data-raw","vr_product_dl_cat_mapping.csv"), stringsAsFactors = F)
head(product_mapping)
usethis::use_data(product_mapping)
