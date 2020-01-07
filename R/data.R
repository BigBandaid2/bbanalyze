#' Standard Enterpirse Data License Ad-Hoc rate card list prices from standard invoices
#'
#' @source Bloomberg invoices
#' @format A data frame with columns:
#' \describe{
#'  \item{Request.Type}{Initital monthly charge (Unique) or subsequent (Access) charges.}
#'  \item{Data.Category}{SecMaster, Pricing, Corporate Actions, etc.}
#'  \item{Fee.Type}{Ad-Hoc or Scheduled, rate card model.}
#'  \item{Asset.Type}{Equity,Fixed Income, Muni, etc.}
#'  \item{Product}{Product name as it appears in Bloomberg invoice line item}
#'  \item{List.Price}{Standard rate card price}
#' }
#' @examples
#' \dontrun{
#'  list_rate_card
#' }
"list_rate_card"

#' Standard Enterpirse Data License Ad-Hoc rate card list prices from published rates
#'
#' @source Bloomberg advertised rates
#' @format A data frame with columns:
#' \describe{
#'  \item{Request.Type}{Initital monthly charge (Unique) or subsequent (Access) charges.}
#'  \item{Data.Category}{SecMaster, Pricing, Corporate Actions, etc.}
#'  \item{Fee.Type}{Ad-Hoc or Scheduled, rate card model.}
#'  \item{Asset.Type}{Equity,Fixed Income, Muni, etc.}
#'  \item{Band}{Unique count range to trigger this rate}
#'  \item{List.Price}{Standard rate card price}
#' }
#' @examples
#' \dontrun{
#'  pub_rate_card
#' }
"pub_rate_card"

#' Mapping of names for asset classes across rate cards, vrs and invoices
#'
#' @source Bloomberg invoices
#' @format A data frame with columns:
#' \describe{
#'  \item{Asset.Class}{Name on Invoices}
#'  \item{Adhoc.Name}{Name on Adhoc Verification Reports}
#'  \item{Scheduled.Name}{Name on Scheduled Verification Reports}
#' }
#' @examples
#' \dontrun{
#'  asset_class_mapping
#' }
"asset_class_mapping"

#' Mapping of names for products across rate cards, vrs and invoices
#'
#' @source Bloomberg invoices
#' @format A data frame with columns:
#' \describe{
#'  \item{Categories}{Name on Invoices}
#'  \item{Adhoc.Name}{Name on Adhoc Verification Reports}
#'  \item{Scheduled.Name}{Name on Scheduled Verification Reports}
#' }
#' @examples
#' \dontrun{
#'  product_mapping
#' }
"product_mapping"

#' Inventory of all Field Mnemonics available through Per Security
#'
#' @source Bloomberg
#' @format A data frame with columns:
#' @examples
#' \dontrun{
#'  dl_cats
#' }
"dl_cats"

#' List of all Back Office offered by Bloomberg
#'
#' @source Bloomberg
#' @format A data frame with columns:
#' @examples
#' \dontrun{
#'  dl_products
#' }
"dl_products"
