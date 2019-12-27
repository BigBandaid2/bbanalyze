#' Standard Enterpirse Data License Ad-Hoc rate card list prices
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
#'  maple
#' }
"list_rate_card"
