#' Hass Avocado Weekly US Sales
#'
#' @description Weekly summary of Hass avocado sales for the United States
#'
#' @details This dataset contains a weekly sales summary for Hass avocados for the US.
#'
#' @format a dataframe with 11 variables.
#' \describe{
#'   \item{week_ending}{The date of the last day of the week in YYYY-MM-DD format.}
#'   \item{type}{Whether the avocado type is conventional (meaning non-organic) or organic.}
#'   \item{avg_selling_price}{The average selling price (which is NOT the advertised selling price) in US$ for Hass avocados. Not adjusted for inflation.}
#'   \item{total_bulk_and_bags_units}{The number of avocados sold in bulk (i.e., individually) or in bags. A unit is 1 avocado - not 1 bag.}
#'   \item{plu4046_units}{The amount or number of PLU4046 (non-organic) or PLU94046 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4225_units}{The amount or number of PLU4225 (non-organic) or PLU94225 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4770_units}{The amount or number of PLU4770 (non-organic) or PLU94770 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{total_bagged_units}{The amount of avocados sold in bags. One unit refers to one avocado, not 1 bag. From 2021 onwards, this value is not broken down by size of bag.}
#'   \item{sml_bagged_units}{The amount of avocados sold in small bags. One unit refers to one avocado. This bag typically consists of Hass #60 sizes & smaller.}
#'   \item{lrg_bagged_units}{The amount of avocados sold in large bags. One unit refers to one avocado. This bag typically consists of Hass #40 & Hass #48 sizes.}
#'   \item{xlrg_bagged_units}{TThe amount of avocados sold in extra large bags. One unit refers to one avocado. This bag typically consists of Hass #36 sizes or larger.}
#' }
#'
#' @source Hass Avocado Board \href{https://hassavocadoboard.com}{Category Data}
#' @keywords avocados timeseries revenue datasets
#'
"hass_usa"

#' Hass Avocado Regional Sales
#'
#' @description Weekly summary of Hass avocado sales for the regions within the United States
#'
#' @details This dataset contains a weekly sales summary for Hass avocados for regions (as defined by the Hass Avocado Board) within the US.
#'
#' @format a dataframe with 12 variables.
#'
#' \describe{
#'   \item{region}{The region of the United States. Defined by the Hass Avocado Board for their reporting. Each region may consist of multiple markets found in different states. See the Hass Avocado Board website for more details.}
#'   \item{week_ending}{The date of the last day of the week in YYYY-MM-DD format.}
#'   \item{type}{Whether the avocado type is conventional (meaning non-organic) or organic.}
#'   \item{avg_selling_price}{The average selling price (which is NOT the advertised selling price) in US$ for Hass avocados. Not adjusted for inflation.}
#'   \item{total_bulk_and_bags_units}{The number of avocados sold in bulk (i.e., individually) or in bags. A unit is 1 avocado - not 1 bag.}
#'   \item{plu4046_units}{The amount or number of PLU4046 (non-organic) or PLU94046 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4225_units}{The amount or number of PLU4225 (non-organic) or PLU94225 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4770_units}{The amount or number of PLU4770 (non-organic) or PLU94770 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{total_bagged_units}{The amount of avocados sold in bags. One unit refers to one avocado, not 1 bag. From 2021 onwards, this value is not broken down by size of bag.}
#'   \item{sml_bagged_units}{The amount of avocados sold in small bags. One unit refers to one avocado. This bag typically consists of Hass #60 sizes & smaller.}
#'   \item{lrg_bagged_units}{The amount of avocados sold in large bags. One unit refers to one avocado. This bag typically consists of Hass #40 & Hass #48 sizes.}
#'   \item{xlrg_bagged_units}{TThe amount of avocados sold in extra large bags. One unit refers to one avocado. This bag typically consists of Hass #36 sizes or larger.}
#' }
#'
"hass_region"

#' Hass Avocado Market Sales
#'
#' @description Weekly summary of Hass avocado sales for specific markets found within regions of the United States
#'
#' @details This dataset contains a weekly sales summary for Hass avocados for locations within regions (as defined by the Hass Avocado Board) the US.
#'
#' @format a dataframe with 13 variables
#'
#' \describe{
#'   \item{region}{The region of the United States. Defined by the Hass Avocado Board for their reporting. Each region may consist of multiple markets found in different states. See the Hass Avocado Board website for more details.}
#'   \item{market}{The market within the specified region of the United States. This market typically represents a major metropolitan city (or cities) reporting the highest sales.}
#'   \item{week_ending}{The date of the last day of the week in YYYY-MM-DD format.}
#'   \item{type}{Whether the avocado type is conventional (meaning non-organic) or organic.}
#'   \item{avg_selling_price}{The average selling price (which is NOT the advertised selling price) in US$ for Hass avocados. Not adjusted for inflation.}
#'   \item{total_bulk_and_bags_units}{The number of avocados sold in bulk (i.e., individually) or in bags. A unit is 1 avocado - not 1 bag.}
#'   \item{plu4046_units}{The amount or number of PLU4046 (non-organic) or PLU94046 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4225_units}{The amount or number of PLU4225 (non-organic) or PLU94225 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{plu4770_units}{The amount or number of PLU4770 (non-organic) or PLU94770 (organic) Hass avocados sold. Use the type column to determine if the units is for non-organic or organic avocados.}
#'   \item{total_bagged_units}{The amount of avocados sold in bags. One unit refers to one avocado, not 1 bag. From 2021 onwards, this value is not broken down by size of bag.}
#'   \item{sml_bagged_units}{The amount of avocados sold in small bags. One unit refers to one avocado. This bag typically consists of Hass #60 sizes & smaller.}
#'   \item{lrg_bagged_units}{The amount of avocados sold in large bags. One unit refers to one avocado. This bag typically consists of Hass #40 & Hass #48 sizes.}
#'   \item{xlrg_bagged_units}{TThe amount of avocados sold in extra large bags. One unit refers to one avocado. This bag typically consists of Hass #36 sizes or larger.}
#' }
#'
"hass_market"
