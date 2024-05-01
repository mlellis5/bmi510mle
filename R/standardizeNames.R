#' @title Standardize Variable Names
#'
#' @description This function converts the variable names in input tibble 'data' to "small_camel" case
#' using the dplyr::rename_with and janitor::make_clean_names functions.
#'
#' @param data A tibble data frame.
#'
#' @return A tibble data frame with variable names standardized to "small_camel" case.
#'
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#'
#' @export
#'
#' @examples
#' data = tibble::tribble(
#'   ~First_Name, ~Last_Name,
#'   "John", "Doe"
#' )
#' standardized_data = standardizeNames(data)
#' # Output:
#' #   firstName lastName
#' #   <chr>     <chr>
#' # 1 John      Doe
#'

standardizeNames = function(data) {
  # Rename variables to "small_camel" case using dplyr::rename_with and janitor::make_clean_names
  renamed_data = data |>
    dplyr::rename_with(~ janitor::make_clean_names(.x, case = "small_camel"))

  return(renamed_data)
}
