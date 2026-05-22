# Based on utils.R from the tidyverse package

# List all packages loaded by weird
#
# @param include_self Include weird in the list?
# @return A character vector of package names.
# @export
# @examples
# weird_packages()
weird_packages <- function(include_self = FALSE) {
  raw <- utils::packageDescription("weird")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
  if (include_self) {
    names <- c(names, "weird")
  }
  names
}

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}
