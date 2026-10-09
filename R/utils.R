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

# Download data-raw/{name}.rds from GitHub and read it. The file is cached in
# tempdir() to avoid repeated downloads in the same session, and any partial
# download is removed on failure so it is not mistaken for a cached copy.
fetch_rds <- function(name, dir = tempdir()) {
  dest_file <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(dest_file)) {
    tryCatch(
      download_file(
        url = paste0(
          "https://raw.githubusercontent.com/robjhyndman/weird/main/data-raw/",
          name,
          ".rds"
        ),
        destfile = dest_file,
        mode = "wb"
      ),
      error = function(e) {
        unlink(dest_file)
        stop(e)
      }
    )
  }
  readRDS(dest_file)
}

# Mockable wrapper for tests
download_file <- function(...) utils::download.file(...)
