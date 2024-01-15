fetch_wine_reviews <- function() {
  # Download to temporary file to avoid repeated downloads in the same session
  dest_folder <- tempdir()
  dest_file <- paste0(dest_folder,"/wine_reviews.rds")
  if(!file.exists(dest_file)) {
    download.file(
      url = "https://github.com/robjhyndman/weird-package/raw/main/data-raw/wine_reviews.rds",
      destfile = dest_file
    )
  }
  readRDS(dest_file)
}
