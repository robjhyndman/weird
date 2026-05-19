# Load fetch datasets from data-raw if available (source tree only, not in
# installed package). Tests that use these objects should skip_if(is.null(.)).
air_quality_raw <- if (file.exists("../../data-raw/air_quality.rds")) {
  readRDS("../../data-raw/air_quality.rds")
} else {
  NULL
}

wine_reviews_raw <- if (file.exists("../../data-raw/wine_reviews.rds")) {
  readRDS("../../data-raw/wine_reviews.rds")
} else {
  NULL
}
