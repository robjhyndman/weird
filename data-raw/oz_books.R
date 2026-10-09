# Oz books by L. Frank Baum and Ruth Plumly Thompson, from Project Gutenberg
# Plain-text files downloaded 9 October 2026 from
# https://www.gutenberg.org/cache/epub/<id>/pg<id>.txt into data-raw/oz/
# The resulting data-raw/oz_books.rds is downloaded by fetch_oz_books().

library(dplyr)

baum <- "L. Frank Baum"
thompson <- "Ruth Plumly Thompson"
books <- tribble(
  ~gutenberg_id, ~title, ~author, ~year,
  55, "The Wonderful Wizard of Oz", baum, 1900,
  54, "The Marvelous Land of Oz", baum, 1904,
  486, "Ozma of Oz", baum, 1907,
  420, "Dorothy and the Wizard in Oz", baum, 1908,
  485, "The Road to Oz", baum, 1909,
  517, "The Emerald City of Oz", baum, 1910,
  955, "The Patchwork Girl of Oz", baum, 1913,
  956, "Tik-Tok of Oz", baum, 1914,
  957, "The Scarecrow of Oz", baum, 1915,
  958, "Rinkitink in Oz", baum, 1916,
  959, "The Lost Princess of Oz", baum, 1917,
  960, "The Tin Woodman of Oz", baum, 1918,
  419, "The Magic of Oz", baum, 1919,
  961, "Glinda of Oz", baum, 1920,
  30537, "The Royal Book of Oz", thompson, 1921,
  53765, "Kabumpo in Oz", thompson, 1922,
  58765, "The Cowardly Lion of Oz", thompson, 1923,
  61681, "Grampa in Oz", thompson, 1924,
  65849, "The Lost King of Oz", thompson, 1925,
  70152, "The Hungry Tiger of Oz", thompson, 1926,
  71273, "The Gnome King of Oz", thompson, 1927,
  73170, "The Giant Horse of Oz", thompson, 1928,
  75720, "Jack Pumpkinhead of Oz", thompson, 1929,
  78637, "The Yellow Knight of Oz", thompson, 1930,
  55851, "The Wishing Horse of Oz", thompson, 1935,
  56073, "Captain Salt in Oz", thompson, 1936,
  56079, "Handy Mandy in Oz", thompson, 1937,
  56085, "The Silver Princess in Oz", thompson, 1938,
  55806, "Ozoplaning with the Wizard of Oz", thompson, 1939
)

# Chapter headings: "Chapter One", "CHAPTER 1", "Chapter I. The Cyclone", ...
# Six Baum books number chapters as "1.  The Earthquake"
heading_default <- "^\\s*(CHAPTER|Chapter)\\s+[0-9IVXLA-Za-z-]+\\.?(\\s.*)?$"
heading_numbered <- "^[0-9]{1,2}\\.\\s+[A-Z]"
# Back matter after the last chapter
end_pattern <- paste0(
  "^\\s*(THE END|The End)\\.?\\s*$|^\\s*(THE FAMOUS OZ BOOKS|The Wonderful Oz Books)",
  "|^\\s*_?A Word about the|^\\s*\\[?(Transcriber|TRANSCRIBER)|^End of (the )?Project Gutenberg"
)

read_book <- function(id) {
  x <- readLines(here::here("data-raw/oz", paste0(id, ".txt")), encoding = "UTF-8", warn = FALSE)
  x <- sub("\r$", "", x)
  x[(grep("^\\*\\*\\* ?START OF", x) + 1):(grep("^\\*\\*\\* ?END OF", x) - 1)]
}

# Headings in a contents list sit on (nearly) consecutive lines; real chapter
# headings are separated by pages of text
body_headings <- function(idx) {
  gap_prev <- c(Inf, diff(idx))
  gap_next <- c(diff(idx), Inf)
  idx[gap_prev > 3 & gap_next > 3]
}

find_headings <- function(x, id) {
  if (id == 54) {
    # The Marvelous Land of Oz: headings are the bare chapter titles listed
    # after "LIST OF CHAPTERS"
    start <- grep("LIST OF CHAPTERS", x)[1]
    toc <- trimws(x[start + seq_len(60)])
    toc <- toc[toc != ""]
    toc <- toc[seq_len(which(toc == "The Riches of Content"))]
    # The contents list omits the "a" in the first chapter title
    toc <- c(toc, "Tip Manufactures a Pumpkinhead")
    idx <- which(tolower(trimws(x)) %in% tolower(toc))
  } else if (id %in% c(486, 420, 485, 517, 419)) {
    idx <- grep(heading_numbered, x)
  } else {
    # Excluding the "Chapter ... Page" header of a contents list
    idx <- setdiff(grep(heading_default, x), grep("^\\s*Chapter\\s+Page\\s*$", x))
  }
  body_headings(idx)
}

clean_book <- function(id) {
  x <- read_book(id)
  h <- find_headings(x, id)
  end <- grep(end_pattern, x)
  end <- c(end[end > max(h)], length(x) + 1)[1]
  x <- x[h[1]:(end - 1)]
  chapter <- cumsum(seq_along(x) %in% (h - h[1] + 1))
  tibble(chapter = chapter, line = trimws(x)) |>
    # Paragraphs are separated by blank lines
    mutate(paragraph = cumsum(line == "" | row_number() == 1)) |>
    filter(line != "") |>
    group_by(chapter, paragraph) |>
    summarise(text = paste(line, collapse = " "), lines = n(), .groups = "drop") |>
    group_by(chapter) |>
    mutate(r = row_number()) |>
    ungroup() |>
    # Drop the headings. Some books put each chapter title in a paragraph of its
    # own after the heading: then the second paragraph of (almost) every chapter
    # is a short single line
    mutate(has_titles = mean((lines == 1 & nchar(text) < 80)[r == 2]) > 0.5) |>
    filter(!(r == 1 | (r == 2 & has_titles))) |>
    mutate(
      text = gsub("\\[(Illustration|Transcriber)[^]]*\\]", "", text),
      text = gsub("_", "", text),
      text = gsub("[“”]", "\"", text),
      text = gsub("[‘’]", "'", text),
      text = gsub("\u00a0", " ", text),
      # Encoding error in the Gutenberg file of The Yellow Knight of Oz
      text = gsub("\u221a\u00b6", "\u00e6", text),
      text = trimws(gsub("\\s+", " ", text))
    ) |>
    # Drop separators such as "* * * * *" and empty illustration paragraphs
    filter(grepl("[A-Za-z]", text)) |>
    mutate(gutenberg_id = id, paragraph = row_number()) |>
    select(gutenberg_id, chapter, paragraph, text)
}

oz_books <- lapply(books$gutenberg_id, clean_book) |>
  bind_rows() |>
  left_join(books, by = "gutenberg_id") |>
  select(title, author, year, gutenberg_id, chapter, paragraph, text)

saveRDS(oz_books, here::here("data-raw/oz_books.rds"), compress = "xz")
