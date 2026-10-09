test_that("oz_books has expected dimensions and column names", {
  skip_if(is.null(oz_books_raw))
  expect_equal(nrow(oz_books_raw), 31258L)
  expect_named(
    oz_books_raw,
    c("title", "author", "year", "gutenberg_id", "chapter", "paragraph", "text")
  )
})

test_that("oz_books has 14 Baum and 15 Thompson books", {
  skip_if(is.null(oz_books_raw))
  books <- unique(oz_books_raw[c("title", "author")])
  expect_equal(nrow(books), 29L)
  expect_equal(sum(books$author == "L. Frank Baum"), 14L)
  expect_equal(sum(books$author == "Ruth Plumly Thompson"), 15L)
})

test_that("oz_books chapters are numbered consecutively from 1", {
  skip_if(is.null(oz_books_raw))
  ok <- tapply(oz_books_raw$chapter, oz_books_raw$title, function(ch) {
    u <- unique(ch)
    all(u == seq_along(u)) && !is.unsorted(ch)
  })
  expect_true(all(ok))
})

test_that("oz_books text contains no editorial material", {
  skip_if(is.null(oz_books_raw))
  expect_false(any(grepl("Gutenberg|\\[Illustration|Transcriber", oz_books_raw$text)))
  expect_false(any(grepl("^(CHAPTER|Chapter)\\b", oz_books_raw$text)))
  expect_false(any(grepl("[“”‘’_]", oz_books_raw$text)))
})
