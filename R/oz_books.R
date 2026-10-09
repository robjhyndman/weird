#' Oz books by L. Frank Baum and Ruth Plumly Thompson
#'
#' The text of 29 Oz books: the 14 written by L. Frank Baum (1900--1920), and
#' 15 of those written by Ruth Plumly Thompson (1921--1939). Each row is a
#' paragraph. Front matter, chapter headings and titles, illustration captions
#' and back matter have been removed, and curly quotes replaced by straight
#' quotes. The data are downloaded and returned.
#'
#' *The Royal Book of Oz* (1921) was credited to Baum, who had died in 1919,
#' but was written by Thompson. *The Magic of Oz* (1919) and *Glinda of Oz*
#' (1920) were published after Baum's death. Thompson's Oz books from 1931 to
#' 1934 are not included as they are not available from Project Gutenberg.
#'
#' @format A data frame with 31,258 rows and 7 columns:
#' \describe{
#'   \item{title}{Title of the book}
#'   \item{author}{Author of the book}
#'   \item{year}{Year of first publication}
#'   \item{gutenberg_id}{Project Gutenberg ebook number}
#'   \item{chapter}{Chapter number}
#'   \item{paragraph}{Paragraph number within the book}
#'   \item{text}{Text of the paragraph}
#' }
#' @return Data frame
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter 10,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' \dontrun{
#' oz_books <- fetch_oz_books()
#' oz_books |>
#'   group_by(title, author, year) |>
#'   summarise(paragraphs = n(), .groups = "drop") |>
#'   arrange(year)
#' }
#' @source Project Gutenberg, \url{https://www.gutenberg.org}. Downloaded 9 October 2026.
#' @aliases oz_books
#' @export
fetch_oz_books <- function() {
  fetch_rds("oz_books")
}
