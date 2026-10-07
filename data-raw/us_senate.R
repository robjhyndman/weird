# US Senate co-voting networks, 40th to 113th Congress (Lee, Li & Wilson, 2020)

library(readr)
library(dplyr)

us_senate_edges <- read_csv(
  "data-raw/senate_votes_edgelist.csv",
  col_names = c("congress", "from", "to"),
  col_types = "iii"
) |>
  as_tibble()
us_senate_members <- read_csv(
  "data-raw/senate_party.csv",
  col_types = "iic"
) |>
  arrange(congress, node)

# Identify the senators. The nodes in each Congress are the rows of the legacy
# Voteview roll call file (President first, then alphabetical by state), after
# omitting members who cast no yea or nay votes, and Silver Party members.
ord_url <- function(congress) {
  file <- case_when(
    congress == 45 ~ "dtaord/sen45kh_2015.ord",
    congress == 106 ~ "sen106kh.ord",
    .default = paste0("dtaord/sen", congress, "kh.ord")
  )
  paste0("https://legacy.voteview.com/k7ftp/", file)
}
read_ord <- function(congress) {
  rows <- read_lines(ord_url(congress))
  rows <- rows[nchar(trimws(rows)) > 0]
  tibble(
    congress = congress,
    icpsr = as.integer(substr(rows, 4, 8)),
    party_code = as.integer(substr(rows, 20, 23)),
    votes = substr(rows, 37, nchar(rows))
  ) |>
    filter(grepl("[1-6]", votes), party_code != 1060) |>
    mutate(node = row_number())
}
ord <- lapply(40:113, read_ord) |>
  bind_rows()
stopifnot(identical(table(ord$congress), table(us_senate_members$congress)))

# Check that every published edge joins two senators who voted the same way on
# at least 75% of the roll calls for which both were present
for (k in 40:113) {
  votes <- do.call(rbind, strsplit(ord$votes[ord$congress == k], ""))
  yea <- matrix(votes %in% 1:3, nrow(votes))
  nay <- matrix(votes %in% 4:6, nrow(votes))
  agree <- (tcrossprod(yea) + tcrossprod(nay)) / tcrossprod(yea + nay)
  edges <- us_senate_edges[us_senate_edges$congress == k, ]
  stopifnot(all(agree[cbind(edges$from, edges$to)] >= 0.75))
}

members <- read_csv(
  "https://voteview.com/static/data/out/members/Sall_members.csv",
  col_types = cols(congress = "i", icpsr = "i", .default = "c")
) |>
  select(congress, icpsr, name = bioname, state = state_abbrev)
us_senate_members <- us_senate_members |>
  left_join(select(ord, congress, node, icpsr), by = c("congress", "node")) |>
  left_join(members, by = c("congress", "icpsr")) |>
  select(congress, node, name, state, party, icpsr) |>
  as_tibble()
stopifnot(!anyNA(us_senate_members))

usethis::use_data(
  us_senate_edges,
  us_senate_members,
  overwrite = TRUE,
  compress = "xz"
)
