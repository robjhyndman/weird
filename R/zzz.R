# Based on zzz.R from the tidyverse package

.onAttach <- function(...) {
  attached <- weird_attach()
  packageStartupMessage(weird_attach_message(attached))

  if (is_attached("conflicted")) {
    return(invisible())
  }

  conflicts <- weird_conflicts()
  packageStartupMessage(weird_conflict_message(conflicts))
}


is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
