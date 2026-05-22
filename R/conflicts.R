# Based on conflicts.R from the tidyverse package

# Conflicts between weird packages and other packages
#
# This function lists all the conflicts between packages in the weird collection
# and other packages that you have loaded.
#
# Some conflicts are deliberately ignored: \code{intersect}, \code{union},
# \code{setequal}, and \code{setdiff} from dplyr; and \code{intersect},
# \code{union}, \code{setdiff}, and \code{as.difftime} from lubridate.
# These functions make the base equivalents generic, so shouldn't negatively affect any
# existing code.
#
# @return A list object of class \code{weird_conflicts}.
# @export
# @examples
# weird_conflicts()
weird_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- stats::setNames(envs, envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- Filter(\(x) length(x) > 1, objs)

  tidy_names <- paste0("package:", weird_packages())
  conflicts <- Filter(\(x) any(x %in% tidy_names), conflicts)

  conflict_funs <- mapply(
    confirm_conflict,
    conflicts,
    names(conflicts),
    SIMPLIFY = FALSE
  )
  conflict_funs <- Filter(Negate(is.null), conflict_funs)

  structure(conflict_funs, class = "weird_conflicts")
}

weird_conflict_message <- function(x) {
  if (length(x) == 0) {
    return("")
  }

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = "weird_conflicts"
  )

  pkgs <- lapply(x, \(pkg) gsub("^package:", "", pkg))
  others <- lapply(pkgs, `[`, -1)
  other_calls <- mapply(
    \(other, nm) paste0(crayon::blue(other), "::", nm, "()", collapse = ", "),
    others,
    names(others)
  )

  winner <- vapply(pkgs, `[[`, character(1), 1)
  funs <- format(paste0(
    crayon::blue(winner),
    "::",
    crayon::green(paste0(names(x), "()"))
  ))
  bullets <- paste0(
    crayon::red(cli::symbol$cross),
    " ",
    funs,
    " masks ",
    other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.weird_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(weird_conflict_message(x))
}

confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- Filter(is.function, lapply(packages, \(pos) get(name, pos = pos)))

  if (length(objs) <= 1) {
    return()
  }

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1) {
    return()
  }

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  if (identical(env, "package:lubridate")) {
    x <- setdiff(x, c("intersect", "setdiff", "union", "as.difftime"))
  }
  x
}
