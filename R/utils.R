#' internal paste function
#' Note: created by DA and not currently used
#' Infix function to paste long strings together a little easier
#'
#' @keywords internal
#' @param lhs = character vector to paste on left
#' @param rhs = character vector to paste on right
`%p%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
}

# CHRIS FIX

# Talk to Evan for new functionality in the API that will list
# all the dbs, then just take the most recent one.

#' Used in case the db is not passed to a function
#' Determines the current db based on the system date
#' Creates a string with the name of the db
#' @keywords internal
current_db <- function() {
  current_month <- as.numeric(format(Sys.Date(),"%m"))
  current_year <- as.numeric(format(Sys.Date(),"%y"))

  if (current_month < 9) {
    paste0("ORExt", paste0(current_year - 1, current_year))
  }

  else {
    paste0("ORExt", paste0(current_year, current_year + 1))
  }

}


check_content <- function(content) {
  if (!tolower(content) %in% c("ela", "math", "science")) {
    stop('`content` should be one of `"ELA"`, `"Math"`, or `"Science"` ' %p%
         'in lower or upper case.',
      call. = FALSE
    )
  }

  content <- switch(tolower(content),
    "ela" = "ELA",
    "math" = "Math",
    "science" = "Science"
  )
}

check_grade <- function(grade) {
  grade_int <- gsub("^[Gg]", "", grade)
  if (!grade_int %in% c(3:8, 11)) {
    stop('`grade` should be an integer from 3-8 or 11`', call. = FALSE)
  }
  paste0("_G", grade_int)
}

create_all_names <- function() {
  ela <- paste0("ELA_G", c(3:8, 11))
  math <- paste0("Math_G", c(3:8, 11))
  sci <- paste0("Science_G", c(5, 8, 11))
  c(ela, math, sci)
}

# collate the IDs of the students who have too many responses
# used in a warning message
too_many_resp <-
  function(l) {
    ids <- Map(function(nm, vals) {
      paste0(nm, ":\n", paste0(vals, "\n"), "\n")
      }, nm = names(l), vals = l)
    ids
    }


is_item <- function(d) {
  grepl("^[EMS]\\d", names(d))
}


#' Checks if R version >= 4.1 to ensure base pipe works
#' @keywords internal
#' @noRd
check_base_pipe <- function(){

  simplified_R_version <-
    as.numeric(
      paste0(
        R.version$major,
        '.',
        strsplit(R.version$minor, '\\.')[[1]][1]
        )
      )
  simplified_R_version >= 4.1
}
