#' @keywords internal
#' @noRd
`%p%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
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
# used in a warning mesage
too_many_resp <- function(l) {
  ids <- Map(function(nm, vals) {
    paste0(nm, ":\n", paste0(vals, "\n"), "\n")
  }, nm = names(l), vals = l)
  ids
}

is_item <- function(d) {
  grepl("^[EMS]\\d", names(d))
}