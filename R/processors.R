#' Function to pull item level data with student demographics from the live
#' ORExt database
#'
#' @param grade Optional argument to return only data from a specific grade.
#'   Default is \code{NULL}, in which case all grades are returned in a list,
#'   where each element of the list is a data frame for the specific test form.
#' @param content Optional argument to return only data from a specific content
#'   area. Default is \code{NULL}, in which case all content areas are returned
#'   in a list, where each element of the list is a data frame for the specific
#'   test form. Should be one of \code{"ELA"}, \code{"Math"}, or
#'   \code{"Science"}, although case does not matter (e.g., \code{"ela"}
#'   will also work).
#' @param demographics Logical, defaults to \code{TRUE}. Should student
#' demographics be returned with the item-level data.
#' @return If both \code{grade} and \code{content} are both \code{NULL}, a list
#'   of all grade/content areas. If one or the other is supplied, a list with
#'   only the specific grade/content area. If both \code{grade} and
#'   \code{content} are supplied, a single data frame for that grade/content
#'   area is returned.
#' @export

get_items <- function(grade = NULL, content = NULL, demographics = TRUE) {

  if (!is.null(content)) {

    if (!tolower(content) %in% c("ela", "math", "science")) {
      stop('`content` should be one of `"ELA"`, `"Math"`, or `"Science"`',
           call. = FALSE)
    }

    content <- switch(tolower(content),
                      "ela" = "ELA",
                      "math" = "Math",
                      "science" = "Science")
  }

  if (is.null(grade) & is.null(content)) {
    form_select <- "_"
  } else {
    form_select <- paste(content, grade, sep = "_G")
  }

  submissions <- db_get("Submissions") |>
    select(.data$submission_id:.data$exam_id)

  stu <- db_get("Students") |>
    select(.data$student_id, .data$ssid,
           .data$district_id:.data$dist_stdnt_id,
           .data$gender:grade, .data$idea_elig_code1, .data$idea_elig_code2,
           .data$ethnic_cd, .data$lang_origin:.data$homeschool_flg,
           .data$transition_prgm:.data$alted_flg)

  exm <- db_get("Exams") |>
    select(-.data$form)

  ans <- db_get("Answers") |>
    select(.data$item_id, .data$answer_id:.data$question_id, .data$item_score)

  itms <- db_get("Items")

  tasks <- db_get("Tasks") |>
    select(.data$task_id, .data$submission_id)

  suppressMessages(
    items <- left_join(submissions, stu) |>
      left_join(exm) |>
      left_join(tasks) |>
      left_join(ans) |>
      left_join(itms) |>
      arrange(.data$ssid, .data$title, .data$question_id) |>
      filter(.data$title != "ORora" &
               .data$title != "ALT-SEED" &
               !is.na(.data$item_id_brt)) |>
      select(.data$ssid:.data$year, .data$item_id_brt, .data$item_score)
  )

  items <- split(items, items$title)

  by_form <- lapply(items, function(x) {
    pivot_wider(x,
                names_from = "item_id_brt",
                values_from = "item_score")
  })

  if (isFALSE(demographics)) {
    by_form <- lapply(by_form, function(x) select(x, -.data$ssid:-.data$year))
  }

  out <- by_form[grepl(form_select, names(by_form))]

  if (length(out) == 1) {
    out <- out[[1]]
  }

  out
}
