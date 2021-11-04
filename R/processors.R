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
#' @param ... Additional arguments passed to [orextdb::db_get()]. Primarily 
#'   used to specify the database (\code{db}).
#' @return If both \code{grade} and \code{content} are both \code{NULL}, a list
#'   of all grade/content areas. If one or the other is supplied, a list with
#'   only the specific grade/content area. If both \code{grade} and
#'   \code{content} are supplied, a single data frame for that grade/content
#'   area is returned.
#' @export

get_items <- function(grade = NULL, content = NULL, demographics = TRUE, ...) {

  if (!is.null(content)) {
    content <- check_content(content)
  }

  if (is.null(grade) & is.null(content)) {
    form_select <- "_"
  } else {
    form_select <- paste(content, grade, sep = "_G")
  }

  submissions <- db_get("Submissions", ...) |>
    select(.data$submission_id:.data$exam_id)

  stu <- db_get("Students", ...) |>
    select(.data$student_id, .data$ssid,
           .data$district_id:.data$dist_stdnt_id,
           .data$gender:grade, .data$idea_elig_code1, .data$idea_elig_code2,
           .data$ethnic_cd, .data$lang_origin:.data$homeschool_flg,
           .data$transition_prgm:.data$alted_flg)

  exm <- db_get("Exams", ...) |>
    select(-.data$form)

  ans <- db_get("Answers", ...) |>
    select(.data$item_id, .data$answer_id:.data$question_id, .data$item_score)

  itms <- db_get("Items", ...)
  itms$item_id <- as.numeric(itms$item_id)

  tasks <- db_get("Tasks", ...) |>
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

#' Get the JSON data for any (or all) tests
#' @inheritParams get_items
#' @param name The name of the test to download (e.g., Science_G5, ELA_G11). If
#'   used, subsequent arguments to \code{grade} and \code{content} are ignored.
#' @return If \code{name} or \code{grade} and \code{content} are supplied, the
#'   json data for just that test is returned. If all are \code{NULL}, then
#'   a list with the json data for all tests are returned.
#' @keywords internal
#' @noRd
get_test_json <- function(name = NULL, grade = NULL, content = NULL) {
  base_link <- "https://orext.brtprojects.org/app/test/manifests/"

  all_null <- is.null(name) & is.null(grade) & is.null(content)

  if (!is.null(name) & (!is.null(grade) | !is.null(content))) {
    warning("Using `name`. Ignoring `grade` and/or `content` arguments")
  }

  if (!all_null & is.null(name)) {
    if (is.null(grade) | is.null(content)) {
      stop("If `name` is not supplied, both `grade` and `content` should " %p%
           "be supplied, or all should be `NULL`",
           call. = FALSE)
    }
    content <- check_content(content)
    grade <- check_grade(grade)

    name <- paste0(content, grade)
  }

  if (!is.null(name)) {
    link <- paste0(base_link, name, ".json")
    return(
      fromJSON(link)
    )
  }
  full_names <- create_all_names()
  full_links <- paste0(base_link, full_names, ".json")

  out <- lapply(full_links, fromJSON)
  names(out) <- full_names
  out
}

#' Pull the item IDS from the json data
#' @param json A single JSON file from \code{get_test_json()} 
#'   (internal dbprocess function)
#' @keywords internal
#' @noRd
pull_item_ids <- function(json) {
  questions <- json$tasks$questions[[1]]
  questions$brtItemID[questions$brtItemID != ""]
}

#' Create a patterned data frame for all possible raw scores
#' @param item_names The column names (items) from which to generate the
#'   data frame. These become the column names of the patterned data frame
#' @keywords internal
#' @noRd
create_pattern_frame <- function(item_names) {
  n <- length(item_names)

  full_zeros <- matrix(rep(0, n), nrow = 1)
  full_ones <- matrix(rep(1, n), nrow = 1)

  ones <- lapply(seq_len(n - 1), function(x) rep(1, x))
  zeros <- lapply(rev(seq_len(n - 1)), function(x) rep(0, x))

  m <- Map(function(a, b) matrix(c(a, b), nrow = 1), a = ones, b = zeros)
  m <- c(list(full_zeros), m, list(full_ones))

  d <- as.data.frame(Reduce(rbind, m))
  names(d) <- item_names
  d
}

#' Create patterned synthetic data for any (or all) tests related to all
#'   possible raw scores
#' @param name The name of the test to download (e.g., Science_G5, ELA_G11). If
#'   used, subsequent arguments to \code{grade} and \code{content} are ignored.
#' @param items Optional set of items to be passed to subset the dataframe,
#'   e.g., to only anchor items. Should be passed as a character vector or,
#'   if returning all tests, a list of character vectors (one for each test).
#' @return Similar to \code{get_test_json}, if \code{name} or both
#'           \code{grade} and \code{content} are supplied, the patterned data
#'           for just that test is returned. Otherwise, patterned data for
#'           all tests is returned.The patterned data is synthetic and created
#'           such that all possible raw scores are generated. This function
#'           is primarily used to create the raw to scale score mapping.
#' @export
get_pattern_data <- function(name = NULL, grade = NULL, content = NULL,
                             items = NULL) {
  json <- get_test_json(name, grade, content)
  if (names(json)[1] == "tasks") {
    item_ids <- pull_item_ids(json)
    item_ids <- item_ids[item_ids %in% items$item_id_brt]
    return(create_pattern_frame(item_ids))
  }
  item_ids <- lapply(json, pull_item_ids)
  item_ids <- lapply(item_ids, function(x) x[x %in% items$item_id_brt])
  lapply(item_ids, create_pattern_frame)
}

