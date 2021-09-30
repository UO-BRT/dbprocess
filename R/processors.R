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
      stop('`content` should be one of `"ela"`, `"math"`, or `"science"`',
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
    setNames(swap_colnames("Submissions")) |>
    select(.data$submission_id:.data$exam_id)

  stu <- db_get("Students") |>
    setNames(swap_colnames("Students")) |>
    select(.data$student_id, .data$ssid,
           .data$district_id:.data$dist_stdnt_id,
           .data$gender:grade, .data$idea_elig_code1, .data$idea_elig_code2,
           .data$ethnic_cd, .data$lang_origin:.data$homeschool_flg,
           .data$transition_prgm:.data$alted_flg)

  exm <- db_get("Exams") |>
    setNames(swap_colnames("Exams")) |>
    select(-.data$form)

  ans <- db_get("Answers") |>
    setNames(swap_colnames("Answers")) |>
    select(.data$item_id, .data$answer_id:.data$question_id, .data$item_score)

  itms <- db_get("Items") |>
    setNames(swap_colnames("Items"))

  tasks <- db_get("Tasks") |>
    setNames(swap_colnames("Tasks")) |>
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
      select(.data$ssid:.data$year, .data$item_id_brt, .data$item_score) |>
      split(.data$title)
  )

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




#' Function to clean up the names of an important table
#'
#' @param table A string specifying the specific table from the Oregon Extended
#'   live database. Should be one of "Accomodations", "Answers", "Districts",
#'   "Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
#'   "Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
#'   "User", "UserStudents", or "UserStudents_old".
#' @return A character vector of the new cleaned up names
#' @noRd
#' @keywords internal

swap_colnames <- function(table) {
  switch(
    table,
    "Accomodations" = c(
      "submission_id", "accomodation"
    ),
    "Answers" = c(
      "answer_id", "task_id", "question_id", "item_score", "duration",
      "answer_date", "answer", "item_id", "positions", "is_practice",
      "is_correct", "is_final", "image_name"
    ),
    "Districts" = c(
      "district_id", "name"
    ),
    "Exams" = c(
      "exam_id", "title", "form", "year"
    ),
    "Items" = c(
      "item_id", "standard", "item_id_brt"
    ),
    "Preferences" = c(
      "user_id", "name", "value"
    ),
    "Schools" = c(
      "district_id", "school_id", "name"
    ),
    "Students" = c(
      "student_id", "user_id", "fname", "mname", "lname", "nickname",
      "gender", "birth_date", "grade", "ssid", "new_ssid", "end_date",
      "district_id", "school_id", "res_dist_id", "res_sch_id",
      "dist_stdnt_id", "idea_elig_code1", "idea_elig_code2", "data_source",
      "date_ineligible", "hisp_eth_flg", "amer_ind_ak_ntv_flg",
      "asian_race_flg", "black_race_flg", "white_race_flg",
      "pac_isl_race_flg", "lang_origin", "econ_dsvnt_flg", "title1_flg",
      "sped_flg", "sect504_flg", "migrant_ed_flg", "indian_ed_flg", "el_flg",
      "distance_learn_flg", "homeschool_flg", "tag_potential",
      "tag_intel_gifted", "tag_reading", "tag_math", "tag_creative",
      "tag_leadership", "tag_perform_arts", "transition_prgm", "alted_flg",
      "amerind_tribal_mem", "amerind_tribal_enroll", "ethnic_cd"
    ),
    "Students_old" = c(
      "student_id", "user_id", "fname", "mname", "lname", "nickname",
      "gender", "birth_date", "grade", "ssid", "new_ssid", "end_date",
      "district_id", "school_id", "res_dist_id", "res_sch_id",
      "dist_stdnt_id", "idea_elig_code1", "idea_elig_code2", "data_source",
      "date_ineligible", "hisp_eth_flg", "amer_ind_ak_ntv_flg",
      "asian_race_flg", "black_race_flg", "white_race_flg",
      "pac_isl_race_flg", "lang_origin", "econ_dsvnt_flg", "title1_flg",
      "sped_flg", "sect504_flg", "migrant_ed_flg", "indian_ed_flg", "el_flg",
      "distance_learn_flg", "homeschool_flg", "tag_potential",
      "tag_intel_gifted", "tag_reading", "tag_math", "tag_creative",
      "tag_leadership", "tag_perform_arts", "transition_prgm", "alted_flg",
      "amerind_tribal_mem", "amerind_tribal_enroll", "ethnic_cd"
    ),
    "Submissions" = c(
      "submission_id", "student_id", "exam_id", "form", "date_started",
      "date_finished", "date_discont", "completed", "score", "num_correct",
      "num_attempt", "comment"
    ),
    "SupplementalDistricts" = c(
      "user_id", "district_id"
    ),
    "SupplementalSchools" = c(
      "user_id", "school_id"
    ),
    "Tasks" = c(
      "task_id", "submission_id", "task_type", "date_started",
      "date_finished", "completed", "score", "num_correct"
    ),
    "User" = c(
      "user_id", "user_name", "password", "user_type", "creds_verified",
      "email", "join_date", "district_id", "school_id", "fname", "lname",
      "is_dtc", "description"
    ),
    "UserStudents" = c(
      "student_id", "user_id", "date_added", "comment"
    ),
    "UserStudents_old" = c(
      "student_id", "user_id", "date_added", "comment"
    )
  )
}
