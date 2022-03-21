#' @keywords internal
#' @noRd
#' @param duplicate_submissions = data frame containing duplicate submissions


# verify this structure of determining which duplicate is correct:
# arrange by date first, then if date is the same, take max score
# I did what DA originally had

handle_duplicates <-
  function(submissions, duplicate_submissions){

  dup_keep <-
    duplicate_submissions |>
    group_by(.data$student_id, .data$exam_id) |>
    arrange(desc(.data$date_finished), desc(.data$score)) |>
    slice(1)

  suppressMessages(
    dup_rm <- submissions |>
      add_count(.data$student_id, .data$exam_id) |>
      filter(.data$n > 1) |>
      anti_join(dup_keep)
  )
  suppressMessages(
    submissions <- anti_join(submissions, dup_rm)
  )
  return(submissions)
  }

# internal function to format the student table differentially based on the year
#' @keywords internal
#' @noRd
#' @param student_table = output of \code{db_get("Students", ...)}
#' @param year = year of student table to reference table format; options c('1718', '1819', 'other')

format_student_table <-
  function(student_table, year){


  if (year == "1718") {
    student_table <-
      student_table |>
      select(
        .data$student_id,
        .data$ssid,
        .data$district_id,
        .data$school_id,
        .data$gender,
        .data$birth_date,
        .data$grade,
        .data$idea_elig_code1,
        .data$idea_elig_code2
      )
  } else if (year == "1819") {
    student_table <-
      student_table |>
      select(
        .data$student_id,
        .data$ssid,
        .data$district_id,
        .data$school_id,
        .data$res_dist_id,
        .data$res_sch_id,
        .data$dist_stdnt_id,
        .data$gender,
        .data$birth_date,
        .data$grade,
        .data$idea_elig_code1,
        .data$idea_elig_code2
      )
  } else {
    student_table <-
      student_table |>
      select(
        .data$student_id,
        .data$ssid,
        .data$district_id,
        .data$school_id,
        .data$res_dist_id,
        .data$res_sch_id,
        .data$dist_stdnt_id,
        .data$gender,
        .data$birth_date,
        .data$grade,
        .data$idea_elig_code1,
        .data$idea_elig_code2,
        .data$ethnic_cd,
        .data$lang_origin,
        .data$econ_dsvnt_flg,
        .data$title1_flg,
        .data$sped_flg,
        .data$sect504_flg,
        .data$migrant_ed_flg,
        .data$indian_ed_flg,
        .data$el_flg,
        .data$distance_learn_flg,
        .data$homeschool_flg,
        .data$transition_prgm,
        .data$alted_flg
      )
  }
  return(student_table)
}

# internal function to format the items table from other tables
#' @keywords internal
#' @noRd

get_items_table <-
  function(
    submissions_table,
    student_table,
    exam_table,
    tasks_table,
    answer_table,
    items_table
    ){

    items <-
      left_join(submissions_table, student_table) |>
      left_join(exam_table) |>
      left_join(tasks_table) |>
      left_join(answer_table) |>
      left_join(items_table) |>
      filter(
        # life skills assessment
        .data$title != "ORora" &
          # survey on feelings of school belonging etc.
          .data$title != "ALT-SEED" &
          !is.na(.data$item_id_brt)
      ) |>
      distinct(
        .data$submission_id,
        .data$item_id_brt,
        .data$date_finished,
        .data$score,
        .keep_all = TRUE
      ) |>
      arrange(.data$ssid, .data$task_type, .data$question_id) |>
      select(
        .data$ssid,
        .data$district_id,
        .data$school_id,
        .data$res_dist_id,
        .data$res_sch_id,
        .data$dist_stdnt_id,
        .data$gender,
        .data$birth_date,
        .data$grade,
        .data$idea_elig_code1,
        .data$idea_elig_code2,
        .data$task_type,
        .data$year,
        .data$question_id,
        .data$item_id_brt,
        .data$item_score
      )

    return(items)

  }

# internal function to check the number of responses for each student.
# this will give a warning if there is anyone with more than 48 responses
# this threshold was established by DA, but should be checked
# perhaps different by year

#' @keywords internal
#' @noRd
#' @param items_list = list of all item responses for all students
 check_num_responses <-
   function(items_list){

     counts <- lapply(items_list, function(x) table(x$ssid))
     out_of_range <- lapply(counts, function(x) names(x[x > 48]))
     non_empty <- vapply(out_of_range, function(x) {
       length(x) > 0
     }, FUN.VALUE = logical(1))

     if (any(non_empty)) {
       out <- lapply(out_of_range[non_empty], function(x) {
         paste0("ssid: ", x, collapse = ", ")
       })

       warning(
         "Students with more than 48 item responses detected.\n\n",
         too_many_resp(out),
         call. = FALSE
       )
     }

   }

 #' @description internal function to reorder the items list
 #' @keywords internal
 #' @noRd
 #' @param items_list = list of all item responses for all students
 return_original_order <-
   function(items_list){

     original_order <-
       lapply(items_list, function(x) {
         items <-
           x[
             x$question_id %in% seq(1, max(x$question_id)),
             c("question_id", "item_id_brt")
           ]
         items <- items[order(items$question_id), ]
         unique(items$item_id_brt)
       })
     return(original_order)
   }

#' @description internal function to pivot each
#' @keywords internal
#' @noRd
#' @param items_list = list of all item responses for forms (i.e., grades & subjects)
#' @param original_order = ordered based on \code{return_original_order()}
#' @param demographics = logical taken from earlier call in \code{get_items()}
#' @param form_select = string created by \code{get_items()}

 sep_and_pivot_by_form <-
   function(
    items_list,
    original_order,
    demographics = demographics,
    form_select = form_select
    ){
     by_form <-
       lapply(items_list, function(x) {
         pivot_wider(
           x,
           names_from = "item_id_brt",
           values_from = "item_score"
         )
       })

     by_form <- Map(function(form, nms) {
       dems <- names(form)[!is_item(form)]
       form[, c(dems, nms)]
     },
     form = by_form,
     nms = original_order
     )

     if (isFALSE(demographics)) {
       by_form <- lapply(by_form, function(x) x[is_item(x), ])
     }

     out <- by_form[grepl(form_select, names(by_form))]

   }


 #' internal function to remove question ids from all forms (i.e., grades & subjects)
 #' @keywords internal
 #' @noRd
 #' @param items_list = list of all item responses for all students
 remove_question_ids <-
   function(items_list){
     lapply(items_list, function(x) x[-grep("question_id", names(x))])
     }

#' @description internal function to that wraps \code{db_get}
#' @keywords internal
#' @noRd
#' @param remove_duplicates = logical indicating if we want duplicates removed. should always be TRUE
 db_get_submissions <-
   function(
    remove_duplicates = TRUE,
    ...){
   submissions <-
     db_get("Submissions", ...) |>
     select(
       .data$submission_id,
       .data$student_id,
       .data$exam_id,
       .data$date_finished,
       .data$score
     ) |>
     distinct(
       .data$student_id,
       .data$exam_id,
       .data$date_finished,
       .keep_all = TRUE
     )

   if (remove_duplicates){
     duplicate_submissions <-
       submissions |>
       add_count(.data$student_id, .data$exam_id) |>
       filter(.data$n > 1)


     if (nrow(duplicate_submissions) > 0) {
       submissions <-
         handle_duplicates(
           submissions = submissions,
           duplicate_submissions = duplicate_submissions
       )
     }
   }
   return(submissions)
 }
