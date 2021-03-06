% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processors.R
\name{get_items}
\alias{get_items}
\title{Function to pull item level data with student demographics from the live
ORExt database}
\usage{
get_items(grade = NULL, content = NULL, demographics = TRUE, ...)
}
\arguments{
\item{grade}{Optional argument to return only data from a specific grade.
Default is \code{NULL}, in which case all grades are returned in a list,
where each element of the list is a data frame for the specific test form.}

\item{content}{Optional argument to return only data from a specific content
area. Default is \code{NULL}, in which case all content areas are returned
in a list, where each element of the list is a data frame for the specific
test form. Should be one of \code{"ELA"}, \code{"Math"}, or
\code{"Science"}, although case does not matter (e.g., \code{"ela"}
will also work).}

\item{demographics}{Logical, defaults to \code{TRUE}. Should student
demographics be returned with the item-level data.}

\item{...}{Additional arguments passed to \code{\link[orextdb:db_get]{orextdb::db_get()}}. Primarily
used to specify the database (\code{db}).}
}
\value{
If both \code{grade} and \code{content} are both \code{NULL}, a list
of all grade/content areas. If one or the other is supplied, a list with
only the specific grade/content area. If both \code{grade} and
\code{content} are supplied, a single data frame for that grade/content
area is returned.
}
\description{
Function to pull item level data with student demographics from the live
ORExt database
}
