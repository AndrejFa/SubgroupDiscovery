#' @title
#' Sort dataframe
#' @description
#' Sort \code{dataframe} by selected column in descreasing order
#' @param df \code{dataframe} to sort
#' @param by character, name of column
#' @param decrease logical, default TRUE
#' @return
#' Sorted \code{dataframe}
#' @export
sortDF <- function(df, by, decrease = TRUE) {
    return(df[order(df[, by], decreasing = decrease), ])
}
################---------------------------------------------###############
#' @title
#' Sort list
#' @description
#' Sort \code{list} by selected column in descreasing order
#' @param li \code{list} object with data frame as each element
#' @param by character, name of column
#' @param decrease logical, default TRUE
#' @return
#' Sorted \code{list} of \code{dataframes}
#' @export
sortL <- function(li, by, decrease = TRUE) {
    return(lapply(li, sortDF, by, decrease))
}
################---------------------------------------------###############
#' @title
#' Sort list of lists
#' @description
#' Sort list by selected column in descreasing order
#' @param ll list of lists object with data frame as each element of innermost list
#' @param by character, name of column
#' @param decrease logical, default TRUE
#' @return
#' Sorted list of lists with data frames
sortLL <- function(ll, by, decrease = TRUE) {
    n <- names(ll)
    for (li in n) {
        ll[[li]] <- sortL(ll[[li]], by, decrease)
    }

    return(ll)
}
