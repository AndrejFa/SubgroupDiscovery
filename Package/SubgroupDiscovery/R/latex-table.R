#' @title
#' LaTeX table
#' @description
#' Get LaTeX code from \code{dataframe}
#' @details
#' Function convert R \code{dataframe} to LaTeX code. Arguments must be inserted individualy
#' and not in list format as user can do with analysis function. Some arguments combination crate
#' an error messages and  description of arguments for \code{\link[xtable]{xtable}} and
#' \code{\link[xtable]{print.xtable}} must be read. Charater column with subgroup conditions can be
#' formated with this 2 LaTeX command (usage without ' ' ):
#' \itemize{
#'   \item \code{tabularx} environment: Y letter can be changed to different capital letter. To use this
#'   enviroment one have to import, at the beginig of .Rnw file, a \code{'\usepackage{tabularx}'} and then command
#'   ,at the begining of the file or right before code chunks, is specified for subgroup column.
#'
#'   \code{'\newcolumntype{Y}{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}X}'}
#'
#'   \item \code{longtable} environment: R letter can be changed to different capital letter. \code{'\usepackage{longtable}'}
#'   import needs to be done before usage. \code{'p{0.42\textwidth}'} can be changed based on desired
#'   witdh of the subgroup column. Decimal number mean proportion of textwith used for selected column.
#'   It can be placed right before code chunks which use longtable environment for LaTeX output
#'   or at the begining if one formating of longtable enviroment will be used for entire document.
#'
#'   \code{'\newcolumntype{R}{>{\raggedright\let\newline\\\arraybackslash}p{0.42\textwidth}}'}
#' }
#' Leters from newcolumntype command are then used in align argument i.e. \code{align = 'lRcr'}. This
#' mean that we have 3 columns, one is reserved for \code{include.rownames} argument (first letter in align string, i.e. l - left),
#' and are aligned R - raggedright means left, c - center and r right.
#' @param df \code{dataframe} with analysis results
#' @param ... arguments for \code{\link[xtable]{xtable}} and \code{\link[xtable]{print.xtable}}
#' @import xtable
#' @return LaTeX code
#' @seealso  \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}
#' @export
latexTable <- function(df, ...) {

    arguments <- list(...)
    #create xtable object based on passed arguments
    if (any(c("caption", "label", "align", "digits", "display", "auto") %in% names(arguments))) {
        #extract xtable argument in list format
        xtable_args <- arguments[which(names(arguments) %in% c("caption", "label", "align", "digits", "display", "auto"))]
        xt <- do.call(what = xtable,
                      args = c(list(x = df), xtable_args))
        #remove xtable arguments and print table
        xt_print_args <- arguments[names(arguments) != names(xtable_args)]

        do.call(what = print.xtable,
                args = c(list(x = xt), xt_print_args))

    } else {
        xt <- xtable(x = df)
        do.call(what = print.xtable,
                args = c(list(x = xt), list(...)))
    }

}
