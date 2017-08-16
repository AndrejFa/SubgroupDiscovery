#' @title
#' SubgroupDiscovery
#' @description
#' Subgroup discovery methods implemented in R environment up to this time. Target attribute need to be categorical and placed as
#' last column in \code{dataframe} with database.
#' @details
#' Subgroup dicovery (SD) is a supervised technique for descriptive and exploratory data mining and lies halfway between classification and descriptions.
#' It can be used for obtaining general relations in database, automatic hypotesis generation and data exploration.
#' Main goal is extraction of 'most interesting' subgroups of individuals according to property of interest or target attribute.
#' Most interesting subgroups are as large as possible, have unussual statistical distribution (characteristic) with respect to target attribute.
#' Subgroups does not have to be complete, partial relations are sufficient due to the fact that SD extracts most interesting subgroups (more false positives (FP) are allowed).
#' Search time is in exponential relation with dimensions of database (bigger database, longer searching time).
#'
#' There are currently 2 packages for subgroup discovery in R: \code{rsubgroup} and \code{SDEFSR}.
#' Both packages have unique methods for subgroups extraction.
#'
#' Package has integrated small database with medical data from years 2014-2015.
#'
#' Package is organize around central function \code{\link{subgroupDiscovery}}, from which subgroups
#' extraction is navigated according to method specified and a return value is \code{dataframe} with
#' subgroups and summary quality statistic. \code{rsubgroup} quality statistic is calculated from \code{\link{confusionMatrix}}
#' and only 2 columns are preserved from \code{\link[rsubgroup]{DiscoverSubgroups}} output: description (later renamed to subgroup) and
#' quality. \code{SDEFSR} quality statistic is untouched and only size and target value count are added to the
#' final \code{dataframe}.
#'
#' Package combine only analysis part for categorical target attribute of both packages and does not keep all functionality. \code{rsubgroup} package
#' analysis consist of \code{\link[rsubgroup]{as.target}}, \code{\link[rsubgroup]{DiscoverSubgroups}}, \code{\link[rsubgroup]{SDTaskConfig}} and \code{SDEFSR} consist of
#' \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}, \code{\link[SDEFSR]{SDIGA}}, \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}.
#'
#' @section SubgroupDiscovery functions:
#'\itemize{
#'\item \code{\link{subgroupDiscovery}} function for subgroup discovery analysis
#'\item \code{\link{confusionMatrix}} confusion matrix dimension 2 by 2
#'\item \code{\link{ruleDataset}} extract subset of each subgroup. Work with \code{lapply}
#'\item \code{\link{dynamicFilter}} filter by columns and values, add all ocurrences of selected target
#'value to subset, remove columns we filter on if subset is for further analysis
#'\item \code{\link{sortDF}} sort \code{dataframe} by column
#'\item \code{\link{sortL}} sort \code{list} by column. When target attribute value is not specified, SD is
#'done for all values and results are return as a \code{list}
#'\item \code{\link{latexTable}} \code{dataframe} to LaTex table
#'}
#' @note
#' I strongly recommend that input data is in \code{dataframe} object, because other formats,
#' like \code{datatable}, may lead to errors or some unpredictable results.
#'
#'\strong{Motivations}:
#'\itemize{
#'\item ability to do reproducible research
#'\item easier folder organization in reproducible research with \code{R} and \code{LaTeX} code
#'\item one import for all functionality for subgroup analysis
#'\item cleaner and more control coding experience
#'}
#' @docType package
#'
#' @author Andrej Fajfar <andrej.fajfar@student.um.si>
#' @references Atzmueller, M. (2015). Subgroup discovery. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 5(1), 35-49.
#' @references Atzmüller, M. (2006). Knowledge-intensive subgroup mining: techniques for automatic and interactive discovery (Vol. 307). IOS Press.
#' @references García, Á. M., Charte, F., González, P., Carmona, C. J., & del Jesus, M. J. Subgroup Discovery with Evolutionary Fuzzy Systems in R: the SDEFSR Package.
"_PACKAGE"
