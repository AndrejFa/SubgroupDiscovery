#' @title
#' Subgroup discovery
#' @description
#' Function combines subgroup discovery algorithms so far implemented in R
#' @details
#' There are currently 2 packages for subgroup discovery in R: \code{rsubgroup} and \code{SDEFSR}.
#' Both packages have unique methods for analysis. This function is a workhorse of this package and
#' present bridge between both packages. Since both packages need some data preparation before analysis, package unify this process
#' with argument \code{data_config} for data configuration. Later is a list of elements, \code{data} element is desame for both packages and other
#' or others are package specific. \code{rsubgroup} part needs as 2nd argument, a character,
#' information about target attribute: its name and value, to create target object with \code{\link[rsubgroup]{as.target}}.
#' \code{SDEFSR} package needs one additional element for \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}, a character \code{relation}, other are optional.
#' The minimum arhitecture of argument \code{data_config} is: \code{data_config = list(data = data.frame, target = character)} or
#' \code{data_config = list(data = data.frame, relation = character)}.
#' \itemize{
#' \item \code{rsubgroup}: present interface to VIKAMINE platform and is Java dependent. Search method
#' implemented are: \code{beam}, \code{bsd}, \code{sdmap} and \code{sdmap-dis}. Function uses \code{\link[rsubgroup]{SDTaskConfig}} and \code{\link[rsubgroup]{DiscoverSubgroups}}
#' for subgroups extraction. Quality statistic of extracted subgroups are calculated from
#' \code{\link{confusionMatrix}}.
#' \item \code{SDEFSR}: non dependent package with evolutionary fuzzy logic algorithms for subgroup discovery.
#' Search method implemented are: \code{SDIGA}, \code{MESDIF}, \code{NMEEF_SD} and \code{FUGEPSD.} Package calculates own quality static for extracted subgroups, addition to it is subgroup size and
#' counter of target value.
#' }
#' Packages can be distinguished by methods they contains. Argument \code{method} is simply a character with name of method.
#' We suggest use of lowercase letters for this matter.
#'
#' After method selection user has to provide arguments for selected method. This can be done in two ways: as list object
#' in form \code{list(arg_name = arg_val)} or passed in individualy as \code{arg_name = arg_val}. We recommend later usage
#' because of easier code understanding. The output of analysis is \code{dataframe} object with subgroups and
#' summary statistic.
#'
#' If target attribute value is not specified analysis for all value in target attribute is performed. Target attribute or class
#' variable has to be the last column of \code{dataframe} and has to be categorical. If target attribute is not binary
#' \code{\link{confusionMatrix}} calculates quality measures one versus other for each target attribute value if its not
#' specified by user.
#'
#' Function test for correct method and does not check arguments to that method, because underlying functions have
#' nice error and warnings presentation.
#' @param data_config \code{list} for data configuration.
#' \itemize{
#' \item \code{rsubgroup}: data configuration must contain \code{data} and character vector with
#' target attribute name and corresponding target attribute value. See also \code{\link[rsubgroup]{SDTaskConfig}} and
#' \code{\link[rsubgroup]{as.target}}.
#' \item \code{SDEFSR}: data configuration must contain \code{data} and arguments for \code{SDEFSR} object creation
#' with \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}.
#' }
#' @param method character, name of algorithm
#' @param ... selected method arguments. It can be passed in each individually or packed in \code{list} object
#' @return
#' \code{dataframe} with subgroups summary statistic for one target value analysis
#' or \code{list} of \code{dataframes} for all target values analysis
#' @seealso
#' \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}, \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{SDIGA}},
#' \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}, \code{\link[rsubgroup]{DiscoverSubgroups}}, \code{\link[rsubgroup]{SDTaskConfig}}
#' @export
subgroupDiscovery <- function(data_config, method, ...) {
    if (toupper(method) %in% c("MESDIF", "NMEEF_SD", "SDIGA", "FUGEPSD")) {
        result <- evolutionAnalysis(data_config = data_config,
                                    method      = method,
                                    ...)
    } else if (method %in% c("beam", "bsd", "sdmap", "sdmap-dis")) {
        result <- rsubgroupAnalysis(data_config = data_config,
                                    method      = method,
                                    ...)
    } else {
        warning("Argument 'method' has values: MESDIF, NMEEF_SD, SDIGA and FUGEPSD for evolutinary part and beam, bsd, sdmap, sdmap-dis for rsugroup part")
        stop("Incorrect 'method' argument")
    }

    return(result)
}
