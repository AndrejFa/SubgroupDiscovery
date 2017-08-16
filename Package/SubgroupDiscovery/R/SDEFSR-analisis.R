# @title
# Repeat evolutionary analysis
# @description
# Repeat evolutionary analysis with selected algoritm n times.
# @details
# Evolutionary algorithms have random initialization of initial population for begining subgroup
# discovery. Analysis has to be repeated n times to see which patterns have emerging in the database.
# Function has own seed generation mechanism, with sampling n times from an integer vector 1000:10000 without replacing.
# We fixate this numbers with argument 'seed' for repetition of results. Function does not allow splitting on
# training and test data. Function repeat analysis for one algorithm at the time.
# @param data_config list object with parameters for \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}
# @param method character. Name of algorithms: \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{SDIGA}},
# \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}
# @param ... arguments that method accepts
# @param n numeric. Integer inticating how many times analise the database
# @param seed numeric
# @return lists with subgroup summary statistic tables for each iteration of the method or list with lists of tables with subgroup
# summary statistic for all target variables
repeatEvolution <- function(data_config, n, method, seed, ...){

    #get seeds without replacement
    set.seed(seed = seed)
    seeds  <- sample(x = 1000:10000, size = n, replace = FALSE)

    #get space for save list of results for every method
    result_list <- list()

    for (i in 1:n) {

        index <- paste(method,"seed", seeds[i], sep = "_")

        result_list[[index]] <- evolutionAnalysis(data_config = data_config,
                                                  method      = method,
                                                  ...)

    }

    return(result_list)
}

############-----------------------------------------------------------------------##############################
#' @title
#' Evolution analysis
#' @description
#' Make subgroup discovery with evolutionary fuzzy logic algoritms implemented in SDEFSR package
#' @details
#' Function that do subgroup analysis with SDEFSR package. User can use all 4 implemented evolutionary algorithms.
#' Arguments for selected method can be passed in as list or each individually. User has to specify correct name as
#' described in documentation of each method. Function does not return SDEFSR object but rather subgroups with
#' summary statistic. Addition to SDEFSR summary statistic for extracted subgroups are: size of subgroup and target count.
#' Function does not privide ability to calculate summary statistic from confusion matrix.
#' @param data_config list object with parameters for \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}
#' @param method character. Name of algorithms: \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{SDIGA}},
#' \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}
#' @param ... arguments that method accepts
#' @import SDEFSR
#' @return data frame with subgroups summary statistic for one target variable or list of data frames with subgroup
#' summary statistic for all target variables
#' @seealso
#' \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}, \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{SDIGA}},
#' \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}
evolutionAnalysis <- function(data_config, method, ...) {
    #create configuration
    config <- list(...)
    #arguments for method passed in as list
    if (length(config) == 1 && is.list(config[[1]])) {
        #if SDEFSR object does not exist in configuration create and add one as training
        if (!any(c("training", "test") %in% names(config[[1]]))) {
            #create SDEFSR object
            d <- do.call(what = SDEFSR::SDEFSR_DatasetFromDataFrame,
                         args = data_config)
            #set configuration
            config <- c(list(training = d), config[[1]])

        }
    } else {
        if (!any(c("training", "test") %in% names(config))) {
            #create SDEFSR object
            d <- do.call(what = SDEFSR::SDEFSR_DatasetFromDataFrame,
                         args = data_config)
            #set configuration
            config <- c(list(training = d), config)
        }
    }

    results <- do.call(what = toupper(method),
                       args = config)

    results <- subgroupSummary(original_data = data_config$data,
                               output        = results,
                               evolution     = TRUE)
    return(results)
}

#' @title
#' Data partition
#' @description
#' Split data into \code{training} and \code{test} subset. Class distribution is preserved.
#' @details
#' Function split data and preserve target attribute value distribution.
#' After spliting on 2 parts, function create 2 \code{SDEFSR} object.
#' @param data_config \code{list} object with parameters for \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}
#' @param split numeric, in range [0, 1]. Share for \code{training} and rest for \code{test} subset
#' @importFrom caret createDataPartition
#' @return SDEFSR objects \code{list} with \code{training} set on 1st place and \code{test} on 2nd place
#' @seealso \code{\link[SDEFSR]{SDEFSR_DatasetFromDataFrame}}
dataPartition <- function(data_config, split, seed, class_vec) {

    set.seed(seed)
    #get indexes
    indexes <- caret::createDataPartition(y     = class_vec,
                                          times = 1,
                                          p     = split,
                                          list  = FALSE)

    #create training SDEFSR object
    new_data_config <- c(list(data = data_config$data[indexes, ]), data_config[names(data_config) != "data"])
    tr <- do.call(what = SDEFSR::SDEFSR_DatasetFromDataFrame,
                  args = new_data_config)
    #create testing SDEFSR object
    new_data_config <- c(list(data = data_config$data[-indexes, ]), data_config[names(data_config) != "data"])
    te <- do.call(what = SDEFSR::SDEFSR_DatasetFromDataFrame,
                  args = new_data_config)

    return(list(training = tr, test = te))
}

############-----------------------------------------------------------------------##############################
