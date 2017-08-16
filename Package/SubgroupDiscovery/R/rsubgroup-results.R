# @title
# Multiple rsubgroup analysis
# @description
# Get combinations, of subgroup discovery analysis, of methods and quality functions from rsubgroup package.
# @details
# Do all analysis at one step with combination of methods and quality functions, other arguments
# needs to be desame.
# @param data_config \code{list} with data frame of data and character vector with target attribute name and target value, latter can be omitted
# @param method character vector with names of methods
# @param qf character vector with names of quality functions
# @param @param ... arguments for \code{\link[rsubgroup]{DiscoverSubgroups}} configuration with \code{\link[rsubgroup]{SDTaskConfig}}.
# Method and quality function are added automatically so must be left out. It can be passed in each individually or packed in list object
# @return \code{list} with subgroup summary statistic tables for each combination of quality function
# and method.
# @seealso  \code{\link[rsubgroup]{CreateSDTask, SDTaskConfig, DiscoverSubgroups, DiscoverSubgroupsByTask}}
multipleRsubgroup <- function(data_config, method, qf, ...) {

    #list of quality function and method combination
    qf_method_pairs <- str_vector_combination(vec1 = method, vec2 = qf)
    #create space for results
    list_results <- list()

    for (i in 1:length(qf_method_pairs)) {

        #extract method and quality function (double [[]] because we have list object)
        method <- qf_method_pairs[[i]][1]
        qf     <- qf_method_pairs[[i]][2]

        #list with discover subgroup configuration
        if (length(list(...)) == 1 && is.list(list(...)[[1]])) {
            new_config <- c(list(qf = qf), ...)
        } else {
            new_config <- c(list(qf = qf), list(...))
        }

        #create index name and append rules to list
        index_name <- paste(method, qf, sep = "_")

        list_results[[index_name]] <- rsubgroupAnalysis(data_config = data_config,
                                                        method      = method,
                                                        new_config)
    }

    return(list_results)
}
########################-------------------------------------------------------------------------#################
#' @title
#' Rsubgroup analysis
#' @description
#' Do subgroup discovery with rsubgroup package
#' @details
#' Function that extracts subgroups from data with interface to Vikamine platform for subgroup discovery.
#' Do basic analysis as we do it with rsubgroup package. Function use \code{\link[rsubgroup]{DiscoverSubgroups}}
#' functions from rsubgroup package for subgroup discovery analysis.
#' Argument passed insted of ... for \code{\link[rsubgroup]{DiscoverSubgroups}} configuration are prof checked for their names
#' and not for their types. If some Java error occur is due to incorrect argument type in configuration
#' see \code{\link[rsubgroup]{SDTaskConfig}} for details arguments type and description of configuration.
#' Function calculates analysis for all target attribute values if desired target value is not specified.
#' @param data_config list with 2 elements: data frame with database and character vector of target attribute name and desired target value
#' @param method character method name
#' @param ... arguments for \code{\link[rsubgroup]{DiscoverSubgroups}} configuration \code{\link[rsubgroup]{SDTaskConfig}}.
#' Method is added automatically so must be left out. It can be passed in each individually or packed in list object
#' @import rsubgroup
#' @return data frame with subgroups and summary statistic or list of data frames with subgroups and summary
#' statistic for each target attribute value
#' @seealso
#' \code{\link[rsubgroup]{SDTaskConfig, DiscoverSubgroups}}
rsubgroupAnalysis <- function(data_config, method, ...) {

    element_type <- sapply(X = data_config, FUN = class)
    data_index <- which(element_type == "data.frame")
    target_index <- which(element_type == "character")

    #extract target and database
    target_info <- data_config[[target_index]]
    database <- data_config[[data_index]]
    attr_names <- names(database)

    if (!is.list(data_config))
        stop("Incorect object type of 'data_config' argument. Has to be list object")
    else if (!all(c("data.frame", "character") %in% element_type))
        stop("Elements type of argument 'data_config' are not valid. Has to be 'data.frame' and 'character' vector")
    else if (!length(target_info) <= 2)
        stop("'character' element of argument 'data_config' has to be length of 1 or 2, with target attribute name only or plus corresponding target attribute value")

    if ((!method %in% c("beam", "bsd", "sdmap", "sdmap-dis")) && length(method) != 1) {
        warning("argument 'method' is character with values: 'beam', 'bsd', 'sdmap', 'sdmap-dis'")
        stop("Incorect object type of 'method' argument or incorect 'method' selected or method does not have 1 value.")
    }

    #set target value and attribute name for as.target
    if (length(target_info) == 2) {
        #user defined target att value
        att <- target_info[which(target_info %in% attr_names)]
        value <- target_info[which(!target_info %in% attr_names)]
    } else if (length(target_info) == 1 && target_info %in% attr_names) {
        #user did not define target att value
        att <- target_info
        value <- unique.default(database[ ,target_info])
    } else {
        stop(paste("Selected target attribute name not in database:", target_info, sep = " "))
    }

    #combine two lists: first argument to new function is Class since this is default
    #in rsubgroup package we do desame here so user does not have to know this
    if (length(list(...)) == 1 && is.list(list(...)[[1]])) {
        config <- c(list(Class = "SDTaskConfig", method = method), ...)
    } else {
        config <- c(list(Class = "SDTaskConfig", method = method), list(...))
    }

    if (!length(config) <= 11)
        stop("Configuration has to many arguments. Method is added automatically")
    else if (!all(names(config) %in% c("Class","method","qf","k","minqual","minsize","maxlen","nodefaults","relfilter","postfilter","attributes"))) {
        warning("Subgroup discovery configuration has elements: 'method','qf', 'k', 'minqual', 'minsize',
                'maxlen', 'nodefaults', 'relfilter', 'postfilter', 'attributes'")
        stop("Invalid arguments for subgroup discovery configuration")
    }

    result <- list()

    for (val in value) {
        #create target
        t_var <- as.target(attribute = att,
                           value     = val)

        subgroups <- DiscoverSubgroups(source = database,
                                       target = t_var,
                                       config = do.call(what = new,
                                                        args = config),
                                       as.df = TRUE)

        #calculate summary statistic
        result[[val]] <- subgroupSummary(original_data = database,
                                         output        = subgroups,
                                         target        = val)
    }

    #if user define target attribute value result has only one element and we want to return
    #summary statistic table as data frame object for simplicity
    if (length(result) == 1) {
        return(result[[1]])
    }
    return(result)
}

#Combination of string vectors elements
str_vector_combination <- function(vec1, vec2) {
    #make string combinations
    qf_method_pairs <- strsplit(x     = as.vector(outer(X   = vec1,
                                                        Y   = vec2,
                                                        FUN = paste,
                                                        sep = "_")),
                                split = "_")
    return(qf_method_pairs)
}


