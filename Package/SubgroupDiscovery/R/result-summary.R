#' @title
#' Rsubgroup summary table
#' @description
#' Calculate summary table for rsubgroup package subgroups.
#' @details
#' Save calculated quality measures inside data frame object. Function can be used on output from
#' \code{\link[rsubgroup]{DiscoverSubgroups}}.
#' @param original_data \code{dataframe} object with database on which analysis was perfomed
#' @param rsubgroup \code{dataframe} object with results from \code{\link[rsubgroup]{DiscoverSubgroups}} function
#' @param target character with target attribute value
#' @return data frame object
rsubgroupSummaryTable <- function(original_data, rsubgroup, target) {

    c_matrix <- NULL
    for (i in 1:nrow(rsubgroup)) {
        rule_set <- ruleDataset(rsubgroup$description[i], original_data)
        c_matrix <- rbind(c_matrix,confusionMatrix(original_data = original_data,
                                                   rule_data     = rule_set,
                                                   target        = target,
                                                   inspection    = FALSE))
    }
    colnames(c_matrix) <- c("size",
                            tolower(target),
                            "coverage",
                            "support",
                            "ppv",
                            "sensitivity",
                            "specificity",
                            "f1_score")

    #summary statistic with floats rounded to 2 decimal places
    summary_stat <- data.frame(subgroup = rsubgroup$description,
                               quality  = rsubgroup$quality,
                               round(c_matrix,2))

    return(summary_stat)
}

################################------------------------------------------------#######################
#Counter for target value in subgroups
count_target_value <- function(d, target){
    return(sum(d[,ncol(d)] == target))
}

#' @title
#' Evolutionary summary table
#' @description
#' Get quality measures for SDEFSR package rules in data frame object.
#' @details
#' Save calculated quality measures inside data frame object. Function used for rule extraction are:
#' \code{\link[SDEFSR]{FUGEPSD, MESDIF, NMEEF_SD, SDIGA}}. For now only MESDIF and NMEEF_SD are used.
#' @param original_data data frame object with data on which analysis was perfomed
#' @param SDEFSR_output data frame object with results from SDEFSR package functions
#' @importFrom plyr rbind.fill
#' @importFrom stringr word
#' @return data frame with quality measures or list with data frame quality measure for each target attribute value
evolutionSummaryTable <- function(original_data, SDEFSR_output) {

    #SDEFSR object to data frame object
    rules_table <- plyr::rbind.fill(lapply(SDEFSR_output, data.frame))
    #no rules for testing
    if (is.null(rules_table)) {
        stop("No rule for analysis")
    }

    #rename columns
    names(rules_table)[2:ncol(rules_table)] <- names(SDEFSR_output[[1]]$qualityMeasures)

    #extract target value or values from evolutionary rules, target value appear as last word in
    #rule column
    values <- stringr::word(as.character(rules_table$rule),-1)
    target_val <- base::unique(values)

    if (length(target_val) == 1) {
        final_table <- get_evol_table(initial_data = original_data,
                                      rule_summary = rules_table,
                                      class_val    = target_val)
    } else {
        final_table <- list()

        for (val in target_val) {
            #set indexes. Different target rules are grouped together in SDEFSR output
            indexes <- which(values %in% val)
            #arguments for get evol table function
            table_args <- list(initial_data = original_data,
                               rule_summary = rules_table[indexes, ],
                               class_val    = val)

            final_table[[val]] <- do.call(what = get_evol_table,
                                          args = table_args)
        }
    }

    return(final_table)
}

#Prepare final evolution table
get_evol_table <- function(initial_data, rule_summary, class_val) {

    #remove if, and, then, resistant from rules and split rules with comma
    rules <- as.character(rule_summary[ ,"rule"])
    rules <- gsub(pattern = "IF ", replacement = "", rules)
    rules <- gsub(pattern = paste(" THEN", class_val, sep = " "), replacement = "", rules)
    rules <- gsub(pattern = " AND", replacement = ",", rules)
    rules <- gsub(pattern = " = ", replacement = "=", rules)

    #get size of each subgroup
    rule_size <- unlist(lapply(X   = lapply(X   = as.character(rules),
                                            FUN = ruleDataset, initial_data),
                               FUN = nrow))
    #vector of count of target value for each rule
    target_count_vector <- unlist(lapply(X   = lapply(X   = as.character(rules),
                                                      FUN = ruleDataset, initial_data),
                                         FUN = count_target_value, class_val))

    # combine into data frame
    rule_summary <- data.frame(subgroup = as.factor(rules),
                               size     = rule_size,
                               target_count_vector,
                               rule_summary[names(rule_summary) != "rule"])

    #change target_val name to actual target name
    names(rule_summary)[which(names(rule_summary) == "target_count_vector")] <- tolower(class_val)

    return(rule_summary)
}
##################---------------------------------------------------------------------------#####################
#' @title
#' Subgroup summary tables
#' @description
#' Calculate quality measures for extracted rules in subgroup discovery analysis
#' @details
#' Summary statistic for each extracted rule from rsubgroup package and
#' evolutionary algorithms (SDEFSR) package. RSubgroup summary tables are calculated from confusion
#' matrix \code{\link{results_quality}}, while function add to summary static from SDEFSR package:
#' size of subgroup and target attribute value count. Argument output must be a output from:
#' \code{\link[rsubgroup]{DiscoverSubgroups}} or from \code{\link[SDEFSR]{MESDIF}}, \code{\link[SDEFSR]{SDIGA}},
#' \code{\link[SDEFSR]{NMEEF_SD}}, \code{\link[SDEFSR]{FUGEPSD}}.
#' @param original_data \code{dataframe} object with data on which analysis was perfomed
#' @param output output from algorithms for subgroup analysis in R
#' @param target character target attribute variable value, default NULL
#' @param evolution logical, default is FALSE.
#' @return \code{dataframe} with subgroup and summary statistic
subgroupSummary <- function(original_data, output, target = NULL, evolution = FALSE) {
    if (!evolution) {
        quality_table <- rsubgroupSummaryTable(original_data, output, target)
    } else {
        quality_table <- evolutionSummaryTable(original_data, output)
    }
    return(quality_table)
}
