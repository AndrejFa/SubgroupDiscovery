#' @title
#' Dynamic filter
#' @description
#' Filter data by attribute (column) value
#' @details
#' Filter database by variable value. After applied filter instances of minor target attribute value are added
#' in case of imbalace data or keep all target value occurences in subset data. If subset data is used for
#' further analysis columns on which filter have been applied are removed.
#' @param dataset \code{dataframe} object, database on which analysis was perfomed
#' @param columns character \code{vector} with columns name
#' @param values character \code{vector} with values of selected columns
#' @param add_minors logical, default FALSE
#' @param analysis logical, default FALSE
#' @return subset \code{dataframe}
#' @export
dynamicFilter <- function(dataset, columns, values, add_minors = FALSE, analysis = FALSE) {
    #dynamically filter database depend on column values
    sub_set <- dataset

    for (i in 1:length(columns)) {
        bool_vector <- sub_set[ ,columns[i]] == values[i]
        sub_set     <- sub_set[bool_vector, ]
    }

    #add instaces of minor class if they are not all included in subset
    if (add_minors) {
        sub_set <- add_minor_instances(dataset = dataset,
                                       sub_set = sub_set)
    }

    #if we need subset for analysis we remove variables according to which we have applied
    #dynamic filter
    if (analysis) {
        sub_set <- sub_set[ ,!(names(sub_set) %in% columns)]
    }

    return(sub_set)
}
################----------------------------------------------------------------------------#############
#' @title
#' Subgroup subset
#' @description
#' Subset according to subgroup conditions
#' @details
#' Function filter dataset for analysis according to conditions in extracted rules. Function can
#' extract subset for both approach: rsugroup package and evolutionary approach. Subgroups
#' conditions has to be outputed from \code{\link{subgroupDiscovery}} and then analyse each subgroup
#' in details if necessary. Function can be used with \code{\link[base]{lapply}}.
#' @param subgroup character \code{vector} of rule description from applied subgroup method output from
#' \code{\link{subgroupDiscovery}}.
#' @param original_data \code{dataframe} object with initial data for analysis
#' @return \code{dataframe} with subset(s) of a rule(s)
#' @export
ruleDataset <- function(subgroup, original_data) {

    splitted_rule <- unlist(lapply(X   = unlist(strsplit(as.character(subgroup), ", ")),
                                   FUN = strsplit, "="))

    # every odd index represend columns, vector of variables names
    columns <- splitted_rule[seq(1, length(splitted_rule), 2)]

    # every even index respresent value variable, vector of variables values
    values <- splitted_rule[seq(2, length(splitted_rule), 2)]

    # extract subset according to variable-value pairs in rule description
    rule_subset_data <- dynamicFilter(dataset = original_data,
                                      columns = columns,
                                      values  = values)

    return(rule_subset_data)
}
