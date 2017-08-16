#' @title
#' Confusion matrix
#' @description
#' Confusion matrix in dimension 2 by 2
#' @details
#' Compute confusion matrix dimensions 2x2. Matrix is presented like:
#' \tabular{ccc}{
#' ---------------\tab Predicted: positive value \tab Predicted: negative value(s)\cr
#' Real: positive value\tab TP \tab FN\cr
#' Real: negative value(s)\tab FP \tab TN\cr
#' }
#' Where 1st value is target attribute value user specified for analysing. TP are true positives,
#' TN are true negative, FP are false positives and FN are false negatives. If target attribute has
#' more than one value positive value present specified target value and negative values all other values
#' to form matrix 2x2. If user does not specify target value, confusion matrix addapt for each value as stated
#' before (positive versus others). Calculated quality statistics for subgroup(s) are:
#' \itemize{
#' \item rule SIZE is number of instaces covered by rule conditions
#' \item true positives (TP) are number of positive instances covered by rule or specified target attribute value
#' \item COVERAGE as \eqn{rule_size / data_size}
#' \item SUPPORT as \eqn{tp / data_size}
#' \item positive predictive value (PPV) as \eqn{tp / rule_size}
#' \item SENSITIVITY or recall as \eqn{tp / (tp + fn )}
#' \item SPECIFICITY as \eqn{tn / (tn + fp)}
#' \item F-1 SCORE as \eqn{2*tp / (2 * tp + fp + fn)}
#' }
#'
#' \code{inspection} argument serve for needs in post extraction, deper inspection of selected subgroup. Function
#' can be used for both analysis principles as soon we do analysis with \code{\link{subgroupDiscovery}} function,
#' because of unified subgroup presentation in form \code{attribute=value} separated by comma.
#' @param original_data \code{dataframe} with data we are analizing
#' @param rule_data \code{dataframe} with subset according to subgroup conditions from \code{\link{ruleDataset}}
#' @param target character with target attribute value
#' @param inspection logical, if we apply confusion matrix after analysis for deper inspection of extracted subgroups, i.e.
#' can be aplied after evolutionary subgroups extraction \code{\link{subgroupDiscovery}}
#' @return \code{matrix} with quality metrics
#' @export
confusionMatrix <- function(original_data, rule_data, target, inspection) {
    ###############################################################
    # vector of target values, which first present predicted values
    ###############################################################

    # extract subject that are not in subgroup
    data_not_in_rule <- original_data[!(as.numeric(row.names(original_data)) %in% as.numeric(row.names(rule_data))), ]

    #target attribute has to be in last column
    target_col <- ncol(original_data)
    # count numbers for confusion matrix
    TP <- nrow(rule_data[rule_data[ ,target_col] == target, ])
    FP <- nrow(rule_data[!rule_data[ ,target_col] == target, ])
    FN <- nrow(data_not_in_rule[data_not_in_rule[ ,target_col] == target, ])
    TN <- nrow(data_not_in_rule[!data_not_in_rule[ ,target_col] == target, ])

    target_values <- base::unique(original_data[ ,target_col])
    not_target_val <- target_values[which(target != target_values)]
    #if do not have binary target value
    if (length(not_target_val) != 1)
        not_target_val <- "OTHER"
    # create a confusion matrix
    confusion_matrix <- matrix(data = c(TP, FP, FN, TN), nrow = 2, ncol = 2)
    rownames(confusion_matrix) <- c(paste("Actual:", target, sep = ""), paste("Actual:", not_target_val, sep = ""))
    colnames(confusion_matrix) <- c(paste("Pred:", target, sep = ""), paste("Pred:", not_target_val, sep = ""))

    if (inspection) {
        print(confusion_matrix)
        #transpose matrix
        m <- t(results_quality(confusion_matrix))
        rownames(m) <- c("size",
                         tolower(target),
                         "coverage",
                         "support",
                         "ppv",
                         "sensitivity",
                         "specificity",
                         "f1_score")
        colnames(m) <- "value"
        #round to 2 decimal places and return
        return(round(m, 2))
    }
    return(results_quality(confusion_matrix))
}
###############------------------------------------------------------------------#######################################
#calculate quality measures
results_quality <- function(conf_matrix) {
    # set variable for calculationg quality measures
    data_size <- sum(conf_matrix)
    rule_size <- sum(conf_matrix[, 1])
    tp <- conf_matrix[1, 1]
    fp <- conf_matrix[2, 1]
    fn <- conf_matrix[1, 2]
    tn <- conf_matrix[2, 2]

    # generality
    coverage <- rule_size/data_size
    support <- tp/data_size

    # precision
    ppv <- tp/rule_size

    # hybrid measures
    sensitivity <- tp/(tp + fn)  #== recall
    specificity <- tn/(tn + fp)
    f1_score <- 2 * tp/(2 * tp + fp + fn)

    # create vector of quality measures, ppv - positive predictive value
    quality <- c(rule_size,
                 tp,
                 coverage,
                 support,
                 ppv,
                 sensitivity,
                 specificity,
                 f1_score)

    m <- matrix(data = quality, nrow = 1, ncol = length(quality))
    return(m)
    #return(quality)
}
