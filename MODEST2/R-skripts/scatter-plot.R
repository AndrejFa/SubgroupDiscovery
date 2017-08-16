scatter.plot <- function(ruleData, x, y, col = "A10", xlab = NULL, ylab = NULL, legend_lab = "diabetes", legend_pos = NULL, seed = 123) {
    set.seed(seed)
    ggplot(data = ruleData, aes(x      = ruleData[,x],
                                y      = ruleData[,y],
                                colour = ruleData[,col])) +
        labs(x      = ifelse(is.null(xlab), x, xlab),
             y      = ifelse(is.null(ylab), y, ylab),
             colour = legend_lab) +
        theme(legend.position = ifelse(is.null(legend_pos), "right", legend_pos)) + geom_jitter()
}