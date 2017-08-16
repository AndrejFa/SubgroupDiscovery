#Add minor minor instaces of target variable if all not included in desired subset
add_minor_instances <- function(dataset, sub_set) {

        #prepare vectors to compare it
        dataset       <- dataset[dataset$SUSCEPTIBILITY == "RESISTANT", ]
        dataset_minor <- row.names(dataset)
        subset_minor  <- row.names(sub_set[sub_set$SUSCEPTIBILITY == "RESISTANT", ])

        #get true - false vector if not all minor class values are included in subset
        binary_vector <- dataset_minor %in% subset_minor

        #check if we have all minor class included
        if (!all(binary_vector)) {
                #get minor instances that are not included in subset and append it to subset
                add_rows <- dataset[!binary_vector, ]
                sub_set  <- rbind.data.frame(sub_set, add_rows)
        }

        return(sub_set)
}
