#funkcija odvisna od dplyr knjižnjice
predpriprava <- function(d) {
    #izberi stolpce
    d <- d %>% select(IDPerson,
                      GenderPerson,
                      PersonAge,
                      ATC3)
    
    #določi unikatne vrednosti zdravil in jih sortiraj
    zdravila <- sort(unique(d$ATC3))
    
    #zamenjava vrstnega reda, tarčna vrednost A10 gre na zadnje mesto
    zdravila <- replace(zdravila, 
                        c(which(zdravila == "A10"), length(zdravila)), 
                        zdravila[c(length(zdravila), which(zdravila == "A10"))])
    #binarne vrstice
    for (z in zdravila) {
        d[ ,z] <- as.integer(d$ATC3 == z)
    }
    #izberi stolpce
    d <- d %>% select(IDPerson,
                      GenderPerson,
                      PersonAge,
                      A01:A10)
    return(d)
}

#funkcija, ki stolpce pretvori v factor spremenljivke
kategorije <- function(d) {
    #spol
    d$GenderPerson <- factor(x      = as.character(d$GenderPerson), 
                             levels = c(1, 2), 
                             labels = c("moski", "zenska"))
    
    #starost
    d$PersonAge <- factor(base::cut(x      = as.integer(d$PersonAge), 
                                    breaks = c(min(as.integer(d$PersonAge)), 60, 70, 80, max(as.integer(d$PersonAge)) + 1), 
                                    right  = FALSE))
    
    #ATC3 kode
    a <- base::apply(X      = d[ ,4:ncol(d)], 
                     MARGIN = 2, 
                     FUN    = factor, 
                     levels = c(0,1), 
                     labels = c("NE", "DA"))
    return(data.frame(spol = d$GenderPerson, starost = d$PersonAge, a))
}
#funkcija odvisna od data.table in dply knjižnjice
zdruziBolnikePoLetuRecepta <- function(file_path) {
    #uvozi bazo podatkov
    baza <- readRDS(file = file_path)
    
    #odstrani manjkajoče vrednosti v ATC kodi
    baza <- baza[!is.na(baza$ATC), ]
    
    poLetih <- list()
    leta <- base::unique(lubridate::year(baza$DateIssueDrug))
    #za vsako leto zduži podatke (spol, starost in recepte) o bolniku in jih shrani kot data frame
    for (leto in leta) {
        #podatki na letni ravni
        baza_leto <- predpriprava(d = baza[which(lubridate::year(baza$DateIssueDrug) == leto), ])
        #združi podatke o pacientih
        baza_leto <- baza_leto[,lapply(.SD, function(x) {
            if (length(unique(x)) == 1) {#pogoj za spol
                unique(x)
            } else if (is.double(x)) {#pogoj za starost
                max(x)        
            } else {#pogoj za ATC3 kode
                as.integer(ifelse(1 %in% x, 1, 0))
            }
        }), by = IDPerson]
        
        #shrani v seznam
        poLetih[[paste(leto)]] <- kategorije(d = baza_leto)
    }
    #vrni seznam
    return(poLetih)
}

#odstrani recepte, ki so bili predpisano manjše število od povprečja
odstraniRecepte <- function(d) {
    #število predpisanih posameznih receptov
    recepti <- apply(d[,3:ncol(d)], 2, function(x) {sum(x == "DA")})
    return(d[,c("spol","starost",names(which(recepti > mean(recepti))))])
}

#ploting function
# plotSubgroups <- function(ruleTable, tpr, fpr, legend.pos = "top") {
#     
#     to_plot <- lapply(X   = 1:nrow(ruleTable),
#                       FUN = function(rule, qf, qf_1, qf_2) {
#                           
#                           data.frame(Rule  = c(paste("Rule", rule), 
#                                                paste("Rule", rule)), 
#                                      Value = c(qf[ ,qf_1][rule], 
#                                                -qf[ ,qf_2][rule]), 
#                                      qm    = c(tpr, fpr))
#                           
#                       }, ruleTable[, c(tpr, fpr)], tpr, fpr)
#     
#     
#     to_plot <- plyr::rbind.fill(to_plot)
#     
#     ggplot2::ggplot(data    = to_plot, 
#                     mapping = ggplot2::aes(x    = Rule, 
#                                            y    = Value,
#                                            )) + 
#         ggplot2::geom_bar(data     = to_plot[to_plot$qm == tpr,],
#                           aes(fill = qm),
#                           stat     = "identity", 
#                           position = "identity") + 
#         ggplot2::geom_bar(data     = to_plot[to_plot$qm == fpr,], 
#                           aes(fill = qm),
#                           stat     = "identity", 
#                           position = "identity") + 
#         ggplot2::scale_y_continuous(limits = c(-1, 1), labels = abs) +
#         guides(fill = guide_legend(title = "Quality measure")) +
#         theme(legend.position = legend.pos) +
#         ggplot2::coord_flip()
# }
# 
# #Tabela
# get_evol_table <- function(initial_data, rule_summary, class_val) {
#     
#     #create dataframe from SDEFSR object
#     ruleSet <- plyr::rbind.fill(lapply(rule_summary, data.frame))
#     #set column names
#     names(ruleSet) <- c("subgroup", substr(names(ruleSet)[2:11], 
#                                            start = 17, 
#                                            stop = nchar(names(ruleSet)[2:11])))
#     
#     #remove if, and, then, resistant from rules and split rules with comma
#     rules <- as.character(ruleSet[ ,"subgroup"])
#     rules <- gsub(pattern = "IF ", replacement = "", rules)
#     rules <- gsub(pattern = paste(" THEN", class_val, sep = " "), replacement = "", rules)
#     rules <- gsub(pattern = " AND", replacement = ",", rules)
#     rules <- gsub(pattern = " = ", replacement = "=", rules)
#     
#     #get size of each subgroup
#     rule_size <- unlist(lapply(X   = lapply(X   = as.character(rules),
#                                             FUN = ruleDataset, initial_data),
#                                FUN = nrow))
#     #vector of count of target value for each rule
#     target_count_vector <- unlist(lapply(X   = lapply(X   = as.character(rules),
#                                                       FUN = ruleDataset, initial_data),
#                                          FUN = function(d, target){
#                                              sum(d[,ncol(d)] == target)
#                                          }, class_val))
#     # combine into data frame
#     rule_summary <- data.frame(subgroup = as.factor(rules),
#                                size     = rule_size,
#                                target_count_vector,
#                                ruleSet[names(ruleSet) != "subgroup"])
#     
#     #change target_val name to actual target name
#     names(rule_summary)[which(names(rule_summary) == "target_count_vector")] <- tolower(class_val)
#     
#     return(rule_summary)
# }