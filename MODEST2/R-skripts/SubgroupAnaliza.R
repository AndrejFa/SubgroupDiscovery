######################################################################################################
# Ohranjeni podatki o pacientih:
# 1. spol
# 2. starost
# 3. ATC3 kode spremenjene v binarne spremenljivke
# 
# Starost je potrebno intervalno spremeniti, na podalagi porazdelitve na celotni bazi uporabil sem 
# [spodnja meja, zgornja meja) interval
# 
# Recepti za diabetes so nastavljeni na ATC3 kodo A10 in so na zadnjem mestu, kot tarčna spremenljivka
# 
# Koda odstrani manjkajoče vnose v ATC kodah!!!
#
# Analiza vsebuje izčrpno metodo "sdmap" in kvalitativno funkcijo "wracc" (rsubgroup paket) in izbere 20 najboljših
# pravil/skupin. Predpriprava podatkov je taka, da lahko uporabimo funkcionalnosti paketa SubgroupDiscovery.
######################################################################################################
# library(lubridate)
# library(dplyr)
# library(SubgroupDiscovery)
# library(data.table)

#absolute path to database file - Spremeni pot
# file_path = "~/Dropbox/MODEST2-koda/MODEST2-model/data/DiabeticsDrugsSample.rds"
# 
database <- readRDS(file_path)
#database <- zdruziBolnikePoLetuRecepta(file_path)
#funkcija, ki ohrani ID, spol, starost in ATC3 kode, ki jih pretvori v binarne spremenljivke
predpriprava <- function(d) {
    #izberi stolpce
    d <- d %>% select(IDPerson,
                      GenderPerson,
                      PersonAge,
                      PostCodePerson,
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
                      PostCodePerson,
                      A01:A10)
    return(d)
}

#funkcija, ki stolpce pretvori v factor spremenljivke
# POTREBNA SPREMEMBA INTERVALOV GLEDE NA PORAZDELITEV 
kategorije <- function(d) {
    #spol
    d$GenderPerson <- factor(x      = as.character(d$GenderPerson), 
                             levels = c(1, 2), 
                             labels = c("moski", "zenska"))
    
    #starost
    d$PersonAge <- factor(base::cut(x      = as.integer(d$PersonAge),
                                        breaks = seq(0, 105, 5),
                                        right  = FALSE))
    d$PersonAge <- plyr::revalue(d$PersonAge, c("[85,90)"="[85+", "[90,95)"="[85+", "[95,100)"="[85+", "[100,105)"="[85+"))
    #ATC3 kode
    a <- base::apply(X      = d[ ,5:ncol(d)], 
                     MARGIN = 2, 
                     FUN    = factor, 
                     levels = c(0,1), 
                     labels = c("NE", "DA"))
    return(data.frame(spol = d$GenderPerson, starost = d$PersonAge, a))
}

#funkcija, ki za vsako leto združi podatke o bolnikih in jih pretvori v data.frame objekt
zdruziBolnikePoLetuRecepta <- function(file_path) {
    #uvozi bazo podatkov
    baza <- readRDS(file = file_path)
    
    #odstrani manjkajoče vrednosti v ATC kodi
    baza <- baza[!is.na(baza$ATC), ]
    
    poLetih <- list()
    leta <- base::unique(lubridate::year(baza$DateIssueDrug))
    #za vsako leto zduži podatke (spol, starost in recepte) o bolniku in jih shrani kot data frame
    for (leto in leta) {
        yearData <- baza[which(lubridate::year(baza$DateIssueDrug) == leto), ]
        #podatki na letni ravni
        baza_leto <- predpriprava(d = yearData)
        #združi podatke o pacientih
        baza_leto <- baza_leto[,lapply(.SD, function(x) {
            if (length(unique(x)) == 1) {#pogoj za spol in poštno številko
                unique(x)
            } else if (is.double(x)) {#pogoj za starost
                max(x)        
            } else {#pogoj za ATC3 kode
                as.integer(ifelse(1 %in% x, 1, 0))
            }
        }), by = IDPerson]
        
        #A10A, A10B or both drug received
        #atc4 <- insulin(yearData)
        
        intData <- kategorije(d = baza_leto)#[which(baza_leto$A10 == 1), ])
        #shrani v seznam
        poLetih[[paste(leto)]] <- intData #data.frame(intData[, 1:2], atcDrug = atc4, intData[, 3:ncol(intData)])
    }
    #vrni seznam
    return(poLetih)
}

#odstrani recepte, ki so bili predpisani v manjšem število od povprečja
#z lapply uporabi na rezultatu, ki ga vrne funkcija zdužiBolnikePoLetuRecepta()
odstraniRecepte <- function(d) {
    #število predpisanih posameznih receptov
    recepti <- apply(d[,4:ncol(d)], 2, function(x) {sum(x == "DA")})
    return(d[,c("spol","starost",names(which(recepti > mean(recepti))))])
}

#database <- lapply(database, odstraniRecepte)

#Analiza podatkov, po starostnih skupinah za vsako leto
poStarostnihSkupinah <- function(list_data) {
    return(lapply(list_data, function(x){
        sapply(as.character(sort(base::unique(x$starost))), function (y) {
            subgroupDiscovery(data_config = list(data   = x[which(x$starost == y), names(x)[c(-2,-3)] ], 
                                                 target = c("A10", "DA")), 
                              method      = "sdmap", 
                              qf          = "wracc", 
                              k           = 20,
                              relfilter   = TRUE, 
                              attributes  = names(x)[c(-2, -3)])
        }, simplify = FALSE, USE.NAMES = TRUE)
    }))
}

#age_analysis <- poStarostnihSkupinah(database)
#Analiza podatkov na letni ravni
poLetih <- function(list_data) {
    return(lapply(list_data, function(x){
        subgroupDiscovery(data_config = list(data   = x[,names(x)[-3]], 
                                             target = c("A10", "DA")), 
                          method      = "sdmap", 
                          qf          = "wracc", 
                          k           = 20,
                          relfilter   = TRUE, 
                          attributes  = names(x)[-3])
        }))
}

#years_analysis <- poLetih(database)

# counter <- c()
# for (col in names(database$`2015`)[3:ncol(database$`2015`)]) {
#     counter <- c(counter, sum(database$`2015`[,col] == "DA"))
# }


# insulin <- function(yearData) {
#     ids <- unique(yearData$IDPerson)
#     val <- c()
#     for (id in ids) {
#         d <- yearData[yearData$IDPerson == id, ]
#         d <- substr(d$ATC, 1, 4)
#         
#         if (all(c("A10A", "A10B") %in% d )) {
#             val <- c(val, "both")
#         } else if ("A10A" %in% d) {
#             val <- c(val, "A10A")
#         } else if ("A10B" %in% d) {
#             val <- c(val, "A10B")
#         }
#     }
#     return(val)
# }

