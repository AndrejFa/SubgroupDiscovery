elapsed_months <- function(x) {
    
    for(i in 1:(length(x) - 1)) {
        print(abs(12*(year(x[i]) - year(x[i + 1])) + (month(x[i]) - month(x[i+1]))))
    }
}

i <- 0

# for (id in unique(diabetes$IDPerson)) {
#     print(sort(dmy(diabetes[diabetes$IDPerson == id, 20])))
#     print("          ")
#     i <- i + 1
#     if (i == 10)
#         break
# }

#leto
unique(year(dmy(diabetes[,20])))
#mesec
sort(unique(month(dmy(diabetes[,20]))))

