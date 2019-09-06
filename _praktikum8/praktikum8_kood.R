# praktikum 8
#-----------------



#----- data.table

library(data.table)  
 

#----ÜL 2.1.1


mass <- read.table("http://kodu.ut.ee/~annes/Rkursus/mass.txt",  sep = "\t", header = T)


#1 tabeli teisendamine data.table-tüübiks
dt <- as.data.table(mass)
str(dt)


#2. sageduse leidmine
dt[AGEP > 60 & SEX == "Male", .N]
dt[AGEP > 60, .N, by = SEX]


#3. abieluseisu sagedustabel
dt[, .(sagedus = .N), by = MAR]


#4. järjestada
dt[, .(sagedus = .N), by = MAR][order(sagedus)]

# järjestamine kui tulemus objektina salvestatud: võtme määramine
tab1 <- dt[, .(sagedus = .N), by = MAR]
setkey(tab1, sagedus)
tab1

#5. osakaalu lisamine
tab1[, osak := round(prop.table(sagedus), 2)]
tab1[, osak2 := round(sagedus/sum(sagedus), 2)]
tab1


# 6
dt[,.(kesk = round(mean(AGEP), 1),
      sh = round(sd(AGEP), 1), 
      "levinum haridus" = 
        names(sort(table(SCHL), decreasing = T))[1]), 
   by = .(SEX, MAR)]


names(dt)






############################## 8. praktikumi juhend



#--- Sõnetöötlus

#-- sõne pikkus, kokkukleepimine ja eraldamine

#install.packages("stringr")  
library(stringr)

 
#---- Ül 1.1.1
# 1. valdkondi on 23 kui kaasta ka töötud (UNEMPLOYED -> UNE)
mass$OCCP[1:4]

valdkonnad <- str_sub(mass$OCCP, 1, 3)
table(valdkonnad)
dim(table(valdkonnad))
length(unique(valdkonnad[!is.na(valdkonnad)]))  # NA tase välja jätta




# 2
mass$OCCP1 <- as.character(mass$OCCP)  # kui jätta faktoriks, siis ei saa uut väärtust lisada

mass$OCCP1[valdkonnad == "UNE" & !is.na(valdkonnad)] <- 
  str_c("UNE-", mass$OCCP[valdkonnad == "UNE" & !is.na(valdkonnad)]) 

mass$OCCP1[valdkonnad == "UNE" & !is.na(valdkonnad)]
        

 

#-- Järgnevad punktid praktikum 8 juhendist kodus läbi proovida
