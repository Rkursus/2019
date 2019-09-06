# praktikum 9
#-----------------


#--- Sõnetöötlus
#install.packages("stringr")  
library(stringr)


 

#--- Alamsõne otsimine, muutmine
#---- Ül 1.2.1
# 1
mass <- read.table("http://kodu.ut.ee/~annes/Rkursus/mass.txt", sep = "\t", header = TRUE)
str(mass)

table(mass$COW)

palgaline <- str_detect(mass$COW, "[Ee]mployee")
table(palgaline)

mean(mass$WAGP[palgaline], na.rm = T)



# teine võimalus: esmalt kõik tekst teisendada väiketähtedega tekstiks
palgaline2 <- str_detect(str_to_lower(mass$COW), "employee")


# 2
link <- "http://kodu.ut.ee/~annes/Rkursus/"
isikukoodid <- read.table(str_c(link, "isikukoodid.txt"))[1,]
str(isikukoodid)

ik <- unlist(str_split(isikukoodid, ","))

table(str_sub(ik, 1, 1))  # millised algusnumbreid esineb
sugu <- ifelse(str_sub(ik, 1, 1) %in% c(4, 6), "Naine", "Mees")
sugu2 <- factor(str_sub(ik, 1, 1) %in% c(4, 6), labels = c("M", "N"))

isikud <- data.frame(ik, sugu, sugu2)
head(isikud)


 

 



#--- Kuupäevad    
#---- ÜL 2.0.1

# 1
algus <- ifelse(str_sub(ik, 1, 1) %in% c(3, 4), 19, 20) 
skp <- as.Date(str_c(algus, str_sub(ik, 2, 7)), format = "%Y%m%d")

isikud$skp <- skp
head(isikud)

# 2
vanus <- floor(as.numeric(Sys.Date() - skp)/365.25)
isikud$vanus <- vanus
head(isikud)

#3 Millal on 10 000 päeva tänasest möödunud: 
Sys.Date() + 10000
# oma 10000 päeva sünnipäeva leidmiseks asenda tänane kuupäev oma sünnipäevaga

 


######################   9. praktikumi juhend


#--- Programmeerimine
  


#----  ÜL 1.1.1

# 1 keskmise ja summarse töötundide tabel üle 15 aastastele
osa <- mass[mass$AGEP > 15 & 
              str_sub(mass$OCCP, 1, 3) != "UNE" & 
              !is.na(mass$OCCP),   ]
str(osa)

# tühi tabel tsüklis täitmiseks
tabel <- data.frame(grupp = NA, keskmine = NA, summa = NA)

rida <- 1
for (amet in unique(osa$OCCP)){
  tabel[rida, "grupp"] <- amet
  tabel[rida, "keskmine"] <- mean(osa$WKHP[osa$OCCP == amet], na.rm = T)
  tabel[rida, "summa"] <- sum(osa$WKHP[osa$OCCP == amet], na.rm = T)
  rida <- rida + 1
}

head(tabel)
dim(tabel)


# 2
# sama tabel data.table-süntaksiga
library(data.table)
massdt  <- as.data.table(mass)

massdt[AGEP > 15 & str_sub(OCCP, 1, 3) != "UNE" & !is.na(OCCP),
       .(keskmine = mean(WKHP, na.rm = T), summa = sum(WKHP, na.rm = T)),
        by = OCCP]




#-- kulunud aeg: tsükkel
system.time({
  osa <- mass[mass$AGEP > 15 & 
                str_sub(mass$OCCP, 1, 3) != "UNE" & 
                !is.na(mass$OCCP),   ]
  tabel <- data.frame(grupp = NA, keskmine = NA, summa = NA)
  rida <- 1
  for (amet in unique(osa$OCCP)){
    tabel[rida, "grupp"] <- amet
    tabel[rida, "keskmine"] <- mean(osa$WKHP[osa$OCCP == amet], na.rm = T)
    tabel[rida, "summa"] <- sum(osa$WKHP[osa$OCCP == amet], na.rm = T)
    rida <- rida + 1
  }
print(tabel)  
})

#-- kulunud aeg: data.table
system.time({
  massdt[AGEP > 15 & str_sub(OCCP, 1, 3) != "UNE" & !is.na(OCCP),
         .(keskmine = mean(WKHP, na.rm = T), summa = sum(WKHP, na.rm = T)),
         by = OCCP]
})







#--- Tingimuslause
   
#---- ÜL 1.2.1
# 1

visiidid <- read.table("http://kodu.ut.ee/~annes/Rkursus/visiidid.txt", sep = "\t",
                       header = TRUE)
head(visiidid)


loenda <- 0
for(i in unique(visiidid$ik)){
  osa <- visiidid[visiidid$ik == i, ]
  osa <- osa[order(osa$visiidi_kp),]
  vahe <- osa$vererohk[1] - osa$vererohk[nrow(osa)]
  if (abs(vahe) > 5) {
    print( paste(i, "vahe: ", vahe))
    loenda <- loenda + 1}
}
loenda






