# 3. praktikum
#-----------------


# andmestik
andmed <- read.table("http://kodu.ut.ee/~annes/Rkursus/mass.txt",
                     header = T, sep = "\t")
str(andmed)
head(andmed)
 



#---- ÜL 1.2.1

# 1. iga viies rida valida
indeksid <- seq(1, nrow(andmed), 5)
valik5 <- andmed[indeksid, ]

# 2. valida välja need, kel MIG-tunnuse väärtus pole teada
levels(andmed$MIG)
summary(andmed$MIG)
osa <- andmed[is.na(andmed$MIG), ]
str(osa)
summary(osa)


 



#---- ÜL 1.3.1

# 1. mehed %
prop.table(table(andmed$SEX))
sum(andmed$SEX == "Male")/length(andmed$SEX)*100
 

# 2. aastapalga 0.8 kvantiil
quantile(andmed$WAGP, .8, na.rm = TRUE)


# 3. lahutatute oskaal, mehed vs naised
levels(andmed$MAR)

prop.table(table(andmed$SEX, andmed$MAR), 1)
prop.table(table(andmed$SEX, onlahutatud = andmed$MAR == "Divorced"), 1)

# kui tabeli read-veerud vahetada:
table(andmed$MAR, andmed$SEX)
prop.table(table(andmed$MAR, andmed$SEX), 2)


# 4. mitu üle 74 naist doktorikraadiga
levels(andmed$SCHL)

sum(andmed$SEX == "Female"  & andmed$AGEP > 74 & andmed$SCHL == "Doctorate degree", na.rm = T)

# sagedustabeli abil
table(naised = andmed$SEX == "Female", 
      üle74 = andmed$AGEP > 74, 
      doktor =  andmed$SCHL == "Doctorate degree")



# 5. mitmel doktori, magistri, baka kraad
sum(andmed$SCHL %in% c("Bachelor's degree","Master's degree" ,"Doctorate degree"))

table(andmed$SCHL == "Bachelor's degree" | 
        andmed$SCHL == "Master's degree" | 
        andmed$SCHL == "Doctorate degree")


# 6. keskmine aastapalk mehed vs naised
by(andmed$WAGP, andmed$SEX, mean, na.rm = T)


# 7. sama arvutus, aga valikandmestikus (valik5)
by(valik5$WAGP, valik5$SEX, mean, na.rm = T)






#---- ÜL 1.4.1

# 1. vanusgrupi tunnuse tekitamine
andmed$vanusgrupp <- cut(andmed$AGEP, 
                         c(0, 17, 49, 64, Inf), 
                         include.lowest = TRUE)
table(andmed$vanusgrupp)

# sama, aga intervallid ümbernimetada
andmed$vanusgrupp1 <- cut(andmed$AGEP, 
                          c(0, 17, 49, 64, Inf), 
                          include.lowest = TRUE, 
                          labels = c("0-17", "18-49", "50-64", "65+"))
table(andmed$vanusgrupp1)




# 2. USA kodakondsuseta vastajad: kas vanusgruppide jaotus meestel-naistel erinev

# mittkodanike alamandmestik
levels(andmed$CIT)
mitte <- andmed[andmed$CIT == "Not a citizen of the U.S.",]

# soo ja vanusgrupi sagedustabel
tab <- table(mitte[, c("SEX", "vanusgrupp1")]); tab

# vanusgruppide jaotus soo kaupa
tab1 <- prop.table(tab, 1);tab1

# joonis (jaotused on sarnased)
barplot(t(tab1), 
        legend.text = TRUE,
        xlim = c(0, 6))


# joonis, täiendatud
barplot(t(tab1), 
        main = "Vanusgruppide jaotus, mittekodanikud",
        legend.text = TRUE, args.legend = list(title = "Vanusgrupid", bty = "n"),
        names.arg = c("naised", "mehed"),
        col = c("#ffff99", "#fdc086", "#beaed4", "#7fc97f"),
        ylab = "osakaal",
        xlim = c(0, 3))
