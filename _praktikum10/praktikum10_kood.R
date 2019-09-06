# praktikum 10
#-----------------

 

#--- Programmeerimine
  
#--- Funktsioonide defineerimine
 
 

#-- ÜL 1.3.1
# 1

zskoor <- function(vektor){
  z <- (vektor - mean(vektor))/sd(vektor)
  return(z)
}

zskoor <- function(vektor, ...){
  if (!is.numeric(vektor)) stop("vektor peab olema arvuline")
  z <- (vektor - mean(vektor, ...))/sd(vektor, ...)
  return(z)
}



zskoor(1:10)
zskoor(c(NA, 1:10))
zskoor(c(NA, 1:10), na.rm = T)
zskoor(c("a", "b"))






# 2

visiidid <- read.table("http://kodu.ut.ee/~annes/Rkursus/visiidid.txt", sep = "\t", header = TRUE)
head(visiidid)


library(dplyr)

uusfun <- function(x){
  kp <- as.Date(x)
  esimene <- min(kp)
  viimane <- max(kp)
  aastaid <- as.numeric(viimane - esimene + 1)/365.25
  kordi <- sum(!is.na(kp))
  keskmiselt <- round(kordi/aastaid, 1)
  return(keskmiselt)
}



tabel <- 
visiidid %>% group_by(ik) %>%
  summarise("keskmine visiitide arv" = uusfun(visiidi_kp))

head(tabel)





#--- Juhuarvud


#-- ÜL 2.0.1
# 1

maatriks <- function(ridu, veerge, lambda){
  x <- rpois(ridu*veerge, lambda)
  m <- matrix(x, nrow = ridu, ncol = veerge)
  return(m)
}

set.seed(234)
maatriks(2, 4, 5)



# 2

test <- function(x){
  veerusumma <- matrix(colSums(x))
  reasumma <- matrix(rowSums(x))
  oodatavad <- reasumma %*% t(veerusumma) /sum(x)
  hiiruut <- sum((x - oodatavad)^2 / oodatavad)
  p.väärtus <-  1 - pchisq(hiiruut, df = prod(dim(x) - 1)) 
  vastus <- list(test.statistik = hiiruut, p = p.väärtus)
  return(vastus)
}

A <- maatriks(2, 2, lambda = 20)
A
test(A)
chisq.test(A, correct = F)

 
# kordamisülesannete lahendused on teises failis
 








