# --- Rakendustarkvara R
# --- VI praktikum ----


# --- 1. Pakett ggplot2. Skaalade muutmine ----

library(ggplot2)

# Maakondade andmestik
mk <- read.table("http://kodu.ut.ee/~annes/Rkursus/maakonnad.txt", sep = " ", header=T)
head(mk)

# Skaleerimisfunktsiooni rakendamiseks tuleb see lisada joonisele
p2 <- ggplot(mk, aes(per_capita_inc, unemployment_rate, colour = State)) + geom_point()
p2 + scale_x_continuous(trans = "log10", breaks = c(1/2, 1:4) * 10^4)

# Teljepiiride määramine 
rotid <- nlme::BodyWeight
rotid <- rotid[rotid$Diet != 1, ]
p <- ggplot(rotid, aes(factor(Time), weight)) + geom_point(color = "gray") +
  stat_summary(geom = "errorbar", fun.data = mean_se, fun.args = list(mult = 1.96)) +
  stat_summary(geom = "point", fun.y = mean)
p

p + ylim(390, 600)





# --- 1.1 Pidevate/arvuliste skaalade muutmine ----


# Argumendi limits mõju: võrdle regressioonsirgeid
p2 + geom_smooth(method = lm) + scale_x_continuous(limits = 1:2*10^4) +
  labs(title = "x-teljel kasutame ja näeme\npunkte vahemikus 10000-20000")
p2 + geom_smooth(method = lm) + coord_cartesian(xlim = 1:2*10^4) +
  labs(title = "kasutame kõiki punkte, tulemust\nnäeme vahemikus 10000-20000")




# --- 1.2 Diskreetsete skaalade muutmine ----

# Oluline on ka väärtuste etteandmise järjekord:
b <- ggplot(data = mk, aes(State, unemployment_rate)) + geom_boxplot()
b + scale_x_discrete(limits = c("Texas", "Maryland", "Alaska"))

#Võrdle, mis mõju on argumendil `breaks`:
# b + scale_x_discrete(breaks = c("Texas", "Maryland", "Alaska"))

# Diskreetsel skaala puhul saab limits argumendiga määrata millisete väärtusklasside elemendid värvitakse
# ja millised jäetakse värvimata (vaikimisi jäävad halliks):
b1 <- ggplot(data = mk, aes(bachelor, unemployment_rate, color = State)) + geom_point()
b1 + scale_color_discrete(limits = c("Texas", "Maryland", "Alaska"))
# NA väärtuse värvi saa ka muuta
b1 + scale_color_discrete(limits = c("Texas", "Maryland", "Alaska"), na.value = "white")



# --- ÜL 1.2.1 ----

# olemas joonis, mida vaja täiendada 
joonis <- ggplot(data = mk, aes(x = high_scl, y = bachelor)) + geom_point()
joonis


# 1. y telg
joonis1 <- joonis + 
  scale_y_continuous(name = "Higher education percentage", 
                            breaks = seq(0, 100, 25) ,
                            labels = paste0( seq(0, 100, 25), "%"),
                            limits = c(0, 100))
joonis1

# 2. punktid värvida + legendi nimetus muuta  (värvile arvuline tunnus: gradientskaala)
joonis1 + geom_point(aes(color = per_capita_inc)) +labs(color = "Per capita income")


# 3. uus tunnus tekitada
mk$income_class <- cut(mk$per_capita_inc, 
                       breaks = quantile(mk$per_capita_inc, 
                                         c(0, .2, .4, .6, .8, 1)),
                       labels = c("Very low", "Low", 
                                  "Medium", "High", "Very high"))


# 4. lisada uus tunnus joonisele
joonis4 <- joonis1 + geom_point(data = mk, aes(color = income_class)) #! siin peab geom_point()-le uuesti andmestiku ette andma, sest oleme seda muutnud! oluline on ka argumendi nime 'data' väljakirjutamine
joonis4

# värv ainult minimaalsele ja maksimaalsele
joonis4 + scale_color_discrete(limits = c("Very low", "Very high"))



# 5. mis juhtub kui lisada guide = FALSE argument? (legendi eemaldamine)
joonis4 + scale_color_discrete(limits = c("Very low", "Very high"), guide = FALSE)




 


# Enne j?rgmist praktikumi l?bi vaadata:
#--- 1.3 V?rviskaalade muutmine
#-- 2. Joonise viimistlemine

 