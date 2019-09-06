# praktikum 7
#-----------------


#--- eelmise praktikumi lõpust
#--- Joonised paketiga ggplot2: värviskaalade muutmine

# install.packages("ggplot2")
library(ggplot2)


# andmed:  
mk <- read.table("http://kodu.ut.ee/~annes/Rkursus/maakonnad.txt", sep = " ", header = T)


  
 


#-- ÜL 1.3.1
#  eelmise ülesande joonis: hajuvusdiagramm kesk ja kõrgharidsuse %, värvid sissetuleku klassi järgi

mk$income_class <- 
  cut(mk$per_capita_inc, 
      br = quantile(mk$per_capita_inc, p = seq(0, 1, .2)), 
      include.lowest = T, 
      labels = c("Very low","Low", "Medium", "High", "Very high"))

joonis4 <- ggplot(data = mk, aes(x = high_scl, y = bachelor)) + 
  geom_point() + 
  scale_y_continuous(name = "Higher education percentage", limits = c(0, 100), breaks = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%")) + 
  geom_point(data = mk, aes(color = income_class))
joonis4


# diskreetse  värviskaala muutus
joonis4 + 
  scale_colour_brewer(type = "div", palette = "RdBu")
 






#--- Joonise viimistlemine

# valime valmis teemakomplekti, lisame joonisele
joonis4 + theme_dark()
joonis4 + theme_bw()
joonis4 + theme_void()  # 'tühi' 

 
# vaikimisi kujundus on   theme_gray()
joonis4 + theme_gray()



# kehtivas teemas mingi elemendi väljavahetamine
joonis4   # esmalt vaatame joonist vaikimisi kujundusega

# kasutame elemendi vahetamiseks theme_replace(), argumendis kirjas muudatus. 
# mida teeb omistamine: muutujasse 'vana' jääb kirja seni kehtinud kujundus, selle saab hiljem taastada
vana <- theme_replace(
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.major = element_line(size = 2, color = "white"))

# pärast eelmist käsku on joonise taustad rohelised
joonis4

# taastame vana teema
theme_set(vana)

# nüüd järgnevad joonised jälle 'tavalise' halli taustaga
joonis4


# valmis teemakomplekti kehtestamine
theme_set(theme_dark())

# edaspidi kehtib 'dark'-teema 
joonis4



# selleks, et vaadata mingi teema parameetreid
theme_bw()
# selleks, et näha kehtiva teema parameetreid
theme_get()




#-- ÜL 2.0.1
#1.  legendi paigutus, x-telje siltide kohendamine, joonise pealkiri
joonis5 <- 
joonis4 + ggtitle("Pealkiri") + 
         theme(plot.title = element_text(size = 20, # pealkirja suurus
                                    color = "red",  # pealkiri punaseks
                                    hjust = 0.5),   # pealkiri joondada keskele(horisontaalsuund)
          axis.text.x = element_text(angle = 45),
          legend.position = "bottom")
joonis5 





#---- selleks, et muuta joonise teksti tüüpi, fondiperet
library(extrafont)
font_import()  # NB! fontide import võtab aega!!
loadfonts(device = "win")

# mis fonte saab valida?
fonts()

# lisame joonisele, üldise teksti-elemendi muutus, peaks mõjuma kõikidele tekstidele joonisel
joonis5 + theme(text = element_text(size = 12, family = "Papyrus"))


 





#--- dplyr pakett

mass <- read.table("http://kodu.ut.ee/~annes/Rkursus/mass.txt", sep = "\t", header = T)
   


#---- ÜL 1.1.1

# 1

# variant A, aheldamist kasutades
mass %>% 
  filter(AGEP > 15 &  AGEP < 86) %>% 
  mutate(vanusgrupp = cut(AGEP, br = seq(15, 85, 5))) %>%
  group_by(vanusgrupp, SEX) %>%
  summarise(kesk = round(mean(WKHP, na.rm = T),1), 
            max = max(WKHP, na.rm = T),
            "gruppide maht" = n(),
            "pole NA" = sum(!is.na(WKHP)))


# variant B, aheldamiseta
tabel <- 
summarise(group_by(mutate(filter(mass, AGEP > 15 &  AGEP < 86), 
         vanusgrupp = cut(AGEP, br = seq(15, 85, 5))), vanusgrupp, SEX),
          kesk = round(mean(WKHP, na.rm = T), 1), 
          max = max(WKHP, na.rm = T),
          "gruppide maht" = n(),
          "pole NA" = sum(!is.na(WKHP)))

tabel


# 2. joonis


# andemstiku võiks esmalt pikaks teha, et keskmised ja maksimumid oleks ühes veerus, lisaks veerg milles on indikaator
library(reshape2)
pikk <- melt(tabel, measure.vars = c("kesk", "max"))
pikk


ggplot(pikk, aes(x = vanusgrupp, y = value, 
           color = SEX, linetype = variable, 
           group = interaction(SEX, variable))) + 
  geom_line() + 
  scale_color_discrete(name = "Sugu", labels = c("Naine", "Mees"))+
  scale_linetype_discrete("Töötunnid", labels = c("keskmine", "maksimaalne"))

 



# 3. ühendada kaks eelmist ülesannet kasutades aheldamist
mass %>% 
  filter(AGEP > 15 &  AGEP < 86) %>% 
  mutate(vanusgrupp = cut(AGEP, br = seq(15, 85, 5))) %>%
  group_by(vanusgrupp, SEX) %>%
  summarise(kesk = round(mean(WKHP, na.rm = T),1), 
            max = max(WKHP, na.rm = T),
            "gruppide maht" = n(),
            "pole NA" = sum(!is.na(WKHP))) %>%
  melt(measure.vars = c("kesk", "max")) %>%
  ggplot(aes(x = vanusgrupp, y = value, 
             color = SEX, linetype = variable, 
             group = interaction(SEX, variable))) + 
  geom_line() 




# siiani 7. praktikumis
# kodus vaata üle  7. praktikumi materjalist ptk 2: Pakett data.table
 