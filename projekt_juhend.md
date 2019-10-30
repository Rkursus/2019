---
layout: page
title: Projekti juhend
---

# Projektitöö juhend

**Projekt tuleb esitada hiljemalt**

- **I rühm**:   24. oktoober 2019 kell 12:00
- **II rühm**:  05. detsember 2019 kell 12:00

Projektid esitage Moodle keskkonna kaudu. Kõikides saadetud töö failide nimedes peab kajastuma tudengi nimi.

Projektide valmimise käigus tekkinud küsimuste ja probleemide lahendamiseks on võimalus tulla ja küsida järgmistel aegadel:

- **I rühm** T, 22. oktoober 2019 kell 14:15 aud. J.Liivi 2 - 003 (eelnevalt peab konsultatsiooni soovist teada andma).
- **II rühm** T, 03. detsember 2019 kell 14:15 (eelnevalt peaks konsultatsiooni soovist teada andma).



## Eesmärk

Projektitöö eesmärgiks on näidata, kuivõrd on tudeng omandanud oskused kasutada R-i andmete töötlemiseks. Samuti oskust R-ga analüüside tulemusi vormistada (nt. joonised).

Sobilik on näiteks kirjeldava analüüsi läbiviimine mõnel andmestikul. Kirjeldav analüüs võiks sisaldada kirjeldavat statistikat (keskmised, mediaanid, miinimum, maksimum jms, mis on parasjagu oluline), jaotuseid (sagedus-, jaotustabelid), graafikuid (histogrammid, karpdiagrammid, hajuvusgraafikud ja muu parasjagu oluline) ja vastuseid andmestikust kerkinud olulistele küsimustele.


## Andmestik

Projektis kasutage oma andmestikku või otsige internetist midagi sobivat (vt. nt. [www.data.gov](https://www.data.gov/), [data.gov.uk](https://data.gov.uk/), [www.europeansocialsurvey.org](https://www.europeansocialsurvey.org/data/country.html?c=estonia) *guugelda* "sample datasets" jne). Andmete päritolu tuleb töös viidata.

**NB! Andmestik peaks kindlasti sisaldama: **

* vähemalt ühte pidevat tunnust (nt. pikkus, kaal, kestus, sissetulek jne);
*	vähemalt kahte kategoriseerivat (nominaalset) tunnust (nt. haridus, sugu, elukoht jne); 
*	vähemalt ühte diskreetset tunnust (nt. laste arv leibkonnas, vanuse kategooriad, kroonlehtede arv õiel jne.);
*	kas aja tunnust või mingit teksti sisendit. 


## Nõuded projektile

Projekt peab olema sisukas, eesmärk ei ole näidata seosetuid R käskude kasutamist, vaid oskust vastavalt andmestikule valida sobivaid R-i vahendeid andmete analüüsiks. Enne valitud andmestikuga tegelema hakkamist võiks valmis mõelda olulisemad küsimused või mida konkreetsest andmestikust teada saada võiks, mis on kõige huvitavamad küsimused.

**Soovitus** Enne kui hakkad R-ga koodi kirjutama püstita 4 küsimust, mida sinu valitud andmestikust saaks järgi uurida.

Projekt peab olema korrektselt (sealhulgas keeleliselt korrektselt) vormistatud PDF-formaadis fail. Pikkus ca 8 lehekulge A4 formaadis. 


Projekti võib esitada kahel viisil:

1. Rmarkdown keeles kirjutatud raport (soovitatav), esitada tuleb:

    * kasutatud andmestik(ud) eraldi failina;
    * Rmarkdown fail, mis sisaldab nii koodi (NB! kommentaaridega!!) kui analüüsiteksti;
    * Rmarkdown faili PDF väljatükk.


1. Mõne teise tekstitöötlus-programmis (nt. MS Word, OpenOffice, LaTeX) vormindatud raport. Esitada tuleb:

    * kasutatud andmestik(ud) eraldi failina;
    * vormistatud analüüsi tekst (ei sisalda koodi ega vormindamata R väljatrükki!);
    * kommenteeritud R koodi eraldi failina (tõrgeteta töötav).



### Nõuded detailsemalt

Oskused, mida kasutada võiks:

* andmete sisselugemine (nt. `read.table`, `read_excel`, jne)
* tüübiteisendused (nt. `as.numeric`, `as.character`, faktorite moodustamine või tasemete ümberjärjestamine)
* esmane andmekirjeldus (nt. `summary`, `nrow`, `ncol`, `names`)
* kirjeldav statistika (nt. `mean`, `sd`, `median`, `min`, `max`, sagedus- ja jaotustabelid jmt)
* alamhulkade eraldamine andmetest (nt. `select`, `filter`, [, $, veerunimed, reaindeksid, eriti aga tõeväärtusvektorid)
* sõnetöötlus, kuupäevade töötlemine (nt. isikukoodist soo eraldamine, moodustada `<aasta>-<nädal>` muutuja)
* andmete teisendamine pika ja laia formaadi vahel (nt. `melt`, `cast`, koos agregeerimisega)
* graafika `ggplot2`-ga
* paketi `dplyr` või `data.table` käskude kasutamine
* lisafunktsioonide kirjutamine (nt. arvutuskäik, mida tihti kasutatakse muudetakse funktsiooniks)


## Projekti hindamine

Projekti hindamisel võetakse aluseks:

* Analüüsist lähtuvalt õigesti ja asjakohaselt kasutatud oskuste arv "Nõuded detailsemalt"" loetelust (kuni 10 p)
* Jooniste vormistus (1 p)
* Teksti vormistus (2 p)
* Projekti sisukus (2 p)
* Boonuspunktid (2 p)


Boonuspunkte võib saada õppejõu äranägemise järgi järgmiste tingimuste olemasolul:

* Praktikumis käsitletud R-i käskude/pakettide/funktsioonide äärmiselt oskuslik kasutamine, 
* Praktikumis mitte käsitletud käskude/pakettide/funktsioonide laialdane kasutamine,
* Ilusasti vormistatud R-i kood koos ammendavate kommentaaridega.


**Projektitöö on arvestatud kui töö eest on saadud vähemalt 8 punkti.**

**Soovitus** Ärge võtke seda projekti kui aine eksamit vaid osa õppeprotsessist ehk kui miski kohe ei õnnestu siis uurige internetist või raamatutest juurde. Kirjutage kõik tekkinud probleemid üles ning tooge need näiteks R-i koodis kommentaarina välja. Proovime leida lahendused dokumendi alguses toodud aegadel.


## Korduma kippuvad vead

* eesmärgistamata töö/analüüs; 
* puudub pealkiri, autor;
* ebaühtlane või lohakas vormistus;
* inglise ja eesti keele segamini kasutamine;
* erineva vormindusega joonised (nt. üks joonis sinise taustaga, teine kollase taustaga; üks lihtne joonis ggplot2-ga, teine lihtne joonis aga baasgraafikaga);
* jooniste toimetamata jätmine (x-, y-telje nimed on võetud veerupealkirjadest, nt on ingliskeelsed);
* R väljundi kopeerimine-kleepimine analüüsifaili ilma toimetamata (nt `summary`);
* analüüsi seisukohast otstarbetute käskude/funktsioonide kasutamine (nt: `nimedkoos <- str_c(names(andmed),collapse = "_"); nimederaldi <- unlist(str_split(nimedkoos, "_"))` );
* lihtsate asjade lahendamine keerulisel moel (nt tsükli kasutamine olukorras, kus oleks saanud rakendada vektortehteid vms), eriti kui praktikumis on ühte võimalikku lahendust käsitletud;
* koodi väga halb vormindamine, liiga vähene kommenteeritud kood;
* vigane/mittetöötav kood.


**Näited**

Näited leiad [projektide lehelt](projektid)

