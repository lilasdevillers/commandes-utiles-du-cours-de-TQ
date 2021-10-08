library(summarytools)
library(tidyverse)
library(stringr)
library(tidyverse)
library(dplyr)
library(questionr)
library(skimr)
library(stargazer)
library(knitr)
library(glm2)
library(oddsratio)
library(blorr)
library(survey)
library(ggplot2)
library(descr)
library(clipr)

## Lecture de la base de donnée
EE20 <- read.csv("FD_csv_EEC20.csv", sep=";")
View(EE20)

# J'utilise ici les données de l'enquête-emploi 2020 et plus précisément la variable ACTOP
# qui s'intéresse au statut actif occupé. Ainsi la fréquence de 1 ici fait référence aux personnes
# actives occupées au sens du BIT. 


#################################################################################################################################################################
##################### test des commandes du cours - intervalles de confiance et test- utilisation de la variable ACTOP de l'EE2020 ##############################
#################################################################################################################################################################

## mesurer la fréquence
summarytools::freq(EE20$ACTOP_01)

#recodage variable actif occupé
EE20$ACTOP_01 <- ifelse(EE20$ACTOP == 1 , 1, 0) # remplcement des 2 par des 0, les 1 restent des 1
EE20 <- EE20 %>% mutate(across(where(is_character), as.factor)) #changement du mode des variables, elles deviennent toutes des facteurs ce qui est nécessaire pour certaines commandes

## moyenne pondérée
wtd.mean(EE20$ACTOP_01, weights = EE20$EXTRIAN)

## utilisation du package survey
test_bw <- svydesign(ids = ~1, data = EE20, weights = ~EE20$EXTRIAN) # configurer l'outil survey
svymean(~ACTOP_01, na.rm = TRUE, test_bw) # donne 
svymean(~ACTOP_01, na.rm = FALSE, test_bw)
## /!\ ajouter na.rm = TRUE pour enlever les NA et na.rm = FALSE pour les garder (en fait non ca ne marche pas de les garder, demander à la  prof pour la pertinence)

## mesurer la fréquence
summarytools::freq(EE20$ACTOP_01)

## intervalles de confiance
confint(svymean(~ACTOP_01, na.rm = TRUE, test_bw), level=0.99) # au seuil de 99%
confint(svymean(~ACTOP_01, na.rm = TRUE, test_bw), level=0.9) # au seuil de 90%

## tests statistiques
res <- svymean(~ACTOP_01, na.rm = TRUE, test_bw)
f.actop <- res[1]
SE.f.actop <- SE(res)
stat.student.actop <- (f.actop - 0.402 )/SE.f.actop
df <-length(EE20$ACTOP_01)-1
pval <- 2*(1-pt(abs(stat.student.actop),df))

# Si test d'hypo avec H0 = x et H1 : p =/= x alors on utilise la commande suivante: 
pval <- 2*(1-pt(abs(stat.student.actop),df))
# Si test d'hypo avec H0 = x et H1 : p > x alors on utilise la commande suivante: 
pval <- 1 - pt(stat.student.actop, df)
# Si test d'hypo avec H0 = x et H1 : p < x alors on utilise la commande suivante: 
pval <- pt(stat.student.actop, df)



