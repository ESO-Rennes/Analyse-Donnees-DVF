---
title: "Script 2"
author: "Boris Mericskay et Florent Demoraes"
date: "12/11/2021"
output: html_document
---


---

INDICATEURS GENERIQUES ET REPRESENTATIONS GRAPHIQUES DES DONNEES DVF
---
Ce deuxième script a comme objectif de produire et de représenter graphiquement sous diverses formes une série d'indicateurs génériques sur le marché immobilier résidentiel

## Préparation du projet

### Définition de l'environnement de travail

On définit ici le dossier qui centralise les données et où les différents jeux de données seront exportés

```{r setup, include=FALSE} 
knitr::opts_knit$set(root.dir = 'C:/DVF')
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

```
### Chargement des packages R nécessaires

Seul le package `tidyverse` dédié à la manipulation de données est nécessaire ici.

```{r}
library(tidyverse)
```


### Import du jeu de données brut
```{r}
DVF <- read.csv("DATA/DVF_brut.csv", encoding="UTF-8", stringsAsFactors=FALSE)
```

--- 


## Tableau récapitulatif des indicateurs génériques
```{r}
DVFOK <- read.csv("Exports/DVFOK.csv", encoding="UTF-8", stringsAsFactors=FALSE) # si nécessaire
recap <- DVFOK %>% group_by(type) %>% summarise(tot = n (), prixmed = median(prix), prixmoy = mean(prix), surfmed = median(surface), surfmoy = mean(surface), prixm2med = median(prixm2), prixm2moy = mean(prixm2))
recap <- recap %>% mutate(part = (tot/sum(tot)*100))
print(recap)
```

--- 

## Visualisation de données avec ggplot2

### Récapitulatif des mutations par type et par département
```{r}
recapdep <- DVFOK %>% group_by(Dep, type) %>% summarise(nb= n())

ggplot(recapdep, aes(x=Dep, y=nb, fill=type)) +
  geom_bar(position = 'stack', stat='identity') +
  xlab("") +  ylab("Nombre de mutations") +
  geom_text(aes(label=nb), color="black", size=4, position = position_stack(vjust = 0.5))+
  theme_bw()
```

### Recapitulatif des mutations par type et par TypoINSEE

```{r}
recapinsee <- DVFOK %>% group_by(Typo_INSEE, type) %>% summarise(nb= n())

ggplot(recapinsee, aes(x=Typo_INSEE, y=nb, fill=type)) +
  geom_bar(position = 'stack', stat='identity') +
  geom_text(aes(label=nb), color="black", size=4, position = position_stack(vjust = 0.5))+
  xlab("") +  ylab("Nombre de mutations") +
  theme_bw()
```

### BoxPlot des prix par type de bien, type de commune et département

```{r}
DVFOK$Typo_INSEE <- factor(DVFOK$Typo_INSEE ,levels = c("Espace rural", "Couronne périurbaine", "Pôle urbain"))

ggplot(data = DVFOK, aes(x = Typo_INSEE, y = prix, color = type)) +
  geom_boxplot(notchwidth = 0.5) +
  ylim(0,200000)+
  facet_grid(type~Dep) +
  labs(x= "Type de commune", y= "Prix") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold") ,axis.text.x = element_text(angle = 45, hjust = 1))
```

### Histogramme des prix au m2 par département

```{r}
RecapPrixDep <- DVFOK %>% group_by(Dep, type) %>% mutate(moydeptype = mean(prixm2))
RecapPrixDep$ moydeptype <- round(RecapPrixDep$moydeptype)

ggplot(RecapPrixDep, aes(x=prixm2, fill= type)) +
  geom_histogram(position = "identity", color = "white") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  geom_vline(aes(xintercept=moydeptype),color="black", linetype="longdash", size=0.75) +
  xlab("Prix au m2") +  ylab("Effectifs") +
  geom_text(y = 5500, aes(x = moydeptype, label = moydeptype), size = 3, hjust = -.3) +
  facet_grid(type~Dep)
```

### Histogramme des prix m2 par TypoINSEE

```{r}
RecapPrixTypoINSEE <- DVFOK %>% group_by(Typo_INSEE, type) %>% mutate(moyeinseetype = mean(prixm2))
RecapPrixTypoINSEE$ moyeinseetype <- round(RecapPrixTypoINSEE$moyeinseetype)

ggplot(RecapPrixTypoINSEE, aes(x=prixm2, fill= type)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
    geom_histogram(position = "identity", color = "white") +
  geom_vline(aes(xintercept=moyeinseetype),color="black", linetype="longdash", size=0.75) +
  xlab("Prix au m2") +  ylab("Effectifs") +
  geom_text(y = 5000, aes(x = moyeinseetype, label = moyeinseetype), size = 3, hjust = -.1) +
  facet_grid(type~Typo_INSEE)
```

### Evolution du nombre de mutations par années et par département

```{r}

evoldvfdep <- DVFOK %>% group_by(annee, Dep, type) %>% summarise(nb = n())
View(evoldvfdep)

ggplot(data=evoldvfdep, aes(x=annee, y=nb, fill=type)) +
  geom_bar(stat="identity") +
  xlab("Année") +  ylab("Nb de mutations") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  facet_wrap(~Dep, nrow = 1)
```

### Evolution du nombre de mutations par années et par type de commune

```{r}
evoldvfINSEE <- DVFOK %>% group_by(annee, Typo_INSEE, type) %>% summarise(nb = n())

ggplot(data=evoldvfINSEE, aes(x=annee, y=nb, fill=type)) +
  geom_bar(stat="identity") +
  xlab("Année") +  ylab("Nb de mutations") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  facet_wrap(~Typo_INSEE, nrow = 1)

```

### Evolution des prix au m2 par département

```{r}
DVFOK$annee <- as.numeric(DVFOK$annee)

EvolPrixDep <- DVFOK %>% group_by(annee, Dep, type) %>% summarise(prix_m2 = mean(prixm2))

ggplot(data=EvolPrixDep, aes(x=annee, y=prix_m2, color=type)) +
  geom_line(stat="identity", size= 1)+
  geom_point(stat="identity", size= 2)+
  scale_y_continuous(breaks=c(1500, 1700,1900, 2100, 2300, 2500, 2700)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  xlab("") +  ylab("Prix moyen au m²") + 
  facet_wrap(~Dep, nrow = 1) 
```


### Evolution des prix au m2 par type de comumune INSEE

```{r}
EvolPrixINSEE <- DVFOK %>% group_by(annee, Typo_INSEE, type) %>% summarise(prix_m2 = mean(prixm2))

ggplot(data=EvolPrixINSEE, aes(x=annee, y=prix_m2, color=type)) +
  geom_line(stat="identity", size= 1)+
  geom_point(stat="identity", size= 2)+
  scale_y_continuous(breaks=c(1500, 1700,1900, 2100, 2300, 2500, 2700)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  xlab("") +  ylab("Prix moyen au m²") + 
  facet_wrap(~Typo_INSEE, nrow = 1)
```



