#Refection des graphs pour le projet

#lire le fichier csv 
path <-""
filename <- paste(path, "Summary_nombre_ecole.csv",sep="")
perci.raw <- read.csv(filename)

#read helper functions 
source('per_ci_helper.R')

#voir un peu le contenu 
head(perci.raw)
tail(perci.raw)
dim(perci.raw)
str(perci.raw)
names(perci.raw)

#si on veut tout voir 
View(perci.raw)

#changer le data.frame en local:facilite la vue et autres manipualtion
perci.local <- tbl_df(perci.raw)
head(perci.local)

#ajouter des librairie pour faciliter la manipulation 
library(dplyr)

#decouvrir le nombre d'ecoles
perci.nbr.ecole <- 
    perci.local %>%
    #select(-Effectifs)%>%
    group_by(Niveau, Annee) %>%
    #summarise(subtotal = sum(Nombre.d.ecole,na.rm = T))
    summarise_each(funs(sum(.,na.rm=T)),Nombre.d.ecole,Effectifs)
perci.nbr.ecole

#graph cumule pour le prescolaire
library(ggplot2)

#variables reutilisables 
titre.nb.ecole <-"Total cumule de l'Evolution du Nombre d'Etablissements dans le "
titre.effectif <-"Total cumule de l'Evolution des Effectifs dans le "

#pour le prescolaire
#graphs pour le nombres d'ecoles
cumul.niveau.graph_nombre(perci.nbr.ecole,titre.nb.ecole,"Prescolaire")
#graph pour les effectifs
cumul.niveau.graph_effectif(perci.nbr.ecole,titre.nb.ecole,"Prescolaire")

#pour le primaire 
#graphs pour le nombres d'ecoles
cumul.niveau.graph_nombre(perci.nbr.ecole,titre.nb.ecole,"Primaire")
#graph pour les effectifs
cumul.niveau.graph_effectif(perci.nbr.ecole,titre.nb.ecole,"Primaire")

#pour le secondaire general
#graphs pour le nombres d'ecoles
cumul.niveau.graph_nombre(perci.nbr.ecole,titre.nb.ecole,"Secondaire  General")
#graph pour les effectifs
cumul.niveau.graph_effectif(perci.nbr.ecole,titre.nb.ecole,"Secondaire  General")

#pour le secondaire technique
#graphs pour le nombres d'ecoles
cumul.niveau.graph_nombre(perci.nbr.ecole,titre.nb.ecole,"Secondaire  Technique")
#graph pour les effectifs
cumul.niveau.graph_effectif(perci.nbr.ecole,titre.nb.ecole,"Secondaire  General")

#graphics en fonction des effectifs et du satatus 
titre.nb.ecole.stat <-"Evolution du Nombre d'Etablissements dans le"
titre.effectif.stat <-"Evolution des Effectifs dans le"
perci.statu.ecole <- 
    perci.local %>%
    group_by(Annee,Type,Niveau) %>%
    summarise_each(funs(sum(.,na.rm=T)),Nombre.d.ecole,Effectifs)
perci.statu.ecole

#prescolaire
niveau.graph.nombre.ecole(perci.statu.ecole,titre.nb.ecole.stat, "Prescolaire")
niveau.graph.effectif(perci.statu.ecole, titre.effectif.stat,"Prescolaire")

#primaire
niveau.graph.nombre.ecole(perci.statu.ecole,titre.nb.ecole.stat, "Primaire")
niveau.graph.effectif(perci.statu.ecole,titre.effectif.stat, "Primaire")

#exclus communautaire de facon volontaire
perci.statu.ecole.nocommu <-
    filter(perci.statu.ecole,!grepl("Commnunautaire", Type))

#Secondaire general 
niveau.graph.nombre.ecole(perci.statu.ecole.nocommu, titre.nb.ecole.stat,"Secondaire  General")
niveau.graph.effectif(perci.statu.ecole.nocommu,titre.effectif.stat,"Secondaire  General")

#Secondaire Technique
niveau.graph.nombre.ecole(perci.statu.ecole.nocommu,titre.nb.ecole.stat, "Secondaire  Technique")
niveau.graph.effectif(perci.statu.ecole.nocommu,titre.effectif.stat,"Secondaire  Technique")
 
    

