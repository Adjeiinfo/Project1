#helper functions 
#Ensemble de functions utilisee par le main pour effectuer 
#des tache de routine. 

#function pour trouver extraire les effectifs pour chaque niveau 
cumul.niveau <-function(data,niveau)
{
    prep.niveau <- 
        data %>%
        # select(Niveau:subtotal)%>%
        filter (Niveau == niveau)
    prep.niveau
}

#function pour faire le graph de chaque niveau: Nombre d ecole
cumul.niveau.graph_nombre <- function(data,gtitle="test ",niveau,gcolor="blue"){
    
    target <-names(data[1])
    data <- cumul.niveau(data, niveau)
    ggplot(data,
           aes(x = Annee,
               y = Nombre.d.ecole))+
        geom_bar(stat="identity",fill=gcolor)+
        geom_text(aes(y=Nombre.d.ecole,ymax=Nombre.d.ecole +1,label=Nombre.d.ecole))+
        ggtitle(paste(gtitle,niveau,sep=" "))
}

#function pour faire le graph de chaque niveau en fonction du status: Nombre d ecole 
cumul.niveau.graph_nombre_status <- function(data,gtitle="test ",gcolor="blue"){
    target <-names(data[1])
    print(data)
    ggplot(data,
           aes(x = Annee,
               y = Nombre.d.ecole))+
        geom_density(stat="identity",fill=gcolor)+
        geom_text(aes(y=Nombre.d.ecole,ymax=Nombre.d.ecole +1,label=Nombre.d.ecole))+
        ggtitle(paste(gtitle,niveau,sep=" "))
}

#function pour faire le graph de chaque niveau: Effectifs
cumul.niveau.graph_effectif <- function(data,gtitle="test ",niveau,gcolor="green"){
    data <- cumul.niveau(data, niveau)
    target <-names(data[1])
    print(data)
    ggplot(data,
           aes(x = Annee,
               y = Effectifs))+
        geom_bar(stat="identity",fill=gcolor)+
        geom_text(aes(y=Effectifs,ymax=Effectifs +1,label=Effectifs))+
        ggtitle(paste(gtitle,niveau,sep=" "))
}

#plot de l'evolution des effectifs en fonction du status
niveau.graph.effectif <- function(data, gtitle,niveau="Prescolaire" )
{
    print (data)
    perci.statu.ecole.presc <- 
        data %>%
        filter(Niveau == niveau)
    perci.statu.ecole.presc  
    ggplot (perci.statu.ecole.presc, aes(x = Annee, y = Effectifs))+
        geom_line(aes(colour=Type, group=Type))+
        geom_point(aes(colour=Type), size= 4)+
        geom_text(aes(y=Effectifs,ymax=Effectifs +1,label=Effectifs))+
        ggtitle(paste(gtitle,niveau,sep=" "))
    
}

#plot de l'evolution du nombre d'etablissements en fonction du status
niveau.graph.nombre.ecole <- function(data, gtitle, niveau="Prescolaire" )
{
    print (data)
    perci.statu.ecole.presc <- 
        data %>%
        filter(Niveau == niveau)
    perci.statu.ecole.presc  
    ggplot (perci.statu.ecole.presc, aes(x = Annee, y = Nombre.d.ecole))+
        geom_line(aes(colour=Type, group=Type))+
        geom_point(aes(colour=Type), size= 4)+
        geom_text(aes(y=Nombre.d.ecole,ymax=Nombre.d.ecole +1,label=Nombre.d.ecole))+
        ggtitle(paste(gtitle,niveau,sep=" "))   
}