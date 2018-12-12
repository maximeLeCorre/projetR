setwd("C:/Users/maxime_pc/R")

#setups####
library(igraph)
require(sqldf)
require(data.table)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(pROC)
require(randomForest)
require(dismo)
require(doParallel)
require(dplyr)
require(tidyr)
require(igraph)

#Acquisition####
tripAdvisor<-fread("tripadvisor_2015-01_FRA.csv")
distancePays<-fread("distances_entre_pays.csv")

sapply(tripAdvisor, function(x) length(unique(x)))

tripAdvisor <- subset(tripAdvisor, select = -c(idAuteur, nomPlace, idPlace, idPlace, titreReview, shape_gid, typeDetails, idAuteur, shape_gid, auteurLieu ) )
tripAdvisor <-subset(tripAdvisor, select = -c(idPlace, idAuteur, shape_gid, shape_name1,shape_name2,shape_name3,shape_name4,shape_name5 ) )

#On recupere la distance entre le pays de visite et le pays d'origin de la personne
tripAdvisor<-sqldf("select a.*,b.distance_km from tripAdvisor a 
                          left join distancePays b 
                          on a.shape_name0 = b.pays1_name0 and a.auteurPays = b.pays2_name0")

class(tripAdvisor$distance_km)
#tripAdvisor <- transform(tripAdvisor, distance_km=as.character(distance_km))
class(tripAdvisor$distance_km)

#On transforme la colonne distance_km en palier
tripAdvisor$distance<-ifelse(is.na(tripAdvisor$distance_km),"pays_inconnu",
                                ifelse(tripAdvisor$distance_km == -1,"resident",
                                ifelse(tripAdvisor$distance_km == 0,"pays_frontalier",
                                ifelse(tripAdvisor$distance_km > 0 & tripAdvisor$distance_km < 500,"0-500 km",
                                ifelse(tripAdvisor$distance_km>=500 & tripAdvisor$distance_km<1000,"500-1000 km",
                                ifelse(tripAdvisor$distance_km>=1000 & tripAdvisor$distance_km<3000,"1000-3000 km",
                                ifelse(tripAdvisor$distance_km>=3000 & tripAdvisor$distance_km<8000,"3000-8000 km",
                                ifelse(tripAdvisor$distance_km>=8000 & !is.na(tripAdvisor$distance_km),"8000+ km",""
                                ))))))))

tripAdvisor <- subset(tripAdvisor, select = -c(distance_km))

#On convertit les champs en date pour par la suite récupérer le delta
tripAdvisor$date_visit<-as.Date(tripAdvisor$date_visit,'%Y-%m-%d')
tripAdvisor$date_review<-as.Date(tripAdvisor$date_review,'%Y-%m-%d')


tripAdvisor<-tripAdvisor[!(tripAdvisor$auteurAge==""),]
tripAdvisor<-tripAdvisor[!(tripAdvisor$auteurGenre==""),]
#traitement du sexe et de l'age
#tripAdvisor$auteurGenre<-ifelse(tripAdvisor$auteurGenre=="","unknowSex",tripAdvisor$auteurGenre)
#tripAdvisor$auteurAge<-ifelse(tripAdvisor$auteurAge=="","unknowAge",tripAdvisor$auteurAge)

tripAdvisor<-sqldf("select *,round((date_review-date_visit)/30) as temps_review from tripAdvisor")

tripAdvisor <- subset(tripAdvisor, select = -c(date_review,date_visit))

tripAdvisor$temps_review<-ifelse(tripAdvisor$temps_review>=0 & tripAdvisor$temps_review < 3 ,"0-3",
                                 ifelse(tripAdvisor$temps_review>=3 & tripAdvisor$temps_review<6,"3-6",
                                 ifelse(tripAdvisor$temps_review>=6 & tripAdvisor$temps_review<9,"6-9",
                                 ifelse(tripAdvisor$temps_review>=9 & tripAdvisor$temps_review<12,"9-12",
                                 ifelse(tripAdvisor$temps_review>=12 & !is.na(tripAdvisor$temps_review),"12+",""
                                 )))))
#tripAdvisor[['date_review']] <- substr(tripAdvisor[,'date_review'], 1, nchar(tripAdvisor[,'date_review'])-3)

#
tripAdvisor <- subset(tripAdvisor, select = -c(langReview, auteurPays, shape_name0, shape_iso, temps_review))
nrow(tripAdvisor)


#Computation####

resultaGraphAll<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("node1", "node2", "weight"))

freq <- function(dt, x, y) {
  print(paste("select ",x,",",y,",(count(*)*100/(select count(*) from tripAdvisor as b where b.",x," = a.",x,")) as pourcentage from tripAdvisor as a
      group by ",x,",",y," order by ", x))
  test<-sqldf(paste("select ",x,",",y,",(count(*)*100/(select count(*) from tripAdvisor as b where b.",x," = a.",x,")) as pourcentage from tripAdvisor as a
      group by ",x,",",y," order by ",x ,sep=""))
  return(test)
}

for(col1 in colnames(tripAdvisor)){
  for(col2 in colnames(tripAdvisor)){
    if(col1 != col2){
      temp<-freq(tripAdvisor,col1,col2)
      for(i in 1:nrow(temp)) {
        row <- temp[i,]
        if((row[3] > 50 || row[3] < 20) && row[3]>5){
          if(sqldf(paste("select (count(*)) from tripAdvisor where ", col1, "='",row[1],"' and ",col2," = '",row[2],"'",sep = ""))>nrow(tripAdvisor)/30){
          names(row)<-c("node1", "node2", "weight")
          resultaGraphAll<-rbind(resultaGraphAll,row)
          }
        }
      }
    }
  }
}

tripAdvisor

freq(tripAdvisor,"distance","note")

resultaGraphAll
write.csv(resultaGraphAll, file = "testGraph.csv" , row.names = F)

xlist<-read.table("testGraph.csv", sep =",", header = T)
xlist <-graph.data.frame(xlist)

E(xlist)$color <- ifelse(E(xlist)$weight < 20, "black", "green")

plot(xlist, layout=layout.auto,edge.curved=.1)


#GraphAmeliore
#Construire une fonction qui construit un graph centre sur la valeur d'une qu'on lui passe en paramètre

freqAmel <- function(dt, x, values, columnTest) {
  
  

  return("test")
}

print(paste(as.character(colnames(tripAdvisor)), collapse=", "))

test <- data.frame(paste(tripAdvisor$note, tripAdvisor$distance))

test

sqldf("select ")
