setwd("C:/Users/maxime_pc/R")

#setups####
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

#Acquisition####
tripAdvisorJanvier<-fread("tripadvisor_2015-01_FRA.csv")
distancePays<-fread("distances_entre_pays.csv")

sapply(tripAdvisorJanvier, function(x) length(unique(x)))

tripAdvisorJanvier <- subset(tripAdvisorJanvier, select = -c(idAuteur, nomPlace, idPlace, idPlace, titreReview, shape_gid, typeDetails, idAuteur, shape_gid, auteurLieu ) )
tripAdvisorJanvier <-subset(tripAdvisorJanvier, select = -c(idPlace, idAuteur, shape_gid, shape_name1,shape_name2,shape_name3,shape_name4,shape_name5 ) )

#On recupere la distance entre le pays de visite et le pays d'origin de la personne
tripAdvisorJanvier<-sqldf("select a.*,b.distance_km from tripAdvisorJanvier a 
                          left join distancePays b 
                          on a.shape_name0 = b.pays1_name0 and a.auteurPays = b.pays2_name0")

class(tripAdvisorJanvier$distance_km)
#tripAdvisorJanvier <- transform(tripAdvisorJanvier, distance_km=as.character(distance_km))
class(tripAdvisorJanvier$distance_km)

#On transforme la colonne distance_km en palier
tripAdvisorJanvier$distance<-ifelse(is.na(tripAdvisorJanvier$distance_km),"UNKNOWN",
                                ifelse(tripAdvisorJanvier$distance_km == -1,"-1",
                                ifelse(tripAdvisorJanvier$distance_km == 0,"0",
                                ifelse(tripAdvisorJanvier$distance_km > 0 & tripAdvisorJanvier$distance_km < 500,"0-500",
                                ifelse(tripAdvisorJanvier$distance_km>=500 & tripAdvisorJanvier$distance_km<1000,"500-1000",
                                ifelse(tripAdvisorJanvier$distance_km>=1000 & tripAdvisorJanvier$distance_km<3000,"1000-3000",
                                ifelse(tripAdvisorJanvier$distance_km>=3000 & tripAdvisorJanvier$distance_km<8000,"3000-8000",
                                ifelse(tripAdvisorJanvier$distance_km>=8000 & !is.na(tripAdvisorJanvier$distance_km),"8000+",""
                                ))))))))

tripAdvisorJanvier <- subset(tripAdvisorJanvier, select = -c(distance_km))

#On convertit les champs en date pour par la suite récupérer le delta
tripAdvisorJanvier$date_visit<-as.Date(tripAdvisorJanvier$date_visit,'%Y-%m-%d')
tripAdvisorJanvier$date_review<-as.Date(tripAdvisorJanvier$date_review,'%Y-%m-%d')

tripAdvisorJanvier<-sqldf("select *,round((date_review-date_visit)/30) as temps_review from tripAdvisorJanvier")

tripAdvisorJanvier <- subset(tripAdvisorJanvier, select = -c(date_review,date_visit))

tripAdvisorJanvier$temps_review<-ifelse(tripAdvisorJanvier$temps_review>=0 & tripAdvisorJanvier$temps_review < 3 ,"0-3",
                                 ifelse(tripAdvisorJanvier$temps_review>=3 & tripAdvisorJanvier$temps_review<6,"3-6",
                                 ifelse(tripAdvisorJanvier$temps_review>=6 & tripAdvisorJanvier$temps_review<9,"6-9",
                                 ifelse(tripAdvisorJanvier$temps_review>=9 & tripAdvisorJanvier$temps_review<12,"9-12",
                                 ifelse(tripAdvisorJanvier$temps_review>=12 & !is.na(tripAdvisorJanvier$temps_review),"12+",""
                                 )))))
#tripAdvisorJanvier[['date_review']] <- substr(tripAdvisorJanvier[,'date_review'], 1, nchar(tripAdvisorJanvier[,'date_review'])-3)

#
tripAdvisorJanvier <- subset(tripAdvisorJanvier, select = -c(langReview, auteurPays))
nrow(tripAdvisorJanvier)

freq <- function(dt, x, y) {
  test<-dt %>% group_by(dt[,x],dt[,y]) %>% summarise(Freq = n())
  test$Freq<-((test$Freq/nrow(dt))*100)
  test<-test[with(test,order(dt[,x],decreasing = TRUE)),]
  return(test)
}

freq(tripAdvisorJanvier, "note","distance")


sapply(tripAdvisorJanvier, function(x) length(unique(x)))
tripAdvisorJanvier
distancePays
write.csv(tripAdvisorJanvier, file = "testResultat.csv")
