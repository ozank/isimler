#Get change in trends
Get_popularity_difference<-function(single_name){
  rank_list<-names$rank[which(names$female==single_name | names$male==single_name)]
  rank_list_difference<-head(rank_list,1)-tail(rank_list,1)
  rank_list_difference
}

#load data
names<-read.csv("veri/populer_isimler.csv", header = TRUE)
names[,4]<-as.character(names$female)
names[,3]<-as.character(names$male)

#Calculate trend (final- first rank)
names["male_rank_change"] <- sapply(names$male,Get_popularity_difference, USE.NAMES=FALSE)
names["female_rank_change"] <- sapply(names$female,Get_popularity_difference, USE.NAMES=FALSE)