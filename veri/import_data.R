yil<-c(1950, 1960, 1970, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,2014,2015,2016,2017,2018)

erkek<-read.csv("veri/erkek.csv",header=TRUE)
kadin<-read.csv("veri/kadin.csv",header=TRUE)
popularity <- data.frame(year= integer(0), rank= integer(0), male= character(0), female= character(0))

Get_popular_names<-function(i){
  
subset_erkek<-erkek[,c(1,i+1)]
ordered_erkek<-subset_erkek[ order(subset_erkek[,2]), ]

subset_kadin<-kadin[,c(1,i+1)]
ordered_kadin<-subset_kadin[ order(subset_kadin[,2]), ]
year_popularity<-data.frame(year=yil[i],rank=1:100,male=ordered_erkek[1:100,1],female=ordered_kadin[1:100,1])

#popularity<-rbind(popularity,year_popularity)
}

for (i in 1:length(yil) ) {
  popularity<-rbind(popularity,Get_popular_names(i))
}

#Write to csv
write.csv(popularity, "veri/populer_isimler.csv", row.names=FALSE)
