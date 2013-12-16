#From https://github.com/jofusa/ssa-baby-names

library(ggplot2)
source("name_functions.R")

###Single Female Name#####
nm<-"DERYA" #Enter Female Name

p<- ggplot(names,aes(x=year,y=rank))+ 
    ylim(max(names$rank),min(names$rank)) + # Flip the Y axis
    geom_line(data = names[which(names$female == nm),], aes(group=female, colour = female), alpha = 1, size = 1) + 
    ylab("Sıralama") + xlab("Yıl") + ggtitle(nm) + 
    theme(legend.position = "none") 
p

####Single Male Name####
nm<-c("AYAZ")
p<- ggplot(names,aes(x=year,y=rank)) + 
    ylim(max(names$rank),min(names$rank)) + 
    geom_line(data = names[which(names$male == nm),], aes(group=male),colour = "Blue", alpha = 1) + 
    ylab("Sıralama") + xlab("Yıl") + ggtitle(nm) + 
    theme(legend.position = "none") 
p

###MULTIPLE PEOPLE####
###MALE####

top_names<-rownames(table(names$male))[as.numeric(table(names$male))>8]
#top_names<-c("YUNUS","EGE","FURKAN","YUSUF")  
subset_data<-names[which(names$male %in% top_names),]


p<- ggplot(subset_data,aes(x=year,y=rank)) + 
    geom_line(aes(group=male, colour =male_rank_change), alpha = 1, size = 1) + 
    ylab("Sıralama") + xlab("Yıl") +
    scale_x_continuous(expand = c(0,0), breaks=c(1960,1980,2000,2012)) + scale_y_reverse(expand=c(0,1),breaks=c(1,20,40,60,80,100)) +
    facet_wrap(~male, ncol =6) +
    scale_colour_gradient(low="red", high="green1", name = "Sıralama\nDeğişimi") 
#p
ggsave(file="./grafikler/erkek_isim_trend.svg", plot=p, width=12, height=30, limitsize=FALSE)

#FEMALE
top_names<-rownames(table(names$female))[as.numeric(table(names$female))>9]
subset_data<-names[which(names$female %in% top_names),]

p<- ggplot(subset_data,aes(x=year,y=rank)) + 
    geom_line(aes(group=female, colour = female_rank_change), alpha = 1, size = 1) + 
    ylab("Sıralama") + xlab("Yıl") +
    scale_x_continuous(expand = c(0,0), breaks=c(1960,1980,2000,2012)) + scale_y_reverse(expand=c(0,1),breaks=c(1,20,40,60,80,100)) +
    facet_wrap(~female, ncol =6) +
    scale_colour_gradient(low="red", high="green1", name = "Sıralama\nDeğişimi") 
#p
ggsave(file="./grafikler/kadin_isim_trend.svg", plot=p, width=12, height=30, limitsize=FALSE)


####FEMALE#####
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)+geom_line(aes(group=Female),colour="#431600",alpha=0.1)+ opts(title = "Female Baby Name Popularity Since 1950")+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p

nm<-"Trinity"
p<-ggplot(names,aes(x=Year,y=Rank)) 
p<- p + ylim(max(names$Rank),min(names$Rank)) 
p<- p + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)
p<- p + opts(title = "People Liked the Matrix Way Too Much")
matrix.label<-data.frame(Year = 1985 , Rank = 220, Text = "Matrix Released - 1999") # create the custom on graphic text label
p <- p +  geom_rect(aes(xmin = 1998 , xmax = 2000 , ymin = 1000 , ymax = 1 ),fill = "Green", alpha = .002)
p <- p + geom_text(data = matrix.label, aes(label = Text))
p


rm(p)

###Calculate Greatest Change over Time
#This would be more elequently done with Hadley Wickam's ddply. The lapply/do.call("rbind") combo is brillinatly useful and for simple things I use
name.min.max<-function(nm){
  data.frame(
    name = nm,
    min = min(names[which(names$female == nm),2]),
    max = max(names[which(names$female == nm),2]),
    dif = max(names[which(names$female == nm),2]) - min(names[which(names$female == nm),2])
  )
}
name.list<-unique(names$female)
out<-lapply(X = as.list(name.list), FUN = name.min.max)
out <- do.call("rbind", out)
female.dif<-out[order(-out$dif),]
#Top Female Names with greatest change overtime # Need to seperate the winners and losser just plots largest difference
nm<-as.character(female.dif[1:10,1])

#Plot
p<-ggplot(names,aes(x=year,y=rank)) 
p <- p + ylim(max(names$rank),min(names$rank)) 
p <- p + geom_line(data = names[which(names$female %in% nm),], aes(group=female, colour = female), alpha = 1, size = 1)
p <- p + opts(title = "Top Movers")
p <- p + facet_wrap(~female)
p
`Delt.Absolute` <-
  function(x1,x2=NULL,k=0,type=c('arithmetic','log'))
  {
    x1 <- try.xts(x1, error=FALSE)
    type <- match.arg(type[1],c('log','arithmetic', 'absolute'))
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
      x2 <- x1 #copy for same symbol deltas
      if(length(k) < 2) {
        k <- max(1,k)
      }
    }
    dim(x2) <- NULL  # allow for multiple k matrix math to happen
    if(type=='log') {
      xx <- lapply(k, function(K.) {
        log(unclass(x2)/Lag(x1,K.))
      })
    } else if (type=='absolute') {
      xx <- lapply(k, function(K.) {
        unclass(x2) - Lag(x1,K.)
      })
    } else {
      xx <- lapply(k, function(K.) {
        unclass(x2)/Lag(x1,K.)-1
      })
      
    }
    xx <- do.call("cbind", xx)
    colnames(xx) <- paste("Delt",k,type,sep=".")
    reclass(xx,x1)
  }

female.names<-names[,c(1,4,2)]
female.names<-female.names[order(female.names$Female, female.names$Year),]
female.names$delta <- Delt.Absolute(female.names$Rank,k=1, type = "absolute")
#female.names$delta3 <- Delt.Absolute(female.names$Rank)

female.names[20:30,]


female.names[c(TRUE, female.names$Female[-1] != female.names$Female[-length(female.names$Female)]), 4] <- NA
female.names.winners<-female.names[order(female.names[4]),]
female.names.losers<-female.names[order(-female.names[4]),]

female.names.winners<-female.names[order(female.names[4]),]    
top.female.names<-unique(female.names.winners[1:12,2])
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female %in% top.female.names),], aes(group=Female, colour = Female), alpha = 1, size = 1)+ opts(title = "Female Baby Name Popularity Since 1950")+ opts(axis.text.x=theme_text(angle=-70),hjust=0) + facet_wrap(~ Female)
p



#Came across this by using the same fast accedency script as above, but using fastest decline. This Is what I found
#Poor, Poor Hillary

nm<-"Hillary"
p<-ggplot(names[which(names$Female %in% nm),],aes(x=Year,y=Rank)) 
p<- p + ylim(max(names$Rank),min(names$Rank)) 
p<- p + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)
p<- p + opts(title = "Poor, Poor Hillary: Popularity of the Female Name 'Hillary' \n But we already that the Clinton Prseidency was better to certain women", size = 30)
matrix.label<-data.frame(Year = 1996 , Rank = 100, Text = "Clinton \n Presidency") # create the custom on graphic text label, kind of a hack
p <- p +  geom_rect(aes(xmin = 1992 , xmax = 2001 , ymin = 1000 , ymax = 1 ),fill = "Blue", alpha = .002) 
p <- p +  geom_rect(aes(xmin = 2007 , xmax = 2008 , ymin = 1000 , ymax = 1 ),fill = "Green", alpha = .002) 
matrix.label2<-data.frame(Year = 2007 , Rank = 100, Text = "Hillary's \n Presidential \n Run")

# 1993 ->2001 the years of the Clinton Presdidency but 1992 was when he won the election so that is our starting point
p <- p + geom_text(data = matrix.label, aes(label = Text, size =20))
p <- p + geom_text(data = matrix.label2, aes(label = Text, size = 20))
print(p)







###Loop through all Male names ### Might give you a Seizure
name.list<-unique(names$Male)
for (i in 1:length(name.list)) {
  print(name.list[i])
  p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == name.list[i]),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.2)+ opts(title = name.list[i])+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
  print(p)
  Sys.sleep(.2)
}






