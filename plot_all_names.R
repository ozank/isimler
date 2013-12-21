#Plot all names
library(ggplot2)
source("name_functions.R")

###MULTIPLE PEOPLE####
###MALE####

top_names<-rownames(table(names$male))[as.numeric(table(names$male))>8]
#top_names<-c("YUNUS","EGE","FURKAN","YUSUF")  
subset_data<-names[which(names$male %in% top_names),]

p<- ggplot(subset_data,aes(x=year,y=rank)) + 
  geom_line(aes(group=male, colour =male_rank_change), alpha = 1, size = 1) + 
  ylab("SIRALAMA") + xlab("YIL") +
  scale_x_continuous(expand = c(0,0), breaks=c(1960,1980,2000,2012)) + scale_y_reverse(expand=c(0,1),breaks=c(1,20,40,60,80,100)) +
  facet_wrap(~male, ncol =6) +
  ggtitle("POPÜLER ERKEK İSİMLERİ 1950-2012") + 
  scale_colour_gradient(low="red", high="green1", name = "Sıralama\nDeğişimi") 
#Add footnote
library(gridExtra)
p <- arrangeGrob(p, sub = textGrob("Hazırlayan: Ozan K.   http://github.com/ozank/isimler     Hiçbir haklı saklı değildir. ", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 10, col="gray")))

#p
ggsave(file="./grafikler/erkek_isim_trend.png", plot=p, width=12, height=30, limitsize=FALSE)

cairo_pdf("./grafikler/erkek_isim_trend.pdf", width=12, height=30) # open an appropriate graphics device
print(p)
dev.off()

p


#FEMALE
top_names<-rownames(table(names$female))[as.numeric(table(names$female))>9]
subset_data<-names[which(names$female %in% top_names),]

p<- ggplot(subset_data,aes(x=year,y=rank)) + 
  geom_line(aes(group=female, colour = female_rank_change), alpha = 1, size = 1) + 
  ylab("SIRALAMA") + xlab("YIL") +
  scale_x_continuous(expand = c(0,0), breaks=c(1960,1980,2000,2012)) + scale_y_reverse(expand=c(0,1),breaks=c(1,20,40,60,80,100)) +
  facet_wrap(~female, ncol =6) +
  ggtitle("POPÜLER KADIN İSİMLERİ 1950-2012") + 
  scale_colour_gradient(low="red", high="green1", name = "Sıralama\nDeğişimi") 
#Add footnote
library(gridExtra)
p <- arrangeGrob(p, sub = textGrob("Hazırlayan: Ozan K.   http://github.com/ozank/isimler     Hiçbir haklı saklı değildir. ", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 10, col="gray")))

ggsave(file="./grafikler/kadin_isim_trend.png", plot=p, width=12, height=30, limitsize=FALSE)

cairo_pdf("./grafikler/kadin_isim_trend.pdf", width=12, height=30) # open an appropriate graphics device
print(p)
dev.off()

p
