library(reshape2)
library(ggplot2)
library(gridExtra)

df1 = read.csv("C:\\Users\\velavarthy\\Desktop\\study8\\data_analysis\\heavy.csv", header = TRUE)
df2 = read.csv("C:\\Users\\velavarthy\\Desktop\\study8\\data_analysis\\black.csv", header = TRUE)
df3 = read.csv("C:\\Users\\velavarthy\\Desktop\\study8\\data_analysis\\death.csv", header = TRUE)
myStopwords<-c("a","is", "to", "by", "i", "the", "in", "of", "for", "de", "from", "my", "pa", "no", "and")
pattern <- paste0("\\b(", paste0(myStopwords, collapse="|"), ")\\b")

#heavy metal
heavydat<-tolower(paste(df1$Band,collapse=" "))
heavydat1<-gsub(pattern, "", heavydat)
words_heavy <- strsplit(heavydat1, " ")

final.list.heavy <- unlist(words_heavy)
not.blanks <- which(final.list.heavy != "")
final.list.heavy <- final.list.heavy[not.blanks]

dat.heavy<-sort(table(final.list.heavy),decreasing=TRUE)[1:15]
dat.heavy<-melt(dat.heavy)
dat.heavy <- transform(dat.heavy, final.list.heavy = reorder(final.list.heavy, value))
p1<-ggplot(dat.heavy, aes(x = final.list.heavy, y = value,)) + geom_bar(stat="identity", color="cornsilk4", fill="cornsilk4")+coord_flip()

#black metal
blackdat<-tolower(paste(df2$Band,collapse=" "))
blackdat1<-gsub(pattern, "", blackdat)
words_black <- strsplit(blackdat1, " ")

final.list.black <- unlist(words_black)
not.blanks <- which(final.list.black != "")
final.list.black <- final.list.black[not.blanks]

dat.black<-sort(table(final.list.black),decreasing=TRUE)[1:15]
dat.black<-melt(dat.black)
dat.black <- transform(dat.black, final.list.black = reorder(final.list.black, value))
p2<-ggplot(dat.black, aes(x = final.list.black, y = value,)) + geom_bar(stat="identity", color="cadetblue", fill="cadetblue")+coord_flip()

#aaaand death metal
deathdat<-tolower(paste(df3$Band,collapse=" "))
deathdat1<-gsub(pattern, "", deathdat)
words_death <- strsplit(deathdat1, " ")

final.list.death <- unlist(words_death)
not.blanks <- which(final.list.death != "")
final.list.death <- final.list.death[not.blanks]

dat.death<-sort(table(final.list.death),decreasing=TRUE)[1:15]
dat.death<-melt(dat.death)
dat.death <- transform(dat.death, final.list.death = reorder(final.list.death, value))
p3<-ggplot(dat.death, aes(x = final.list.death, y = value,)) + geom_bar(stat="identity", color="darkslategrey", fill="darkslategrey")+coord_flip()

grid.arrange(p1,p2,p3, ncol=1)
