#Load the required libraries
library(XML)
library(ggplot2)
library(plyr)

#Read table data from the National UFO Reporting Center's site
base_url<-"http://www.nuforc.org/webreports/"
url0<-"http://www.nuforc.org/webreports/ndxevent.html"
table0<-lapply(url0, function(url) {
  total<-readHTMLTable(url)
  n.rows<-unlist(lapply(total, function(t) dim(t)[1]))
  as.data.frame(total[[which.max(n.rows)]])
}  )
table0[[1]]$Month<-substring(table0[[1]]$Reports,1,2)
table0[[1]]$Year<-substring(table0[[1]]$Reports,4,7)

#Converting the "factor" format to the more usable Date and numeric format
table0[[1]]$Date<-as.Date(paste("01/",table0[[1]]$Reports,sep=""), "%d/%m/%Y")
table0[[1]]$Count<-as.numeric(as.character(table0[[1]]$Count))
table0[[1]] <- table0[[1]][nrow(table0[[1]]):3,]

#Plotting no. of sightings over time.. Interesting results are in store!
try2<-tail(table0[[1]],n=762)
ggplot(try2, aes(x=try2$Date, y=try2$Count, group=1))+geom_line(color="#CD4F39", size=0.75)+xlab("Date (Year)") + ylab("Count (#Sightings)")

#Heatmap of the last 15 years
try3<-tail(table0[[1]],n=183)
ggplot(try3, aes(try3$Month, try3$Year)) + 
  geom_tile(aes(fill = Count), colour = "white") + 
  scale_fill_gradient(low = "steelblue",high = "firebrick3")+
  theme_grey(base_size = 10)+
  labs(x = "Month", y = "Year")+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))

#Attempting to visit each page for the year 2014 and read the HTML tables
urls<-paste(base_url,"ndxe",
            table0[[1]]$Year,table0[[1]]$Month,
            ".html", sep="")
urls<-urls[grepl("ndxe2014",urls)]

tabla<-lapply(urls, function(url) {
  total<-readHTMLTable(url)
  n.rows<-unlist(lapply(total, function(t) dim(t)[1]))
  as.data.frame(total[[which.max(n.rows)]])
  }  )

#Pounding the data into shape.. 
tabla1<-ldply(tabla, data.frame)
tabla1$DateTime<-as.character(tabla1$Date...Time)
tabla1$Month<-sub("[//].*$", "", tabla1$DateTime)
tabla1$Month<-as.numeric(tabla1$Month)
try4<-count(tabla1, c("Month", "State"))
try5<-count(tabla1, "State")
try6<-merge(try4, try5, "State")
try6<-try6[(try6$freq.y>=200),]

#ANd finally one more plot
ggplot(try6, aes(x=try6$Month, y=try6$freq.x, group=try6$State, colour=try6$freq.y))+geom_line(size=1)+scale_colour_gradient(low="coral4", high="coral1")

