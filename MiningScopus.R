require('bibliometrix')
require('ggplot2')

sensors<-c('Hyperion','CMODIS','PRISMA',
           'HYSI','EnMAP','HISUI','SHALOM',
           'Dyson','MSI-Sentinel-2')
#'TG-1', 'GISAT'

setwd('C://Users/dandrimont/Documents/THEO/Scopus')

df.publiYr.all<-data.frame()
colnames(df.publiYr.all)<-c('Year','Publication','Sensor')

for (i in seq(1,length(sensors))){
biblio <- readLines(paste0(sensors[i],'.bib'))
df<-convert2df(biblio, dbsource = "scopus", format = "bibtex")
results <-biblioAnalysis(df)
S=summary(object = results, k = 10, pause = FALSE)

df.publiYr<-data.frame(cbind(
  as.numeric(as.character(S$AnnualProduction$Year)),
  S$AnnualProduction$Articles,
  rep(sensors[i],nrow(S$AnnualProduction))))

colnames(df.publiYr)<-c('Year','Publication','Sensor')
df.publiYr$Year<-as.numeric(as.character(df.publiYr$Year))
df.publiYr$Publication<-as.numeric(as.character(df.publiYr$Publication))

df.publiYr.all<-rbind(df.publiYr.all,df.publiYr)
}

#PLOT ALL YEARS
ggplot(aes(x=Year, y=Publication, colour=Sensor), data = df.publiYr.all) +
  xlab('Year')+
  ylab('Number of publications')+ 
  geom_point() + geom_line()

aggregate(df.publiYr.all$Publication, by=list(Category=df.publiYr.all$Sensor), FUN=sum)


#PLOT TILL 2016
df.publiYr.all.till2016<-df.publiYr.all[df.publiYr.all$Year!=2017,]

ggplot(aes(x=Year, y=Publication, colour=Sensor), data = df.publiYr.all.till2016) +
  xlab('Year')+
  ylab('Number of publications')+ 
  geom_point() + geom_line()

df.sum<-aggregate(df.publiYr.all.till2016$Publication, by=list(Sensor=df.publiYr.all.till2016$Sensor), FUN=sum)
colnames(df.sum)<-c("Sencor","Sum")

write.csv(df.sum, file = "PublicationSumBySensorTill2016.csv")

