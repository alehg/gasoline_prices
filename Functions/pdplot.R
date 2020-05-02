pdplot<-function(data,reg){
  date<-unique(data$date)
  res<-resid(reg)
  sqres<-res^2
  t<-length(date)
  avsr<-numeric(t)
  l<- length(unique(data[cutoff_datetime<='2017-03-30 00:00:00']$idcode)) #508
  for (i in 1:t){
    avsr[i]<-mean(sqres[l*(i-1)+1:l])
  }
  d<-cbind(date, avsr)
  d<-as.data.frame(d)
  d %>% mutate_if(is.factor, as.character) -> d
  d[,2]<-as.numeric(d[,2])
  g<-ggplot(d,aes(date,avsr))+geom_line() +
    labs(x='Days after deregulation',y='Average Squared Residuals') +
    theme_bw()
  return(g)
}