#https://datascienceplus.com/time-series-analysis-with-wind-resource-assessment-in-r/
library(tidyverse) # r-package ecosystem
library(MASS) # statistical functions
library(knitr) # fancy tables
library(clifro) # windrose plot
library(scales) # percentages
library(elevatr) # elevation data
library(raster) # geospatial
library(leaflet) # mapping


api_key<-'zZN9lLYGEIuYd6ruQynCnwBYBAg0bwkkutTBLMfh'
name='Mohamed+Abuella'
affiliation='UNCC'
email='mhdabuella@gmail.com'
lat=42.2756887
lon=-71.21161789999996

####2009
# url<-paste('http://developer.nrel.gov/api/wind-toolkit/wind/wtk_download.csv?api_key=',api_key,'&wkt=POINT(',lon,'%20',lat,
#            ')&attributes=wind_speed,wind_direction,power,temperature,pressure&names=2009&full_name=',name,'&email=',email,'&affiliation=',affiliation,
#            '&reason=Example',sep='')
###2010
url<-paste('http://developer.nrel.gov/api/wind-toolkit/wind/wtk_download.csv?api_key=',api_key,'&wkt=POINT(',lon,'%20',lat,
           ')&attributes=wind_speed,wind_direction,power,temperature,pressure&names=2010&full_name=',name,'&email=',email,'&affiliation=',affiliation,
           '&reason=Example',sep='')


#download data as a dataframe
df<-read_csv(url,skip=3,col_types = cols())
#df<-read_csv('C:/Users/Mhdella/Desktop/wind_time_series_analysis/wind_data_2009_Boston.csv',skip=3,col_types = cols())

# tidy names
names(df)<-gsub("[^[:alnum:]]", "", names(df))
# convert dates to timestamp
df$timestamp_utc=as.POSIXct(paste(df$Year,df$Month,df$Day,df$Hour,df$Minute,'00',sep='-'),format='%Y-%m-%d-%H-%M-%S',tz='UTC')
head(df)
### Timeseries
ggplot(df,aes(timestamp_utc,windspeedat100mms))+geom_line()+theme_minimal()
### Monthly Wind Speed Distribution
ggplot(df,aes(factor(Month),windspeedat100mms))+
  geom_boxplot()+
  theme_minimal()+
  labs(x='Month')
### Wind Rose
windrose(speed = df$windspeedat100mms,
         direction = df$winddirectionat100mdeg,
         n_directions = 12,
         speed_cuts = seq(0,20,4),
         ggtheme='minimal',
         col_pal = 'YlGnBu')

### Weibull Fit
```{r, weibull}
weibull_fit<-fitdistr(df$windspeedat100mms,'weibull')
x<-seq(0,20,.01)
weibull_density<-tibble(x,y=dweibull(x = x,shape = weibull_fit$estimate[1],scale = weibull_fit$estimate[2]))
ggplot(df,aes(windspeedat100mms))+
  geom_histogram(aes(y=..density..),bins=30,color='white')+
  geom_line(data=weibull_density,aes(x=x,y=y),color='red')+
  theme_minimal()
```

## Energy Capture

### Power Curve
url<-'http://www.wind-power-program.com/Downloads/Databasepowercurves(May2017).zip'
tmp<-tempfile()
download.file(url,tmp)
unzip(tmp,files = 'Databasepowercurves(May2017)/HAWTs/500kw and above/General Electric/GE 1.5SLE 77m 1.5MW (MG).pow',junkpaths = T)
unlink(tmp)
pc<-read_csv('GE 1.5SLE 77m 1.5MW (MG).pow',
             skip=5,col_names = F,col_types='n',n_max = 30)
pc<-tibble(ws=seq(0.5,29.5,1),kw=pc$X1)
ggplot(pc,aes(ws,kw))+geom_point()+geom_line()+theme_minimal()


### Density Adjust Wind Speed

df<-mutate(df,air_density=(df$surfaceairpressurePa/df$airtemperatureat2mK*287)*.00001,
dc_ws=windspeedat100mms*(air_density/mean(air_density))^(1/3))
ggplot(df,aes(windspeedat100mms,dc_ws,color=air_density))+
geom_point()+
theme_minimal()+
coord_equal()

### Predict Energy
df$kw<-approx(pc$ws,pc$kw,df$dc_ws)$y
ggplot(df,aes(kw))+
geom_density(fill='blue')+
theme_minimal()


### Results

### Aggregates
monthly<-df %>%
  group_by(Month) %>%
  summarise(hours=n()/12,
            windspeedat100mms=mean(windspeedat100mms),
            mwh=sum(kw,na.rm=T)/(1000*12)) %>%
  mutate(ncf=percent(mwh/(hours*1.5)))
kable(monthly,digits=1,caption='monthly totals',align='c')

###annual
annual<-data.frame(mwh=sum(monthly$mwh),ncf=percent(sum(monthly$mwh/(sum(monthly$hours)*1.5))))
kable(annual,digits=1,caption = 'annual totals',align='c')

### Comparison with NREL Model

ggplot(df,aes(kw*.001,powerMW))+
  geom_point()+
  theme_minimal()+
  labs(x='GE 1.5sle MW',y='NREL 5MW turbine example MW')