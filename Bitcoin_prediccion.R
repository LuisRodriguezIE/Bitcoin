# Prediccion Bitcoin
#Pagina de informacion: https://blockchain.info/charts/market-price?timespan=all

rm(list=ls()) 

library(forecast)
library(ggplot2)
library(ggfortify)
library(highcharter)
library(zoo)
library(tseries)
library(tidyr)

###Conexion al servicio WEB y almacenamiento de datos.
archivo_api <- "https://blockchain.info/charts/market-price?timespan=1800days&format=csv"
info_csv <- read.csv(file=archivo_api, header = FALSE)
bit_df <- data.frame(info_csv)
View(bit_df)

bit_df1<-bit_df

bit_df<-separate(data = bit_df, col = V1, into = c("V1", "hour"), sep = "\\ ")
bit_df$hour<-NULL
bit_df$V1 = as.Date(bit_df$V1)

plot(bit_df)
ggplot(data=bit_df, aes(x=V1,y=V2)) + geom_point(color="blue",size=1)+
   scale_x_date() + 
  xlab("Fecha") + ylab("Precio USD")+ggtitle("Comportamiento Bitcoin")

#Asignacion a un nuevo data frame
bit_df$Close <- bit_df$V2     #  csv_df$Close<-csv_df$V2
valdf <- bit_df               #  (edf<-csv_df)     

#Analizar el valor de las ultimos datos agregados
tail(valdf)

#Calculo de la media movil (moving average)
valdf$v7_MA = ma(valdf$Close, order=7)
valdf$v14_MA = ma(valdf$Close, order=14)
valdf$v30_MA = ma(valdf$Close, order=30)

plot(valdf$Close,type="l",col="red",main="Precio original")
plot(valdf$v7_MA,col="blue",main="Orden 7")
plot(valdf$v14_MA,col="gold",main="Orden 14")
plot(valdf$v30_MA,col="green",main="Orden 30")

#edf<=>valdf

#STL
#Seasonal and Trend decomposition using Loess
#Descomposicion de la serie de tiempoo
valdf_ma <- ts(na.omit(valdf$v7_MA), frequency=10)
valdf_ma
plot(valdf_ma)

#Descomposicion de series temporales
decomp_valdf <- stl(valdf_ma, s.window="periodic")
plot(decomp_valdf)          #plot(decomp_rental)

#Ajuste estacional de la informacion
adj_valdf <- seasadj(decomp_valdf)
plot(adj_valdf) 

#Modelos de ajuste

#Triple Exponential Smoothing
hfit1<-HoltWinters(adj_valdf)
f1<-forecast(hfit1, h=(30*3))

#Trigonometric Box-Cox ARMA Trend Seasonal
#Exponential smoothing.  #Mejor modelo de ajuste de acuerdo a las caracteristicas de la informacion.
hfit2<-tbats(adj_valdf)
f2<-forecast(hfit2, h=(30*3))

#Stationarity short term implies
hfit3<-auto.arima(adj_valdf)
f3<-forecast(hfit3, h=(30*3))

plot(f1)
plot(f2)
plot(f3)

#Curves and points to a data frame.
df <- fortify(f2)
View(df)
df$Data<-round(df$Data)
df$Fitted<-round(df$Fitted)
df$Index<-seq(as.Date("2013-04-03"), (as.Date("2018-03-07")+(30*(3))),length.out=length(df$Index))


highchart(type = "stock") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_title(text = "Prediccion Bitcoin usando modelo TBATS") %>% 
  hc_add_series(df, "line", hcaes(Index, Data), name = "Precio Actual", color="black") %>% 
  hc_add_series(df, "line", hcaes(Index, Fitted), name = "Prediccion Modelo") %>%
  hc_add_series(df, "line", hcaes(Index, `Point Forecast`), name = "Precio predecido") %>% 
  hc_add_series(df, "arearange", hcaes(Index, low = `Lo 95`, high = `Hi 95`), name = "Intervalo de prediccion") 
