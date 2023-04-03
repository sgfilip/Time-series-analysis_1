wig20_d <- read.csv("C:/Users/Filip/OneDrive/EFiD/Projekt2/wig20_d.csv")

install.packages("data.table")
install.packages("urca")
install.packages("forecast")
install.packages("zoo")
install.packages("aTSA")
install.packages("rugarch")
library(rugarch)
library(aTSA)
library(zoo)
library(forecast)
library(urca)
library(data.table)

stopyLog <- (log(wig20_d$Zamkniecie/shift(wig20_d$Zamkniecie)))

stopyLog <- stopyLog[-1]

#Test ADF - rozszerzony Dickeya-Fulera, na stacjonarno¶æ
stacjo <- ur.df(stopyLog, type = "none")

summary(stacjo)
#Stacjonarny

#Tworzenie modelu ARMA/ARIMA
arma <- auto.arima(stopyLog)

summary(arma)
#Najlepszy model ARMA jest bez opó¼nieñ

model <- arima(stopyLog, order = c(0,0,0))
print(model)
#Testowanie modelu
Acf(model$residuals)

ljungBox <- c()

for(i in 1:10){
  ljungBox <- c(ljungBox, Box.test(model$residuals, lag = i, type = "Ljung-Box")$p.value)
  
}

ljungBox
#Brak autokorelacji w resztach modelu ARMA

#Badanie efektu ARCH
archEfekt <- arch.test(model)

#Efekt ARCH wystÄ™puje

#Tworzenie modelu GARCH(1,1)

ModelSpec <- ugarchspec(mean.model = list(armaOrder=c(0,0)), variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model = "norm")

ModelGARCH <- ugarchfit(data = model$residuals, spec = ModelSpec, out.sample = 20)

ModelGARCH

plot(ModelGARCH)
#Wykresy modelu GARCH

#Prognoza wariancji z modelu GARCH
prognozaWariancji <- ugarchforecast(fitORspec = ModelGARCH, n.ahead = 5)

plot(fitted(prognozaWariancji))
plot(sigma(prognozaWariancji))

#Prognoza z ARIMA

prognozaLog <- predict(model, n.ahead = 5)

plot(prognozaLog$pred)
#Prognoza modelu ARIMA(0,0,0)

#Zmiana logarytmow
stopy <- exp(prognozaLog$pred)
#Obliczanie cen
ceny <- c(wig20_d$Zamkniecie[1061]*stopy[1])

for (i in 2:5) {
  ceny <- c(ceny, ceny[i-1]*stopy[i])
}

ceny
#Porownanie faktycznych cen do predykcji
wig20_dDoPrognozy <- read.csv("wig20_d (1).csv")

ME <- mean(ceny-wig20_dDoPrognozy$Zamkniecie)
MAE <- mean(abs(ceny-wig20_dDoPrognozy$Zamkniecie))
MAPE <- mean(abs((wig20_dDoPrognozy$Zamkniecie-ceny)/wig20_dDoPrognozy$Zamkniecie))

ME
MAE
MAPE

