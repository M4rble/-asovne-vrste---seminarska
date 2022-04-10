#seminarska Časovne vrste

#knjižnjice
library(ggplot2)
#library(TSA)
library(LSTS)
library(modeest)


#uvoz in obdelava podatkov
podatki1 <- scan("A12nti.txt")
x_t <- ts(podatki1, start=1, frequency = 1)
x_t

plot(x_t, xlab="id", ylab = "value", main = "Graf podatkov A za opazovanje trenda in sezonske komponente")
osnovni1 <- recordPlot()
print(osnovni1)
paste("Očitna sezonskost (vsakih cca 20 podatkov padec), rahel trend navzgor")

#odstranjevanje trenda in sezonskosti
#z diff
no.trend.1 <- diff(x_t, lag = 15, differences = 1)
plot(no.trend.1, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 1")
abline(h=mean(no.trend.1), col="red")
legend("bottomright",
       title="data A",
       c("diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.1))
paste("Očitno smo se rešili trenda, sezonskost morda še ostaja vsakih podatkov")

no.trend.1.no.season <- diff(no.trend.1, lag = 50)
plot(no.trend.1.no.season, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 1.1")
abline(h=mean(no.trend.1.no.season), col="red")
legend("bottomleft",
       title="data A",
       c("diff diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.1.no.season))
paste("Mogoče še rahla seznoska vsakih 50 podatkov")

no.trend.1.no.season2 <- diff(no.trend.1.no.season, lag = 50)
plot(no.trend.1.no.season2, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 1.2")
abline(h=mean(no.trend.1.no.season2), col="red")
legend("bottomleft",
       title="data A",
       c("diff diff diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.1.no.season2))
paste("Potencialna rahla sezonska komponenta, ni nujno")



#diff = 2
no.trend.2 <- diff(x_t, lag = 20, differences = 2)
plot(no.trend.2, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 2")
abline(h=mean(no.trend.1), col="red")
legend("bottomright",
       title="data A",
       c("diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.2))
paste("Očitno smo se rešili trenda, sezonskost še vedno ostaja vsakih 50 podatkov")

no.trend.2.no.season <- diff(no.trend.2, lag = 60)
plot(no.trend.2.no.season, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 2.1")
abline(h=mean(no.trend.2.no.season), col="red")
legend("bottomleft",
       title="data A",
       c("diff diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.2.no.season))
paste("Morda še vedno seznoska komponenta 50 podatkov?")

no.trend.2.no.season2 <- diff(no.trend.2.no.season, lag = 50)
plot(no.trend.2.no.season2, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 2.2")
abline(h=mean(no.trend.2.no.season), col="red")
legend("bottomleft",
       title="data A",
       c("diff diff diff podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.2.no.season2))
paste("Ni več sezonske komponente")


#z kombinacijo logaritma in diff
no.trend.3 <- log(x_t)
plot(no.trend.3, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 3")
abline(h=mean(no.trend.3), col="red")
legend("bottomright",
       title="data A",
       c("log podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.3))
paste("Očitno smo uspešno odstranili trend, ostaja še sezonska komponenta cca 40 dni")

no.trend.3.no.season <- diff(no.trend.3, lag = 40)
plot(no.trend.3.no.season, xlab="id", ylab = "value", main = "Graf podatkov A brez trenda in sezonskosti 3.1")
abline(h=mean(no.trend.3.no.season), col="red")
legend("topleft",
       title="data A",
       c("diff log podatki","Povprečje"),
       col=c("black","red"),
       lty="solid",
       bty="n")
print(mean(no.trend.3.no.season))
paste("Očitno smo uspešno odstranili tudi sezonsko komponento")

#druge metode - ko bomo delal na vajah :)

#periodogram
par(mfrow=c(1,3))
periodogram(no.trend.1.no.season2)
periodogram(no.trend.2.no.season2)
periodogram(no.trend.3.no.season)

smooth.periodogram(no.trend.1.no.season2)
smooth.periodogram(no.trend.2.no.season2)
smooth.periodogram(no.trend.3.no.season)

paste("Pri prvih dveh metodah perioda okoli 0.25, pri tretji je ni?")

napoved <- naive(x_t)
residuali1 <- residuals(x_t)















podatki2 <- scan("B12bio.txt")
y_t <- ts(podatki2, start=2, frequency = 1) 
y_t
plot(y_t, xlab="id", ylab = "value", main = "Graf podatkov B za opazovanje trenda in sezonske komponente")
osnovni2 <- recordPlot()
print(osnovni2)
paste("Očitna sezonskost (vsakih cca 30 podatkov), očiten trend navzgor")
