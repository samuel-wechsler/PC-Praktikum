#########################################################
#R script zur Bestimmung der Echolaufzeiten aus White-Noise Daten
#jschmaelzle, 02.03.2023, updated 02/2025
#adapted from Dominique Borgeaud dit Avocat
#########################################################

#-------------------------------------------------------
#clear workspace and plots
rm(list=ls())
graphics.off()

library(tuneR)        #wichtiges Package zum Arbeiten mit Sound-Dateien

source("../APECPVutils.R")     #euren Pfad hinzufügen, wo ihr "APECPVutils.R" gespeichert hab

#globaler Graphikparameter
par(las=1)
#-------------------------------------------------------
#exakter Filename der Messung, die ausgewertet werden soll
filename <- "C:/ETH/4. Jahr/Assistenz CPV/jonas CPV test/Whitenoise400mmSauerstoff.wav"


data <- read.WAV(filename)

t <- data$Time
s <- data$Signal
sr <- data$Rate 
dt <- 1/sr 

t <- t - trigger(t, s, 0.1) #Nullpunkt verschieben

s <- s[ t>0 & t<5] #Nur interessante Werte weiterverwenden
t <- t[ t>0 & t<5]


#-------------------------------------------------------
#ursprüngliche messung plotten und als pdf speichern
plot(t,s,type="l", xlim=c(0,1),
     xlab="t / s",
     ylab="A / V")
dev.copy2pdf(file="outputname_raw.pdf", width=8, height= 6)

#-------------------------------------------------------
#Autokorrelationsfunktion anwenden
auto <- myacf(s,1000,dt)  #Does the autocorrelation function

tau <- auto$tau #Relative Verschiebung
acf <- auto$ACF # Amplitude

windows()
plot(tau,acf,type="l")

#plot speichern
dev.copy2pdf(file="outputname_acf.pdf", width=8, height= 6)
#-------------------------------------------------------
#Maxima finden und anklicken
N <- 5 
pos <- NULL
for (k in 1:N){
  
  klick <- locator(1)$x
  ind <- which( abs(tau-klick) <0.00025)
  m <- which.max(acf[ind])
  pos <- c(pos, tau[ind][m])
}

plot(tau,acf,type="l",
     xlab="shift / s",
     ylab="relative correlation")
abline (v=pos, col="red", lty=2)

#echolaufzeiten bestimmen und ausgeben
te <- diff(pos)
cat("\n", "Berechnete Echolaufzeit (Durchschnitt): ", round(mean(te),7), "s", "\n")
#-------------------------------------------------------
#um aus diesen Zeiten die Schallgeschwindigkeit zu berechnen: gleiches Vorgehen wie beim Puls-Echo
#(siehe Seite 552 im Praktikumsbuch, Formel 52)