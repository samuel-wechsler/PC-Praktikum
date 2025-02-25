#########################################################
#R script zur Auswertung von Frequenzsweeps
#jschmaelzle, 02.03.2023, updated 02/2025
#########################################################

#-------------------------------------------------------
#clear workspace and plots
rm(list=ls())
graphics.off()

library(tuneR)        #wichtiges Package zum Arbeiten mit Sound-Dateien

source("../APECPVutils.R")     #euren Pfad hinzufügen, wo ihr "APECPVutils.R" gespeichert hab

#globaler Graphikparameter
par(las=1)

#number of ticks on x axis (for the plot)
num.tickmarks <-6 

#-------------------------------------------------------
#find all measurement files 
files <- list.files(path = "data_sweep_2025",pattern = "wav") #data_sweep = muss ein Unterordner vom Working directory sein (!), wo eure Messungen gespeichert sind

#-------------------------------------------------------
#loop over those files
for(n in 1:length(files)){
  
  
  #pdf speichern als
  outputname <- sub(".wav","_ausgewertet.pdf",files[n])
  
  #-------------------------------------------------------
  #read the files that were found
  FullPath <- paste(getwd(),"data_sweep_2025",files[n], sep="/") #Vollständiger Name zu der n-ten messung im Unterordner "data_einschwingen" des Working directory
  
  data <- read.WAV(FullPath)   #n-te messung auslesen
  
  t <- data$Time
  s <- data$Signal
  sr <- data$Rate #in hz
  dt <- 1/sr # in s

  
  t <- t - trigger(t, s, 0.05) # move t0	
  
  s <- s[t>0] # move s0
  t <- t[t>0]
  
   t.p <- approx(t,t, n=sr)$x #if number of data needs to be shortened
   s.p <- approx(t,s, t.p)$y

#-------------------------------------------------------
D <- readline(prompt = "Enter duration of sweep in seconds: \n")
D <- as.integer(D)

start <- readline(prompt = "Enter starting frequency in Hz: \n")
start <- as.integer(start)

stop <- readline(prompt = "Enter end frequency in Hz: \n")
stop <- as.integer(stop)

l <- readline(prompt = "Enter position of microphone in m: \n")
l <- as.numeric(l)

cat("Plotting...", "\n")

#-------------------------------------------------------
#zeit in frequenz umwandeln
f <- t2f(D,start,stop,t)

#plot data in new window
windows()
plot(t.p,s.p, type="l",
     xlab="t / s",
     ylab="A / V",
     xlim=c(0,D),
     xaxt="n")

#make x-axis beautiful
axis(at=seq(0,D,length=num.tickmarks),labels = round(seq(start,stop,length=num.tickmarks),2) ,side=3)
axis(at=seq(0,D,length=num.tickmarks),labels = round(seq(0,D,length=num.tickmarks),2) ,side=1)
mtext(expression(italic(f)*" / Hz"), side=3, line=2.8, cex=1.2)


#-------------------------------------------------------
#Loop to find the maxima
pos <- NULL
N <- as.integer(readline(prompt = "Enter number of maxima to be evaluated: \n"))
for (k in 1:N){ 
  klick <- locator(1)$x  # find the x position where we clicked
  ind <- which( abs(t-klick) <0.1)
  m <- which.max(s[ind])
  pos <- c(pos, t[ind][m])
}

abline (v=pos, col="red", lty=2)

#save plot as pdf and clear plot window
dev.copy2pdf(file=outputname, width=8, height= 6)
#graphics.off()

#----------------------------------
#schallgeschwindigkeit berechnen und ausgeben (Formel aus Praktikumsbuch S.556)
posF <- t2f(D,start,stop,pos)
m <- posF/mean(diff(posF))

vs <- posF*2*l/m
cat("Calculated speed of sound for \"", files[n],"\" is:", "\n")
cat(mean(vs), "m/s", "\n", "\n")

}
readline(prompt="Press [enter] to continue")

#---------------------------------
#(to calculate the uncertainty take into account the uncertainty of the length measurement)
#this uncertainty is very high, as we cant measure the absolute distance very accurately