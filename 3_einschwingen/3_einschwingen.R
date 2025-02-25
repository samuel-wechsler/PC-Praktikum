#########################################################
#R-script zur Visualisierung des Einschwingverhaltens eines Resonators
#5 Sounddateien können übereinander und mit schöner Achse visualisiert werden
#jschmaelzle, 02.03.2023, updated 02/2025
#########################################################

#-------------------------------------------------------
#clear workspace and plots
rm(list=ls())
graphics.off()

library(tuneR)        #wichtiges Package zum Arbeiten mit Sound-Dateien

source("C:/ETH/3. Jahr/FS23/Hilfsassistenz/APECPVutils.R")     #euren Pfad hinzufügen, wo ihr "APECPVutils.R" gespeichert hab

#-------------------------------------------------------
#set global graphic parameter
par(mfrow=c(5,1),mar=c(5,5,1,3), bty="n",las=1)

titles <- c("A","B","C","D","E")
#-------------------------------------------------------
#find all measurement files (should be 5)
files <- list.files(path = "data_einschwingen",pattern = "wav") #data_einschwingen = muss ein Unterordner vom Working directory sein (!), wo eure Messungen gespeichert sind

#-------------------------------------------------------
#loop over those files
for(n in 1:length(files)){
  
  #-------------------------------------------------------
  #read the files that were found
  FullPath <- paste(getwd(),"data_einschwingen",files[n], sep="/") #Vollständiger Name zu der n-ten messung im Unterordner "data_einschwingen" des Working directory
  
  data <- read.WAV(FullPath)   #n-te messung auslesen
  
  D <- 0.15 #right limit of x axis in seconds
  t <- data$Time
  s <- data$Signal
  sr <- data$Rate #in hz
  dt <- 1/sr # in s
  
  t <- t - trigger(t, s, 0.1) #move t0 according to trigger function				
  
  s <- s[ t>0 & t<D] #move s0 and send
  
  t <- seq(from=0, by=dt, length=length(s))  # redefine t according to send
  
  #-------------------------------------------------------
  #plot the data and only add x axis to last (bottom) plot
  if(n<length(files)){
  plot(t,s,type="l",xaxt="n", xlab="", ylab="A / V", main=titles[n], cex.main=1.75,
       ylim=c(-1,1))}
  
  else{ plot(t,s,type="l", xlab="t / s", ylab="A / V", main=titles[n], cex.main=1.75,
              ylim=c(-1,1))}
}

#save plot as pdf
dev.copy2pdf(file="Einschwingen.pdf", width=6.5, height= 8)
cat("\n","--Plot saved as ","Einschwingen.pdf", "-- \n")
