#########################################################
#R-script zur Bestimmung von Echo-Laufzeiten aus Puls-Echos
#jschmaelzle, 02.03.2023, updated 02/2025
#########################################################

#clear workspace and plots
rm(list=ls())
graphics.off()

#set global graphic parameter
par(las=1) #horizontale Achsenbeschriftung

#-------------------------------------------------------
#install.packages("tuneR")            #in Konsole eingeben, um das Package zu installieren
library(tuneR)                        #wichtiges Package zum Arbeiten mit Sound-Dateien

source("../APECPVutils.R")     #euren Pfad hinzuf?gen, wo ihr "APECPVutils.R" gespeichert habt

#-------------------------------------------------------
#read all measurement files
measurement.files <- list.files(path = "data_cropped",pattern = "wav") #data_puls = muss ein Unterordner vom Working directory sein (!), wo eure Messungen gespeichert sind

#-------------------------------------------------------
#Datenauswertung
#-------------------------------------------------------
te <- NULL        #Vektor definieren, in den die Echolaufzeiten gespeichert werden

#Loop ?ber alle gefundenen measurement Files
for(n in 1:length(measurement.files)){

  
  FullPath <- paste(getwd(),"data_cropped",measurement.files[n], sep="/") #Vollst?ndiger Name zu der n-ten messung im Unterordner "data_puls" des Working directory
  
  data <- read.WAV(FullPath)   #n-te messung auslesen
  
  t <- data$Time        #Zeit-Vektor aus Daten
  s <- data$Signal      #Amplitude bzw. Signal aus Daten
  sr <- data$Rate       #distance between two data points in hz 
  dt <- 1/sr            #from hz to s
  
  #-------------------------------------------------------
  #Rohdaten plotten
  plot(t,s, type="l",
       xlab="t / s",
       ylab="A / V")
  title(main = paste(sub(".wav", "",measurement.files[n]), "- raw data")) #filename als titel nehmen, ohne ".wav"

  readline(prompt="Press [enter] to continue")
  
  #-------------------------------------------------------
  #nochmal plotten, aber sch?ner
  t <- t - trigger(t, s, 0.1) #Move the time axis to begin of signal (triggerfunktion gibt zeit zur?ck, an der ein bestimmter signalwert ?berschritten wird)
  #t <- seq(from=0, by=dt, length=length(s))
  
  #in neuem fenster, damit locator auf jeden Fall funktioniert
  dev.new()
  
  plot(t[which(t>0)],s[which(t>0)],   #nur werte bei t>0 plotten
       type="l",
       xlab="t / s",
       ylab="A / V",
       xlim=c(0,0.05))  #x-achsen limit des plots muss evtl. abge?ndert werden
  title(main = paste(sub(".wav", "",measurement.files[n]), "- with trigger")) #filename als titel nehmen, ohne ".wav"
  
  #diesen plot als pdf speichern
  dev.copy2pdf(file=sub(".wav","_raw.pdf",measurement.files[n]), width=9.0, height= 6.5)
  cat("\n","--Plot saved as ",sub(".wav","_raw.pdf",measurement.files[n]), "-- \n")
  
  #-------------------------------------------------------
  #maxima-positionen finden, um Echolaufzeit zu bestimmen
  pos <- NULL
  N <- 2      #Anzahl an maxima, die wir bestimmen wollen
  
  cat("\n","--> Click on the", N, "maxima you want to evaluate..", "\n")
  
  
  #loop ?ber diese Anzahl
  for (k in 1:N){ 
    klick <- locator(1, type="p", cex=0.5)   #find the position where we clicked
    
    #im Bereich +- 0.00065 des geklickten Punkts nach maxima suchen
    ind <- which(abs(t-klick$x) <0.000065)
    m <- which.max(s[ind])
    
    #x werte der gefundenen maxima in vektor "pos" speichern
    pos <- c(pos, t[ind][m])
  }
  
  #gefundene maxima einzeichnen 
  abline(v=pos, col="red", lty=2)
  
  #Echolaufzeit als Differenz der Maxima bestimmen
  te <- c(te,diff(pos))
  mtext(paste("Time difference between highlighted maxima: ", diff(pos),"s", sep=""), side=3, adj=0)
  
  
  readline(prompt="Press [enter] to continue")  # Gives you time to check out the plots to see if the maxima have been chosen correctly
  #zweiten plot als pdf speichern
  dev.copy2pdf(file=sub(".wav","_ausgewertet.pdf",measurement.files[n]), width=9, height= 6.5)
  cat("\n","--Plot saved as ",sub(".wav","_ausgewertet.pdf",measurement.files[n]), "-- \n", "\n")
}

cat("Bestimmte Echolaufzeiten in s:", te, "\n", "\n")
#Zur Berechnung der Schallgeschwindigkeit aus diesen Echolaufzeiten, siehe Praktikumsbuch ab S.551