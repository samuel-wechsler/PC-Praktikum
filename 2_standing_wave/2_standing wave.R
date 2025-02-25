#########################################################
#R-script für Lorentz-Fit der Standing-Wave-Amplituden
#jschmaelzle, 02.03.2023, updated 02/2025
#adapted from Dominique Borgeaud dit Avocat
#########################################################

#clear workspace and plots
rm(list=ls())
graphics.off()

#set global graphic parameter
par(las=1) #horizontale Achsenbeschriftung

#-------------------------------------------------------
#Data

x <- c(10.0 , 11.0 , 12.0 , 13.0 , 14.0 , 15.0 , 15.2 , 15.5 , 15.7 , 15.8 ,
       15.9 , 16.0 , 16.1 , 16.2 , 16.3 , 16.4 , 16.5 , 16.6 , 16.7 , 16.8 ,
       16.9 , 17.0 , 17.5 , 18.0 , 18.5 , 19.0 , 19.5 , 20.0 , 21.0 , 22.0 ,
       23.0 , 24.0 , 25.0 , 26.0 , 27.0 , 28.0 , 29.0 , 29.5 , 30.0 , 30.3 ,
       30.5 , 30.6 , 30.9 , 31.0 , 31.2 , 31.5 , 32.0 , 32.5 , 33.0 , 33.5 ,
       34.0 , 35.0 , 36.0 , 37.0 , 38.0 , 39.0 , 40.0 , 41.0 , 42.0 , 43.0 ,
       43.5 , 44.0 , 44.5 , 45.0 , 45.2 , 45.7 , 46.0 , 46.5 , 47.0 , 47.5 ,
       48.0 , 48.5 , 49.0) #in cm
A <- c (0.057 , 0.062 , 0.071 , 0.089 , 0.129 , 0.216 , 0.268 , 0.377 ,
        0.491 , 0.614 , 0.697 , 0.910 , 0.928 , 0.983 , 0.875 , 0.739 , 0.630 ,
        0.517 , 0.446 , 0.394 , 0.340 , 0.294 , 0.194 , 0.143 , 0.113 , 0.096 ,
        0.086 , 0.076 , 0.066 , 0.059 , 0.057 , 0.057 , 0.060 , 0.066 , 0.077 ,
        0.099 , 0.146 , 0.199 , 0.283 , 0.408 , 0.584 , 0.666 , 0.727 , 0.642 ,
        0.490 , 0.336 , 0.216 , 0.157 , 0.120 , 0.101 , 0.087 , 0.070 , 0.062 ,
        0.058 , 0.056 , 0.057 , 0.061 , 0.069 , 0.083 , 0.109 , 0.133 , 0.173 ,
        0.257 , 0.450 , 0.510 , 0.534 , 0.392 , 0.231 , 0.171 , 0.127 , 0.107 ,
        0.093 , 0.082) #in Volt
Freq <- 1500 # Frequency of the data

# x <- c(15,15.5, 16, 16.5, 16.75, 17, 17.3, 17.4, 17.5, 17.7, 18, 18.5, 19, 19.5, 20,
#        20.5, 21.5, 22.5, 23.5, 24, 24.5, 25, 25.3, 25.4, 25.5, 25.6, 25.7,
#        26, 26.4, 26.8, 27.5, 28.5, 29.5, 31, 32, 32.5, 33, 33.3, 33.5, 33.7,
#        33.8, 33.9, 34, 34.2, 34.3, 34.9, 35.5, 36.5, 37.5, 39)
# A <-  c(0.043, 0.052, 0.069, 0.11, 0.156, 0.265, 0.476, 0.426, 0.301, 0.182, 0.119, 0.077,
#         0.055, 0.044, 0.039, 0.035, 0.033, 0.030, 0.048, 0.061, 0.088, 0.163, 0.27, 0.378,
#         0.416, 0.377, 0.294, 0.177, 0.099, 0.068, 0.048, 0.036, 0.033, 0.038, 0.055, 0.072,
#         0.12, 0.174, 0.273, 0.367, 0.341, 0.294, 0.248, 0.151, 0.137, 0.077, 0.052, 0.038,
#         0.034, 0.036)
# Freq <- 2000 # Frequency of the data

#-------------------------------------------------------
#plot data
plot(x,A,
     ylim=c(0, max(A)+0.2*max(A)),
     ylab= "A / V", 
     xlab= "x / cm")

number_of_maxima <- as.integer(readline(prompt = "Enter number of maxima to be evaluated: \n"))

#Find the Maxima
pos <- NULL
for(k in 1:number_of_maxima){  
  klick <- locator(1)$x
  ind <- which( abs(x-klick) < abs(max(x)-min(x))/5)
  m <- which.max(A[ind])
  pos <- c(pos, x[ind][m])
} 

#Find the distance that separate the maxima
dp <- mean(diff(pos))/2     

#The position at which the amplitude is maximized -> resonance
Lm <- NULL 

#loop over number of maxima (3) and fit lorentz function
for(k in 1:number_of_maxima){
  
  x.fit <- x[ abs(x-pos[k]) <= dp]    # The data that will be fitted
  y.fit <-A[ abs(x-pos[k]) <= dp]   
  
  C.s <- min(y.fit)   # Fitting parameter (min value)
  A.s <- abs(min(y.fit)-max(y.fit)) # Fitting parameter (Amplitude)
  x0.s <- x.fit[which.max(y.fit)] # Fitting parameter (x pos of maxima)
  
  G <- which(y.fit >= A.s/2 + C.s) # Get values for FWhM
  G.min <- min(G)
  G.max <- max(G)
  G <- abs( x.fit[G.max]-x.fit[G.min]) # Full width at half maximum
  s.s <- G/2 # Fitting parameter (x pos of maxima)
  
  A.s <- A.s/max(dcauchy(x.fit,x0.s, s.s))
  
  fit <- nls(y.fit ~ A * dcauchy(x.fit,x0,s) + C,
             start=list(A=A.s, x0=x0.s, s=s.s, C=C.s))  #The cauchy/lorentz fit
  x.p <- seq(from=min(x.fit), to=max(x.fit), length=10000) # Predicted data
  y.p <- predict(fit, list(x.fit=x.p))
  
  lines(x.p, y.p,col="red")
  Lm <- c(Lm, summary(fit)$coeff[2,1])
  
  #Beschriftung hinzufügen
  label <- round(summary(fit)$coeff[2,1], digits=2)
  label <- sprintf("%.2f", label)
  text ( summary(fit)$coeff[2,1], max(y.p)+0.05, labels = paste(label,"cm"), srt=90)
  
}

readline(prompt="Press [enter] to continue")

#diesen plot als pdf speichern
dev.copy2pdf(file="Standing_Wave.pdf", width=9.0, height= 6.5)
cat("\n","--Plot saved as ","Standing_Wave.pdf", "-- \n")

#-------------------------------------------------------
#m und vs berechnen
#m berechnen und auf ganze Zahl runden
m <- round(Lm/mean(diff(Lm)),0)

#linearer fit m vs. Lm machen
model <- lm(Lm ~ m)

#aus Steigung vs berechnen
lambda <- summary(model)$coeff[2,1] * 2
vs <- Freq * lambda / 100
sdlambda <- summary(model)$coeff[2,2] * 2
sd <- Freq * sdlambda / 100
plot(m,Lm)
abline(model)

#diesen plot als pdf speichern
dev.copy2pdf(file="Standing_Wave_LinearModel.pdf", width=9.0, height= 6.5)
cat("\n","--Plot saved as ","Standing_Wave_LinearModel.pdf", "-- \n")
