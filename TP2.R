setwd("C:/Users/Grégoire/Dropbox/ISFA/Cours 2A/Analyse de donnees/TP2")

CSD <- read.csv("CrimeStateDate.csv", header=TRUE)

SR0910 <- read.table("SecRoutiere0910.txt", header=TRUE)

library(ade4)
data(banque)
names(banque)

cv2005 <- CSD[CSD$Date=="2005",]
dim(cv2005)
crime <- cv2005$Crime_Violent
class(crime)

hist(crime, col="lightblue", main="Nombre de crimes violents")

par(mfrow=c(1,2))
dotchart(crime, main="Série brute", pch=20)
dotchart(sort(crime), main="Série ordonnée", pch=20)

par(mfrow=c(1,1))
boxplot(crime, col="lightblue", main="Nombre de crimes violents")
boxplot(crime, col="lightblue", horizontal=TRUE, main="Nombre de crimes violents")

class(banque$csp)
levels(banque$csp)
length(levels(banque$csp))

freqrel <- summary(banque$csp)/length(banque$csp)
360*freqrel

pie(summary(banque$csp), main="CSP présentes dans la banque")
pie(summary(banque$sexe), main="Le genre présent dans cette banque",
    col=c("darkblue", "pink"), label=c("homme", "femme"))

par(mfrow=c(1,2))
barplot(summary(banque$csp), main="Fréquences absolues", col="lightblue", las=2)
barplot(summary(banque$csp)/length(banque$csp), main="Fréquences relatives", col="lightblue", las=2)

par(mfrow=c(1,1))
cor(SR0910[,9], SR0910[,10])
plot(SR0910[,10], SR0910[,9], pch=20, main="Similitude du nb de blessés sur 2 ans",
     xlab="Blessés en 2009", ylab="Blessés en 2010")

library(gplots)
balloonplot(table(banque$age, banque$interdit), main="Interdits bancaires et classes d'age")
