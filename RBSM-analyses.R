library(readxl)
Guate_Data97 <- read_excel("Dropbox/Lia-IA-Enviro/Data/Guate-Data97.xlsx")
View(Guate_Data97)

names(Guate_Data97)

######### Sensitivities for overall counts #########
ind.DF.spp <- which(Guate_Data97$DF==1,arr.ind=TRUE)
DF.alldata <- Guate_Data97[ind.DF.spp,]
DF.sens.counts <- table(DF.alldata$Sens)
DF.sens.counts <- c(DF.sens.counts[c("L","M")],0)
names(DF.sens.counts)[3] <- "H"
print(DF.sens.counts)


ind.HT.spp <- which(Guate_Data97$HT==1,arr.ind=TRUE)
HT.alldata <- Guate_Data97[ind.HT.spp,]
HT.sens.counts <- table(HT.alldata$Sens)
HT.sens.counts <- HT.sens.counts[c("L","M","H")]
print(HT.sens.counts)

ind.PO.spp <- which(Guate_Data97$PO==1,arr.ind=TRUE)
PO.alldata <- Guate_Data97[ind.PO.spp,]
PO.sens.counts <- table(PO.alldata$Sens)
PO.sens.counts <- PO.sens.counts[c("L","M","H")]
print(PO.sens.counts)

ind.CF.spp <- which(Guate_Data97$CF==1,arr.ind=TRUE)
CF.alldata <- Guate_Data97[ind.CF.spp,]
CF.sens.counts <- table(CF.alldata$Sens)
CF.sens.counts <- CF.sens.counts[c("L","M","H")]
print(CF.sens.counts)

#######  0 meters ########
ind.DF0.spp <- which(Guate_Data97$`DF0-tot`>0,arr.ind=TRUE)
DF0.alldata <- Guate_Data97[ind.DF0.spp,]
DF0.sens.counts <- table(DF0.alldata$Sens)
DF0.sens.counts <- c(DF0.sens.counts[c("L","M")],0)
names(DF0.sens.counts)[3] <- "H"
print(DF0.sens.counts)

DF0.split <- split(x=DF0.alldata,f=DF0.alldata$Sens)
DF0.L <- DF0.split$L 
DF0.M <- DF0.split$M
DF0.sens.abunds <- c(sum(DF0.L$`DF0-tot`),sum(DF0.M$`DF0-tot`),0)
names(DF0.sens.abunds) <- c("L","M","H") 




ind.HT0.spp <- which(Guate_Data97$`HT0-tot`>0,arr.ind=TRUE)
HT0.alldata <- Guate_Data97[ind.HT0.spp,]
HT0.sens.counts <- table(HT0.alldata$Sens)
HT0.sens.counts <- HT0.sens.counts[c("L","M","H")]
print(HT0.sens.counts)

ind.PO0.spp <- which(Guate_Data97$`PO0-tot`>0,arr.ind=TRUE)
PO0.alldata <- Guate_Data97[ind.PO0.spp,]
PO0.sens.counts <- table(PO0.alldata$Sens)
PO0.sens.counts <- PO0.sens.counts[c("L","M","H")]
print(PO0.sens.counts)

ind.CF0.spp <- which(Guate_Data97$`CF0-tot`>0,arr.ind=TRUE)
CF0.alldata <- Guate_Data97[ind.CF0.spp,]
CF0.sens.counts <- table(CF0.alldata$Sens)
CF0.sens.counts <- CF0.sens.counts[c("L","M","H")]
print(CF0.sens.counts)


#######  500 meters ########
ind.DF500.spp <- which(Guate_Data97$`DF500-tot`>0,arr.ind=TRUE)
DF500.alldata <- Guate_Data97[ind.DF500.spp,]
DF500.sens.counts <- table(DF500.alldata$Sens)
DF500.sens.counts <- c(DF500.sens.counts[c("L","M")],0)
names(DF500.sens.counts)[3] <- "H"
print(DF500.sens.counts)


ind.HT500.spp <- which(Guate_Data97$`HT500-tot`>0,arr.ind=TRUE)
HT500.alldata <- Guate_Data97[ind.HT500.spp,]
HT500.sens.counts <- table(HT500.alldata$Sens)
HT500.sens.counts <- HT500.sens.counts[c("L","M","H")]
print(HT500.sens.counts)

ind.PO500.spp <- which(Guate_Data97$`PO500-tot`>0,arr.ind=TRUE)
PO500.alldata <- Guate_Data97[ind.PO500.spp,]
PO500.sens.counts <- table(PO500.alldata$Sens)
PO500.sens.counts <- PO500.sens.counts[c("L","M","H")]
print(PO500.sens.counts)

ind.CF500.spp <- which(Guate_Data97$`CF500-tot`>0,arr.ind=TRUE)
CF500.alldata <- Guate_Data97[ind.CF500.spp,]
CF500.sens.counts <- table(CF500.alldata$Sens)
CF500.sens.counts <- CF500.sens.counts[c("L","M","H")]
print(CF500.sens.counts)


#######  1000 meters ########
ind.DF1000.spp <- which(Guate_Data97$`DF1k-tot`>0,arr.ind=TRUE)
DF1000.alldata <- Guate_Data97[ind.DF1000.spp,]
DF1000.sens.counts <- table(DF1000.alldata$Sens)
DF1000.sens.counts <- c(DF1000.sens.counts[c("L","M")],0)
names(DF1000.sens.counts)[3] <- "H"
print(DF1000.sens.counts)

ind.HT1000.spp <- which(Guate_Data97$`HT1k-tot`>0,arr.ind=TRUE)
HT1000.alldata <- Guate_Data97[ind.HT1000.spp,]
HT1000.sens.counts <- table(HT1000.alldata$Sens)
HT1000.sens.counts <- HT1000.sens.counts[c("L","M","H")]
print(HT1000.sens.counts)

ind.PO1000.spp <- which(Guate_Data97$`PO1k-tot`>0,arr.ind=TRUE)
PO1000.alldata <- Guate_Data97[ind.PO1000.spp,]
PO1000.sens.counts <- table(PO1000.alldata$Sens)
PO1000.sens.counts <- PO1000.sens.counts[c("L","M","H")]
print(PO1000.sens.counts)

ind.CF1000.spp <- which(Guate_Data97$`CF1k-tot`>0,arr.ind=TRUE)
CF1000.alldata <- Guate_Data97[ind.CF1000.spp,]
CF1000.sens.counts <- table(CF1000.alldata$Sens)
CF1000.sens.counts <- CF1000.sens.counts[c("L","M","H")]
print(CF1000.sens.counts)

all.sens.counts <- rbind(
  DF.sens.counts,
  HT.sens.counts,
  PO.sens.counts,
  CF.sens.counts,
  DF0.sens.counts,
  HT0.sens.counts,
  PO0.sens.counts,
  CF0.sens.counts,
  DF500.sens.counts,
  HT500.sens.counts,
  PO500.sens.counts,
  CF500.sens.counts,
  DF1000.sens.counts,
  HT1000.sens.counts,
  PO1000.sens.counts,
  CF1000.sens.counts)

Forest <- as.factor(rep(c("DF", "HT", "PO", "CF"), 4))
Locale <- as.factor(rep(c("Overall", "0 mts", "500 mts", "1000 mts"), each=4))

Sensitivity.df <- data.frame(all.sens.counts, Forest=Forest, Locale=Locale)
write.csv(Sensitivity.df, "~/Dropbox/Lia-IA-Enviro/Data/Sensitivtytable.csv")






#DF.per.sens <- split(x=DF.data, f=DF.data$Sens) 
#DF.L <- DF.per.sens$L
#DF.M <- DF.per.sens$M
#DF.H <- DF.per.sens$H