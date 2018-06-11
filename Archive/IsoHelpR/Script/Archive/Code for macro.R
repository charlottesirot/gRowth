################### Project macro for isotopic analysis #######################

library(pracma)
library(tcltk2)
library(outliers)

# function to ask for the background values
inputs <- function(generalText, TextInput.1, TextInput.2){
  
  xvar <- tclVar("")
  yvar <- tclVar("")
  
  tt <- tktoplevel()
  tkwm.title(tt,paste("Choose", generalText))
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    x <- as.numeric(tclvalue(xvar))
    y <- as.numeric(tclvalue(yvar))
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  tkgrid(tklabel(tt,text=TextInput.1), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text=TextInput.2), y.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)
  
  tkwait.window(tt)
  return(c(x,y))
}

############################################################################################################################## 
########################################## for the QUANT file ##############################################
############################################################################################################################## 
##### Read the TCD file
 
#tkmessageBox(message = "Choose a quant file", type = "ok")
#TCDpath <- file.choose()

TCDpath <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/TCD.csv"

TCD <- read.csv(file = TCDpath, h = T, dec = ".", sep = ",")

Identifier.1 <- colnames(TCD)[3] # first identifer name (e.g. box name or standard used)

Identifier.2 <- colnames(TCD)[4] # Second identifer name (i.e. place of the sample in the box)

#### creation of sample table

identifier.2.NonVide <- which(!is.null(TCD[,Identifier.2]) & TCD[,Identifier.2] != "")

TCD_dataSample <- data.frame(TCD[identifier.2.NonVide,])

nbRow.TCD_dataSample <- nrow(TCD_dataSample)

id.previous.1 <- "NA"
id.previous.2 <- "NA"

temporary.TCDtable <- data.frame(matrix(NA, ncol = 4))

 k <- 1

for(x in 1:nbRow.TCD_dataSample) {
  
  id.1 <- as.character(TCD_dataSample[x,]$Identifier.1)
  id.2 <- as.character(TCD_dataSample[x,]$Identifier.2)
  
  if(!strcmp(id.1,id.previous.1) | !strcmp(id.2, id.previous.2)){
    
    toAdd <- c(id.1, id.2, TCD_dataSample[x,]$rArea.Flash.TCD, TCD_dataSample[x+1,]$rArea.Flash.TCD)
    
    temporary.TCDtable[k,] <- toAdd
    
    k <- k + 1 
  }
  
  id.previous.1 <- id.1
  id.previous.2 <- id.2
  
}
 
 nRow.temporary.TCDtable <- nrow(temporary.TCDtable)
 
 temporary.TCDtable <- cbind(seq(1:nRow.temporary.TCDtable), temporary.TCDtable)
 
 colnames(temporary.TCDtable) <- c("Analysis.Rank", colnames(TCD)[3:4], "Area.Flash.TCD.N", "Area.Flash.TCD.C")
 
 temporary.TCDtable$Area.Flash.TCD.N <- as.numeric(as.character(temporary.TCDtable$Area.Flash.TCD.N))
 temporary.TCDtable$Area.Flash.TCD.C <- as.numeric(as.character(temporary.TCDtable$Area.Flash.TCD.C))
 
##### Check the outliers

par(mfrow = c(1,2), mar = c(2,3,1.2,1.2))
boxplot(temporary.TCDtable$Area.Flash.TCD.N)
mtext("Area.Flash.TCD.N", side = 1)
boxplot(temporary.TCDtable$Area.Flash.TCD.C, xlab="Area.Flash.TCD.C")
mtext("Area.Flash.TCD.C", side = 1)
 
### upload weight
 
#tkmessageBox(message = "Choose the file containing the weight of your sample", type = "ok")
#weightPath <- file.choose()
 
weightPath <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/weight.csv"

weightData <- read.csv(weightPath, h = T, dec = ".", sep = ",")

nRow.weightData <- nrow(weightData)


###### Copy paste the column in temporary.TCDtable and check the validity
#############################################################################
### Be careful if this is missing one line put a blank line in the wieght table

error <- 0

if(nRow.temporary.TCDtable != nRow.weightData) {
  error <- 1
} else {
  for(i in 1:nRow.temporary.TCDtable){
    if(!strcmp(temporary.TCDtable[i,]$Identifier.2,as.character(weightData[i,]$Box.pos))){
      error <- 1
    }
  }
  temporary.TCDtable <- cbind(temporary.TCDtable, weightData$Name, weightData$Measured.Weigth..mg.)
  
  colnames(temporary.TCDtable)[6:7] <- c("sampleName", "SampleWeight")
}

########################## ask the value of the background to the user 

# backgroundValues <- inputs("background values", "N background", "C background")
backgroundValues <- c(0, 1167)

################################################################################################

################################################################################################
## correction of the value with the background
################################################################################################

corrected.N <- temporary.TCDtable$Area.Flash.TCD.N - backgroundValues[1]

corrected.C <- temporary.TCDtable$Area.Flash.TCD.C - backgroundValues[2]

temporary.TCDtable <- cbind(temporary.TCDtable, corrected.N, corrected.C)

colnames(temporary.TCDtable)[8:9] <- c("Corrected.Area.N", "Corrected.Area.C")

################################################################################################
## creation of the GelA table
################################################################################################

content.GelA <- inputs("N and C content in GelA", "N value", "C value")

GelA.table <- data.frame()

for(i in 1:nRow.temporary.TCDtable) {
  if(temporary.TCDtable[i,]$sampleName == "GEL A") {
    GelA.table <- rbind(GelA.table, temporary.TCDtable[i,])
  } else {}
}

colnames(GelA.table) <- colnames(temporary.TCDtable)

toPaste <- c(0,0, 0, backgroundValues, NA, 0, backgroundValues)

GelA.table <- rbind(toPaste, GelA.table)

GelA.table[1, 2:3] <- c("Blank", "Blank")

mg.N.perCup <- GelA.table$SampleWeight * content.GelA[1] * 1000
mg.C.perCup <- GelA.table$SampleWeight * content.GelA[2] * 1000

GelA.table <- cbind(GelA.table, mg.N.perCup, mg.C.perCup)

save(GelA.table, file = "GelA.table.csv")

### Linear regression ####

# 1. Nitrogen

model.N <- lm(GelA.table$Area.Flash.TCD.N ~ GelA.table$mg.N.perCup)

summary(model.N)
Intercept.N <- summary(model.N)$coefficients[1]
Slope.N <- summary(model.N)$coefficients[2]

plot(GelA.table$Area.Flash.TCD.N ~ GelA.table$mg.N.perCup, ylab = "Area.Flash.TCD.N", xlab = "mg.N.perCup")
abline(summary(model.N)$coefficients[1], summary(model.N)$coefficients[2], col = 'red')

# 2. Carbon

model.C <- lm(GelA.table$Area.Flash.TCD.C ~ GelA.table$mg.C.perCup)

summary(model.C)
Intercept.C <- summary(model.C)$coefficients[1]
Slope.C <- summary(model.C)$coefficients[2]
  
plot(GelA.table$Area.Flash.TCD.C ~ GelA.table$mg.C.perCup, ylab = "Area.Flash.TCD.C", xlab = "mg.C.perCup")
abline(summary(model.C)$coefficients[1], summary(model.C)$coefficients[2], col = 'red')


### calculation of µg N/Cup, µg C/Cup, mg N/kg sample, mg C/kg sample & C/N ####

Weight.N.perCup <- temporary.TCDtable$Corrected.Area.N/Slope.N
Weight.C.perCup <- temporary.TCDtable$Corrected.Area.C/Slope.C

Weight.N.kg <- Weight.N.perCup/temporary.TCDtable$SampleWeight*1000
Weight.C.kg <- Weight.C.perCup/temporary.TCDtable$SampleWeight*1000

C.N.Ratio <- Weight.C.kg/Weight.N.kg

temporary.TCDtable <- cbind(temporary.TCDtable,Weight.N.perCup, Weight.C.perCup, Weight.N.kg, Weight.C.kg, C.N.Ratio)

rpivotTable(temporary.TCDtable)

#Split the temporary.TCDtable in samples and GelA for only keeping the samples

TCDtable <- temporary.TCDtable[which(temporary.TCDtable$sampleName != "GEL A"),]

plot(TCDtable$Corrected.Area.N ~ TCDtable$SampleWeight)

############################################################################################################################## 
########################################## for the signature file ##############################################
############################################################################################################################## 

# 1. Import the data

CNEApath <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/CNEA.csv"

CNEA <- read.csv(file = CNEApath, h = T, dec = ".", sep = ",")

nrow.CNEA <- nrow(CNEA)

CNEA <- cbind(seq(1:nrow.CNEA), CNEA)

SplitData <- split(CNEA, CNEA$Identifier.1)

blankData <- rbind(SplitData$Blank, SplitData$`System blank`)

USG40Data <- SplitData$`USG 40`

sucroseData <- SplitData$Sucrose

N2Data <- SplitData$N2


# 2. Create the table of standards

determineC <- function(data) {
  
  NAPlace <- which(is.na(data$d.13C.12C))
  
  k <- vector() 
  
  for(i in 2: length(NAPlace)) {
    if (NAPlace[i] != NAPlace[i-1] + 1) {
      k <- c(k,NAPlace[i-1]+1)
    } else {}
  }
  
  if(NAPlace[length(NAPlace)] < length(data$d.13C.12C)){
    k <- c(k, NAPlace[length(NAPlace)] +1)
  }
  
  return(data$d.13C.12C[k])
  
}

determineN <- function(data) {
  
  NAPlace <- which(is.na(data$d.15N.14N))
  
  k <- vector() 
  
  
  if(NAPlace[1] > 1){
    k <- NAPlace[1]-1
  }
  
  for(i in 2: length(NAPlace)) {
    if (NAPlace[i] != NAPlace[i-1] + 1) {
      k <- c(k,NAPlace[i]-1)
    } else {}
  }
  
  return(data$d.15N.14N[k])
  
}

standardValue.C.Sucrose <- determineC(sucroseData)
standardValue.C.USG40 <- determineC(USG40Data)

standardValue.N.N2 <- determineN(N2Data)
standardValue.N.USG40 <- determineN(USG40Data)
























