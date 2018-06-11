CN_EA.path <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/CNEA.xls"
CN_EA.data <- read.xls(xls = CN_EA.path, sheet = 1, header = TRUE)

separateAnalysis <- function(data, column1, column2) {

  vect1 <- which(is.na(column1))
  listPlace1 <- successiveValues(vect1)

  vect2 <- which(is.na(column2))
  listPlace2 <- successiveValues(vect2)

  if(length(listPlace1) == length(listPlace2)) {

    toReturn <-  lapply(1:length(listPlace1), function(x) {
      data[c(listPlace1[[x]],listPlace2[[x]]), ]
    })

  } else {
    toReturn <- NULL
  }
  return(toReturn)
}

Rank.Analysis <- 2: (nrow(CN_EA.data)+ 1) # +1 to make the excel file correspond to the R file

CN_EA.data <- cbind(Rank.Analysis, CN_EA.data)

nrow.CN_EA <- nrow(CN_EA.data)
ncol.CN_EA <- ncol(CN_EA.data)

level.identifier.1 <- levels(CN_EA.data$Identifier.1)

bad.Id1 <- which(CN_EA.data$Identifier.1 == "" | is.na(CN_EA.data$Identifier.1) | is.null(CN_EA.data$Identifier.1))

if(length(bad.Id1) != 0) {
  CN_EA.data <- CN_EA.data[-bad.Id1, ]
} else {}


CN_EA.data.liste <- split(CN_EA.data, CN_EA.data$Identifier.1)

CN_EA.data.liste <- CN_EA.data.liste[which(names(CN_EA.data.liste) != "")]

resultMatrix <- matrix(NA,ncol = 4) #the matrix summurizing the number of values for d13C and d15N
signature.Table <- matrix(NA,ncol = 6) # the final matrix with the signature

error.dilution <- vector()

for(i in 1:length(CN_EA.data.liste)) {

  dataProv <- CN_EA.data.liste[[i]]

  One.Analysis <- separateAnalysis(dataProv, column1 = dataProv$d.13C.12C, column2 = dataProv$d.15N.14N)

  number.Analysis <- length(One.Analysis)

  for(j in 1:number.Analysis) {

    One.Analysis[[j]][1:nrow(One.Analysis[[j]]), 1:10]

    ### deal with dilution

    dilution <- One.Analysis[[j]]$Sample.Dilution


    not0 <- which(dilution != 0)

    length.not0 <- length(which(duplicated(dilution[not0]) == FALSE))

    if(length.not0 != 1){

      error.dilution <- c(error.dilution, One.Analysis[[j]]$Rank.Analysis[1])

    } else {}

    d13C <- which(!is.na(One.Analysis[[j]]$d.13C.12C) & !is.null(One.Analysis[[j]]$d.13C.12C) & One.Analysis[[j]]$d.13C.12C != "")
    d15N <- which(!is.na(One.Analysis[[j]]$d.15N.14N) & !is.null(One.Analysis[[j]]$d.15N.14N) & One.Analysis[[j]]$d.15N.14N != "")

    toAdd <- c(names(CN_EA.data.liste)[i], One.Analysis[[j]]$Rank.Analysis[1], length(d13C), length(d15N))

    resultMatrix <- rbind(resultMatrix, toAdd)

    toAdd.signture <- c(names(CN_EA.data.liste)[i], One.Analysis[[j]]$Rank.Analysis[1], as.character(One.Analysis[[j]]$Identifier.2[1]), One.Analysis[[j]]$d.15N.14N[d15N[length(d15N)]], dilution[not0[1]], One.Analysis[[j]]$d.13C.12C[d13C[1]])

    signature.Table <- rbind(signature.Table, toAdd.signture)
  }
}

resultMatrix <- resultMatrix[-1,]

signature.Table <- signature.Table[-1,]

colnames(resultMatrix) <- c("identifier.1", "FirstLine", "Nb.d13C", "Nb.d15N")
resultMatrix <- as.data.frame(resultMatrix)

colnames(signature.Table) <- c("identifier.1", "FirstLine", "identifier.2","d13C", "dilution", "d15N")
rownames(signature.Table) <- seq(1:nrow(signature.Table))
signature.Table <- as.data.frame(signature.Table)

## detect the error

error.d13C <- which(as.numeric(as.character(resultMatrix$Nb.d13C)) == 0 | resultMatrix$Nb.d13C == "")
error.d15N <- which(as.numeric(as.character(resultMatrix$Nb.d15N)) == 0 | resultMatrix$Nb.d15N == "")

is.error.d13C <- length(error.d13C)
is.error.d15N <- length(error.d15N)

## detect weirdlines (i.e. different that 2 analysis for d13C and different than 3 or 4 for d15N)

length.error.dilution <- length(error.dilution)

weird.d13C <- which(as.numeric(as.character(resultMatrix$Nb.d13C)) != 2)
weird.d15N <- which(as.numeric(as.character(resultMatrix$Nb.d15N)) != 4 & as.numeric(as.character(resultMatrix$Nb.d15N)) != 3)

is.weird.d13C <- length(weird.d13C)
is.weird.d15N <- length(weird.d15N)


## plot

TypeChosen <- c("USG 40")

dataToPlot <- split(signature.Table, signature.Table$identifier.1)

dataTemp <- dataToPlot[which(names(dataToPlot) == TypeChosen)]

ylim.d13C  <- c(min(as.numeric(as.character(dataTemp[[1]]$d13C)), na.rm = T) - abs(min(as.numeric(as.character(dataTemp[[1]]$d13C)), na.rm = T))*0.1, max(as.numeric(as.character(dataTemp[[1]]$d13C)), na.rm = T) + abs(max(as.numeric(as.character(dataTemp[[1]]$d13C)), na.rm = T))*0.1)

boxplot.with.outlier.label(as.numeric(as.character(dataTemp[[1]]$d13C)), label_name = dataTemp[[1]]$identifier.2, ylim = ylim.d13C)
mtext("d13C", side = 1)

ylim.d15N  <- c(min(as.numeric(as.character(dataTemp[[1]]$d15N)), na.rm = T) - abs(min(as.numeric(as.character(dataTemp[[1]]$d15N)), na.rm = T))*0.1, max(as.numeric(as.character(dataTemp[[1]]$d15N)), na.rm = T) + abs(max(as.numeric(as.character(dataTemp[[1]]$d15N)), na.rm = T))*0.1)

boxplot.with.outlier.label(as.numeric(as.character(dataTemp[[1]]$d15N)), label_name = dataTemp[[1]]$identifier.2, ylim = ylim.d15N)
mtext("d15N", side = 1)

############################ isoSignature creatR

CN_EA.data.liste <- split(CN_EA.data, CN_EA.data$Identifier.1)

CN_EA.data.liste <- CN_EA.data.liste[which(names(CN_EA.data.liste) != "")]

signature.Table <- matrix(NA,ncol = 8) # the final matrix with the signature

for(i in 1:length(CN_EA.data.liste)) {

  dataProv <- CN_EA.data.liste[[i]]

  One.Analysis <- separateAnalysis(dataProv, column1 = dataProv$d.13C.12C, column2 = dataProv$d.15N.14N)

  number.Analysis <- length(One.Analysis)

  for(j in 1:number.Analysis) {

    dilution <- One.Analysis[[j]]$Sample.Dilution

    not0 <- which(dilution != 0)

    d13C <- which(!is.na(One.Analysis[[j]]$d.13C.12C) & !is.null(One.Analysis[[j]]$d.13C.12C) & One.Analysis[[j]]$d.13C.12C != "")
    d15N <- which(!is.na(One.Analysis[[j]]$d.15N.14N) & !is.null(One.Analysis[[j]]$d.15N.14N) & One.Analysis[[j]]$d.15N.14N != "")

    areaC <- One.Analysis[[j]]$Area.All[d13C[1]]
    areaN <- One.Analysis[[j]]$Area.All[d15N[1]]

    toAdd.signture <- c(names(CN_EA.data.liste)[i], One.Analysis[[j]]$Rank.Analysis[1], as.character(One.Analysis[[j]]$Identifier.2[1]), areaN, One.Analysis[[j]]$d.15N.14N[d15N[length(d15N)]], dilution[not0[1]], areaC, One.Analysis[[j]]$d.13C.12C[d13C[1]])

    signature.Table <- rbind(signature.Table, toAdd.signture)
  }
}

signature.Table <- signature.Table[-1,]

colnames(signature.Table) <- c("identifier.1", "FirstLine", "identifier.2", "Area.N", "d15N", "dilution", "Area.C", "d13C")
rownames(signature.Table) <- seq(1:nrow(signature.Table))
signature.Table <- as.data.frame(signature.Table)

blank <- c("Blank", "System blank")

sample <- c('Sirot_Box 1')

C.define <- c("Sucrose", "USG 40")

N.define <- c("N2", "USG 40")

nitrogen.table1 <- list()

for(i in 1:length(N.define)) {

  dataTemp <- signature.Table[which(signature.Table$identifier.1 == N.define[i]),]

  nitrogen.table1[[i]] <- dataTemp$d15N

}

carbon.table1 <- list()

for(i in 1:length(C.define)) {

  dataTemp <- signature.Table[which(signature.Table$identifier.1 == C.define[i]),]

  carbon.table1[[i]] <- dataTemp$d13C

}

nrow.table1 <- max(max.list(nitrogen.table1), max.list(carbon.table1)) + 5
ncol.table1 <- length(C.define) + length(N.define)+1

table1 <- as.data.frame(matrix(ncol = ncol.table1, nrow = nrow.table1)) ## Blank corrected delta values for isotope standards

table1[1,] <- c("Blank corrected delta values for isotope standards", rep("Nitrogen", length(N.define)), rep("Carbon", length(C.define)))
table1[2,] <- c(" ", N.define, C.define)

for(i in 1:length(N.define)) {

  table1[3:(2+length(nitrogen.table1[[i]])),i+1] <- as.character(nitrogen.table1[[i]])

}

for(i in 1:length(C.define)) {

  table1[3:(2+length(carbon.table1[[i]])),i+length(N.define)+1] <- as.character(carbon.table1[[i]])

}

table1[3:(nrow(table1)-3),1] <- " "

table1[(nrow(table1)-2) : nrow(table1), 1] <- c("Average", "Stdev", "True.Value")

for(i in 2:(length(C.define) + length(N.define) + 1)) {

  analysis <- as.numeric(as.character(table1[,i]))

  table1[(nrow.table1-2),i] <- mean(analysis, na.rm = T)
  table1[(nrow.table1-1),i] <- sd(analysis, na.rm = T)
}

#to replace by user value
table1[nrow.table1,(2:ncol.table1)] <- c(20.3, -4.5, -10.45, -26.39)

colnames(table1) <- c("Names", paste("Nitrogen", N.define, sep ="."), paste("Carbon", C.define, sep ="."))

#### creation of the standard table (i.e. called Blank correction of delta value in standards)

signature.Table <- signature.Table[order(as.numeric(as.character(signature.Table$FirstLine))),]

line.sample  <- lapply(1:length(sample), function(x) {
  which(signature.Table$identifier.1 == sample [x])
}) # the line of the signature.Table that correspond to the sample

line.sample <- unlist(line.sample)

table2.sample.Temp <- signature.Table[line.sample,]

table2.sample.Temp <- table2.sample.Temp[order(as.numeric(as.character(table2.sample.Temp$FirstLine))),]

line.standard <- complementVector(1:nrow(signature.Table), line.sample)

table2.standard.Temp <- signature.Table[line.standard,]

table2.standard.Temp <- table2.standard.Temp[order(as.numeric(as.character(table2.standard.Temp$FirstLine))),]

## table2.standard final (i.e. with corrected value)

table2.standard <- as.data.frame(matrix(ncol = 16, nrow = nrow(table2.standard.Temp)))

colnames(table2.standard) <- c("Box.No", "Box.position", "Sample.name", "sample.weight","Area.N", "d14N.d15N.ratio", "d14N.d15N.BlankCorrected", "Normalized.d14N.d15N", "At.N", "equipement.dilution", "Area.C", "d12C.d13C.ratio", "Area.C.Corrected", "d12C.d13C.BlankCorrected", "Normalized.d12C.d13C", "At.C")

table2.standard$Box.No <- " "
table2.standard$Box.position <- " "

table2.standard[,c(3,5,6)] <- cbind(as.character(table2.standard.Temp$identifier.1), as.numeric(as.character(table2.standard.Temp$Area.N)), as.numeric(as.character(table2.standard.Temp$d15N)))
table2.standard[,10:12] <- cbind(as.numeric(as.character(table2.standard.Temp$dilution)), as.numeric(as.character(table2.standard.Temp$Area.C)), as.numeric(as.character(table2.standard.Temp$d13C)))

blank.correction <- function(Area.blank, Sign.blank, Area.sample, Sign.sample) {
  value <- (Area.sample*Sign.sample - Area.blank*Sign.blank)/(Area.sample - Area.blank)
  return(value)
}

blankValue.N <- c(0,0)
blankValue.C <- c(2.15, -25.678)

toConvert <- c(5,6, 10, 11, 12)

for(i in 1:length(toConvert)) {
  table2.standard[,toConvert[i]] <-as.numeric(as.character(table2.standard[,toConvert[i]]))
}

 ## Blank correction d15N

blankCorrected.d15N <- sapply(1:nrow(table2.standard), function(x) {
  blank.correction(blankValue.N[1], blankValue.N[2], table2.standard$Area.N[x],table2.standard$d14N.d15N.ratio[x])
})

table2.standard$d14N.d15N.BlankCorrected <- blankCorrected.d15N

## Area Correction d13C

Area.correction <- function(value.toCorrect, dilution) {
  value <- value.toCorrect * (1+dilution/100)
  return(value)
}

Area.Corrected.d13C <- sapply(1:nrow(table2.standard), function(x) {
  Area.correction(table2.standard$Area.C[x], table2.standard$equipement.dilution[x])
})

table2.standard$Area.C.Corrected <- Area.Corrected.d13C

## blank correction d13C

blankCorrected.d13C <- sapply(1:nrow(table2.standard), function(x) {
  blank.correction(blankValue.C[1], blankValue.C[2], table2.standard$Area.C.Corrected[x],table2.standard$d12C.d13C.ratio[x])
})

table2.standard$d12C.d13C.BlankCorrected <- blankCorrected.d13C

## table2.sample final (i.e. with corrected value)

table2.sample <- as.data.frame(matrix(ncol = 16, nrow = nrow(table2.sample.Temp)))

colnames(table2.sample) <- c("Box.No", "Box.position", "Sample.name", "sample.weight","Area.N", "d14N.d15N.ratio", "d14N.d15N.BlankCorrected", "Normalized.d14N.d15N", "At.N", "equipement.dilution", "Area.C", "d12C.d13C.ratio", "Area.C.Corrected", "d12C.d13C.BlankCorrected", "Normalized.d12C.d13C", "At.C")

table2.sample[,1:2] <- cbind(as.character(table2.sample.Temp$identifier.1), as.character(table2.sample.Temp$identifier.2))
table2.sample[,5:6] <- cbind(as.numeric(as.character(table2.sample.Temp$Area.N)), as.numeric(as.character(table2.sample.Temp$d15N)))
table2.sample[,10:12] <- cbind(as.numeric(as.character(table2.sample.Temp$dilution)), as.numeric(as.character(table2.sample.Temp$Area.C)), as.numeric(as.character(table2.sample.Temp$d13C)))

toConvert <- c(5,6, 10, 11, 12)

for(i in 1:length(toConvert)) {
  table2.sample[,toConvert[i]] <-as.numeric(as.character(table2.sample[,toConvert[i]]))
}

# blank correction d15N

blankCorrected.d15N <- sapply(1:nrow(table2.sample), function(x) {
  blank.correction(blankValue.N[1], blankValue.N[2], table2.sample$Area.N[x],table2.sample$d14N.d15N.ratio[x])
})

table2.sample$d14N.d15N.BlankCorrected <- blankCorrected.d15N

## Area Correction d13C

Area.Corrected.d13C <- sapply(1:nrow(table2.sample), function(x) {
  Area.correction(table2.sample$Area.C[x], table2.sample$equipement.dilution[x])
})

table2.sample$Area.C.Corrected <- Area.Corrected.d13C

## blank correction d13C

blankCorrected.d13C <- sapply(1:nrow(table2.sample), function(x) {
  blank.correction(blankValue.C[1], blankValue.C[2], table2.sample$Area.C.Corrected[x],table2.sample$d12C.d13C.ratio[x])
})

table2.sample$d12C.d13C.BlankCorrected <- blankCorrected.d13C

######################## Normalisation (Karina's procedure) ######################

table2.sample.Karina <- table2.sample

Karina.Normalisation <- function(signature, true.Values, real.Values){
  coef <- (true.Values[1] - true.Values[2])/(real.Values[1]-real.Values[2])

  value <- (signature - real.Values[2]) * coef + true.Values[2]

  return(value)
}

norm.Karina.d15N <- sapply(1:nrow(table2.sample.Karina), function(x) {
  Karina.Normalisation(signature = table2.sample.Karina$d14N.d15N.BlankCorrected[x], true.Values = as.numeric(as.character(table1[nrow(table1),2:3])), real.Values = as.numeric(as.character(table1[(nrow(table1)-2),2:3])))
})

table2.sample.Karina$Normalized.d14N.d15N <- norm.Karina.d15N

At.d15N <- 100/(271.872114/(1+(table2.sample.Karina$Normalized.d14N.d15N/1000))+1)

table2.sample.Karina$At.N <- At.d15N

norm.Karina.d13C <-  sapply(1:nrow(table2.sample.Karina), function(x) {
  Karina.Normalisation(signature = table2.sample.Karina$d12C.d13C.BlankCorrected[x], true.Values = as.numeric(as.character(table1[nrow(table1),4:5])), real.Values = as.numeric(as.character(table1[(nrow(table1)-2),4:5])))
})

table2.sample.Karina$Normalized.d12C.d13C <- norm.Karina.d13C

At.d13C <- 100/(89.443838/(1+(table2.sample.Karina$Normalized.d12C.d13C/1000))+1)

table2.sample.Karina$At.C <- At.d13C

######################## Normalisation (Peter's procedure) ######################

table2.sample.Peter <- table2.sample

#Import the sample names and weight

weightPath <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/weight.csv"
weightData <- read.csv(weightPath, h = T, dec = ".", sep = ",")

nRow.weightData <- nrow(weightData)

errorToCount <- 0 # flag for error
line.flag <- NA

nrow.table2.sample <- nrow(table2.sample.Peter)

if(nrow.table2.sample != nRow.weightData) {
  errorToCount <- 1
} else {
  for(i in 1:nrow.table2.sample){
    if(table2.sample.Peter$Box.position[i] != as.character(weightData$Box.pos[i])){
      errorToCount <- 1
      line.flag <- table2.sample.Peter$Box.position[i]
    } else {}
  }
  if(errorToCount == 0) {
    table2.sample.Peter[,3:4] <- cbind(as.character(weightData$Name), weightData$Measured.Weigth..mg.)
  } else {}
}

Rank.Analysis <- seq(1,nrow(table2.sample.Peter))

table2.sample.Peter <- cbind(Rank.Analysis, table2.sample.Peter)

#do to
gelA <- "GEL A"

gelA.lines <- NULL

for(i in 1:length(gelA)) {

  gelA.lines <- c(gelA.lines, which(table2.sample.Peter$Sample.name == gelA[i]))

}

gelA.table <- table2.sample.Peter[gelA.lines,]

table.WithoutGelA <- table2.sample.Peter[complementVector(1:nrow(table2.sample.Peter), gelA.lines),]

## table of the dilution

dilution.Sample <- vector()

dilution.factor <- length(levels(as.factor(table.WithoutGelA$equipement.dilution)))

for(i in i:dilution.factor) {
  dilution.Sample[i] <- length(which(table.WithoutGelA$equipement.dilution == as.numeric(as.character(levels(as.factor(table.WithoutGelA$equipement.dilution))[i]))))
}

#ask the user about the dilution to take in account in the linear regression

chosenDilution <- 57

gelA.table.dilution <- gelA.table[which(gelA.table$equipement.dilution == chosenDilution), ]

##linear regression d15N

model.d15N <- lm(gelA.table.dilution$d14N.d15N.BlankCorrected ~ gelA.table.dilution$Rank.Analysis)

Intercept.N <- summary(model.d15N)$coefficients[1]
Slope.N <- summary(model.d15N)$coefficients[2]

Peter.Correction.d15N <- function(signature, rank, slope, intercept) {
  newValue <- signature - (slope * rank + intercept - 5.4)
  return(newValue)
}

Peter.Correction.d15N(table2.sample.Peter$d14N.d15N.BlankCorrected, table2.sample.Peter$Rank.Analysis, Slope.N, Intercept.N)

##linear regression d13C

model.d13C <- lm(gelA.table.dilution$d12C.d13C.BlankCorrected ~ gelA.table.dilution$Rank.Analysis)

Intercept.C <- summary(model.d13C)$coefficients[1]
Slope.C <- summary(model.d13C)$coefficients[2]

Peter.Correction.d13C <- function(signature, rank, slope, intercept) {
  newValue <- signature - (slope * rank + intercept + 21.8)
  return(newValue)
}

Peter.Correction.d13C(table2.sample.Peter$d12C.d13C.BlankCorrected, table2.sample.Peter$Rank.Analysis, Slope.C, Intercept.C)

