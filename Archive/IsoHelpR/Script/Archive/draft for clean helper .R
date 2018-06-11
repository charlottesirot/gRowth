library(R6)
source("/home/cha/Dropbox/Post-doc_Danemark/Results/Script/ChaTools.R")
require(gdata)

complementVector <- function(VectR, x) {

  length.VectR <- length(VectR)
  length.x <- length(x)

  y <- vector() #output

  for(i in 1:length.VectR) {

    if(is.na(match(VectR[i], x))) {
      y <- c(y, i)
    } else {}

  }

  return(VectR[y])

}

successiveValues <- function(vect) {

  length.vect <- length(vect)

  i <- 1
  k <- 1

  finalList <- list()

  while(i < length.vect) {

    finalList[k][[1]] <- vect[i]

    while(vect[i + 1] == vect[i] +1 & i < length.vect) {

      finalList[k][[1]] <- c(finalList[k][[1]], vect[i+1])

      i <- i+1

    }

    i <- i+1
    k <- k+1

  }

  return(finalList)

}

is.null.vector <- function(x) {
  sapply(x, is.null)
}

separateAnalysis_TCD <- function(data, column1) {

  vect <- column1

  length.vect <- length(vect)

  begin <- NULL
  end <- NULL

  k <- 1

  oneAnalyis <- list()

  for (i in 0:(length.vect-1)) {

    if(i != 0) {
      if(is.na(vect[i]) & !is.na(vect[i+1])) {
        end <- i

        oneAnalyis[[k]] <- begin:end

        begin <- i+1
        k <- k+1

      } else {}
    } else {
      begin <- 1
    }

  }

  length.OneAnalysis <- length(oneAnalyis)

  notNA <- which(is.na(vect))

  if(length(unlist(oneAnalyis)) != length.vect) {

    toTake <- length.vect - length(unlist(oneAnalyis))

    oneAnalyis[[length.OneAnalysis + 1]] <- (length(vect) - toTake +1) : length(vect)
  }

  oneAnalyisToReturn <- lapply(1:length(oneAnalyis), function(x) {
    data[oneAnalyis[[x]], ]
  })

  return(oneAnalyisToReturn)

}

TCDpath <- "/home/cha/Dropbox/Post-doc_Danemark/Results/Script/TCD.xls"
TCD <- read.xls (TCDpath, sheet = 1, header = TRUE)

Rank.Analysis <- 2: (nrow(TCD)+ 1) # +1 to make the excel file correspond to the R file

TCD <- cbind(Rank.Analysis, TCD)

nrow.TCD <- nrow(TCD)
ncol.TCD <- ncol(TCD)

##########################################
### error if row has no identifier.1
##########################################

Lineerror_identifier.1 <- which(is.null.vector(TCD$Identifier.1) | is.na(TCD$Identifier.1) | TCD$Identifier.1 == "NA" | TCD$Identifier.1 == "")

LinePeak.Nr <- which(TCD$Peak.Nr..Flash.TCD != 1 & TCD$Peak.Nr..Flash.TCD != 2 & !is.na(TCD$Peak.Nr..Flash.TCD) & TCD$Peak.Nr..Flash.TCD != "")

##########################################
### split into sample and standards tables
##########################################

level.identifier.1 <- levels(TCD$Identifier.1)

sample <- c("Sirot_Box 1")

place <- NULL

for(x in 1:length(sample)) {

  if(x == 1) {
    TCD.sample <- TCD[which(TCD$Identifier.1 == sample[x]), ]
    place <- which(TCD$Identifier.1 == sample[x])
  } else {
    TCD.sample <- rbind(TCD.sample, TCD[which(TCD$Identifier.1 == sample[x]), ])
    place <- c(place, which(TCD$Identifier.1 == sample[x]))
  }
}

place <- sort(place)
PlaceBar <- complementVector(1:nrow.TCD, place)

TCD.sample <- TCD.sample[order(TCD.sample$Rank.Analysis),] # data with only the sample
TCD.standard <- TCD [PlaceBar, ] # data with only the standards

##########################################
### check the data for sample ############
##########################################

# Condition of clean file for sample data
# 1/ identifier.2 not empty
# 2/ only the number 1 and 2 and NA in the Peak.Nr..Flash.TCD
# 2/ 3 values in a row with exactly two values
#

############ 1. identifier.2 not empty ############

Lineerror_identifier.2.Sample <- which(is.null.vector(TCD.sample$Identifier.2) | is.na(TCD.sample$Identifier.2) | TCD.sample$Identifier.2 == "NA" | TCD.sample$Identifier.2 == "")

############################################

listSample <- split(TCD.sample, TCD.sample$Identifier.2)

# delete if some level with 0 lines
place.To.delete <- NULL

for(x in 1:length(listSample)) {
  if(nrow(listSample[[x]]) == 0) {
    place.To.delete <- c(place.To.delete, x)
  }
}

listSample <- listSample[-place.To.delete]

## end of deletion

length.listSample <- length(listSample)

error.list.sample <- NULL # i.e. if the ConsideredSample is not a multiple of 3
error.Value.sample <- NULL # where is the value missing

for(x in 1:length.listSample){

  ConsideredSample <- listSample[[x]]

  list.analysis <- successiveValues(ConsideredSample$Rank.Analysis)

  n.Analysis <- length(list.analysis)

  for(i in 1:n.Analysis){

    dataTemp <- ConsideredSample[match(list.analysis[[i]], ConsideredSample$Rank.Analysis),]

      if(nrow(dataTemp) != 3) {
        error.list.sample <- c(error.list.sample, names(listSample)[x])
      } else {
        if(is.na(dataTemp$rArea.Flash.TCD[1])){
          error.Value.sample <- c(error.Value.sample, dataTemp$Rank.Analysis[1])
        }
        if(is.na(dataTemp$rArea.Flash.TCD[2])){
          error.Value.sample <- c(error.Value.sample, dataTemp$Rank.Analysis[2])
        }
        if(!is.na(dataTemp$rArea.Flash.TCD[3])){
          error.Value.sample <- c(error.Value.sample, dataTemp$Rank.Analysis[3])
        }
      }
  }
}

error.list.sample <- unique(error.list.sample)


##########################################
### check the data for standards ############
##########################################

# Condition of clean file for standards data
# 2/ only the number 1 and 2 and NA in the Peak.Nr..Flash.TCD
# 2/ 3 rArea Flash TCD in a row with exactly two values (except for the sucrose)
# ###############################

listStandard <- split(TCD.standard, TCD.standard$Identifier.1)

# delete if some level with 0 lines
place.To.delete <- NULL

for(x in 1:length(listStandard)) {
  if(nrow(listStandard[[x]]) == 0) {
    place.To.delete <- c(place.To.delete, x)
  }
}

listStandard <- listStandard[-place.To.delete]

## end of deletion

length.listStandard <- length(listStandard)

error.list.Standard <- NULL # i.e. if the ConsideredSample is not a multiple of 3
error.Value.Standard <- NULL # where is the value missing

for(x in 1:length.listStandard){

  ConsideredStandard <- listStandard[[x]]

  list.analysis <- successiveValues(ConsideredStandard$Rank.Analysis)

  n.Analysis <- length(list.analysis)

  for(i in 1:n.Analysis){

    dataTemp <- ConsideredStandard[match(list.analysis[[i]], ConsideredStandard$Rank.Analysis),]

    if(names(listStandard)[[x]] == "Sucrose" | names(listStandard)[[x]] == "sucrose") {
      if(nrow(dataTemp) != 2) {
        error.list.Standard <- c(error.list.Standard, names(listStandard)[x])
      } else {
        if(is.na(dataTemp$rArea.Flash.TCD[1])){
          error.Value.Standard <- c(error.Value.Standard, dataTemp$Rank.Analysis[1])
        }
        if(!is.na(dataTemp$rArea.Flash.TCD[2])){
          error.Value.Standard <- c(error.Value.Standard, dataTemp$Rank.Analysis[2])
        }
      }
    } else {
      if(nrow(dataTemp) != 3) {
        error.list.Standard <- c(error.list.Standard, names(listStandard)[x])
      } else {
        if(is.na(dataTemp$rArea.Flash.TCD[1])){
          error.Value.Standard <- c(error.Value.Standard, dataTemp$Rank.Analysis[1])
        }
        if(is.na(dataTemp$rArea.Flash.TCD[2])){
          error.Value.Standard <- c(error.Value.Standard, dataTemp$Rank.Analysis[2])
        }
        if(!is.na(dataTemp$rArea.Flash.TCD[3])){
          error.Value.Standard <- c(error.Value.Standard, dataTemp$Rank.Analysis[3])
        }
      }
    }
  }
}

error.list.Standard <- unique(error.list.Standard)

TCD.liste <- split(TCD, TCD$Identifier.1)

TCD.liste <- TCD.liste[which(names(TCD.liste) != "")]


sample <- c('Sirot_Box 1', "Blank")

dataProv <- TCD.liste[[i]]

One.Analysis <- separateAnalysis_TCD(dataProv, dataProv$rArea.Flash.TCD)

sapply(1:length(sample), function(x) {length(which(names(TCD.liste)[1] == sample[x]))})

