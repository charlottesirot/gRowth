##############################################################
#
# gRowth - 12/10/2016
#
# charlott.sirot@gmail.comr
#
#####################################################################



############################################################
############################################################
##################################### gRowth_data Class
############################################################
############################################################

gRowth_data <- R6Class("gRowth_data",
                         public = list(
                           name = NA, # A character string corresponding to the name of the considered replicate
                           data = NA, # the image
                           fPath = NA, # A character string corresponding the path of the raw data
                           flag = 0, #
                           axisCoord = NULL,
                           axisFunction = NULL,
                           IncrementPoint = NULL, #as user choose them
                           IncrementPointAxis = NULL, # put on the growth axis
                           IncrementFinal = NULL,
                           distanceFinalPixel = NULL,
                           distanceFinal = NULL,
                           widthIncrement = NULL,
                           scale = NULL,
                           nameScale = NULL,
                           NbPixelScale = NULL,
                           DistScale = NULL,
                           unitScale = NULL,
                           edge = NULL,
                           format = NULL,
                           setScale = function(name, pix, dist, unit){
                             self$nameScale <- name
                             self$NbPixelScale <- pix
                             self$DistScale <- dist
                             self$unitScale <- unit
                           },
                           setAxisCoord = function(x){
                             self$axisCoord <- x
                           },
                           setAxisFunction = function(CourbeCroiss){
                             EquaDroites = matrix(0,(length(CourbeCroiss)-1),2)
                             colnames(EquaDroites) = c("coef directeur","ordonne a l'origine")
                             for(i in 1:(length(CourbeCroiss)-1)){
                               EquaDroites[i,1] = (CourbeCroiss[[i]][2] - CourbeCroiss[[i+1]][2])/(CourbeCroiss[[i]][1]-CourbeCroiss[[i+1]][1])
                               EquaDroites[i,2] = ((CourbeCroiss[[i]][2] + CourbeCroiss[[i+1]][2]) - EquaDroites[[i]][1]*(CourbeCroiss[[i]][1] + CourbeCroiss[[i+1]][1]))/2
                               }
                             self$axisFunction <- EquaDroites
                           },
                           setIncrementPoint = function(pointGrowth){
                             self$IncrementPoint <- pointGrowth
                           },
                           putIncrementAxis = function(pointGrowth){

                             nvPoint <- lapply(1: length(pointGrowth), function(x){

                               point <- pointGrowth[[x]][1]

                               # de quel intervalle
                               intr <- lapply(1:(length(self$axisCoord)-1), function(i){
                                if(self$axisCoord[[i]][1] < point & point <= self$axisCoord[[i + 1]][1] | self$axisCoord[[i]][1] >= point & point > self$axisCoord[[i + 1]][1]) {

                                  dist <- abs((self$axisFunction[i,1]*pointGrowth[[x]][1]-pointGrowth[[x]][2]+self$axisFunction[i,2])/sqrt(self$axisFunction[i,1]*self$axisFunction[i,1] + 1))

                                  ret <- c(i,dist,self$axisFunction[i,1],self$axisFunction[i,2])
                                  names(ret)<- c("presence", "distance", "slope", "intercept")

                                  return(ret)

                                  } else {
                                    return(NULL)
                                  }
                               })



                               #calcul nouvelle coordonnées
                               t <- NULL

                               if(is.null(unlist(intr))){
                                 return(NULL)
                               } else {

                                 for(i in 1:length(intr)){
                                   if(!is.null(intr[[i]])){
                                     if(intr[[i]][2] == min(unlist(sapply(intr,function(x) x[2])))){
                                       t <- intr[[i]][1]
                                     } else {}
                                   }
                                 }

                                 a <- intr[[t]][3]
                                 b <- intr[[t]][4]

                                 aP <- -1/a
                                 bP <- pointGrowth[[x]][1]/a + pointGrowth[[x]][2]


                                 X <- (b - bP)/(aP - a)
                                 Y <- a*X+b

                                 if(self$axisCoord[[t]][1] < self$axisCoord[[t+1]][1]){

                                   if(X < self$axisCoord[[t]][1]){
                                     X <- self$axisCoord[[t]][1]
                                     Y <- self$axisCoord[[t]][2]

                                   } else {}
                                   if(X > self$axisCoord[[t+1]][1]){
                                     X <- self$axisCoord[[t + 1]][1]
                                     Y <- self$axisCoord[[t + 1]][2]
                                   } else {}

                                 } else if(self$axisCoord[[t]][1] > self$axisCoord[[t+1]][1]) {

                                 } else {

                                   if(X > self$axisCoord[[t]][1]){
                                     X <- self$axisCoord[[t]][1]
                                     Y <- self$axisCoord[[t]][2]
                                   }else {}

                                   if(X < self$axisCoord[[t+1]][1]){
                                     X <- self$axisCoord[[t + 1]][1]
                                     Y <- self$axisCoord[[t + 1]][2]
                                   }else {}

                                 }

                                 return(c(X,Y))
                               }


                             })

                             #pour ceux nuls
                             t <- which(sapply(1:length(nvPoint), function(x){
                               if(is.null(nvPoint[[x]])) T
                               else F
                             }) == T) ## numero à remplacer

                             if(length(t) > 0){
                               for(i in t){
                                 distance <- lapply(1:length(self$axisCoord), function(x){
                                   calculDistance(pointGrowth[[i]], self$axisCoord[[x]])
                                 })

                                 NbAxisCoord <- which(unlist(distance) == min(unlist(distance)))

                                 nvPoint[[i]] <- self$axisCoord[[NbAxisCoord]]
                                }
                               }

                             self$IncrementPointAxis <- nvPoint

                           },
                           setIncrementFinalPixel = function(){

                             final <- list()

                             for(i in 1:(length(self$axisCoord)-1)){

                               t <- c(self$axisCoord[[i]][1], self$axisCoord[[i]][2])

                               y <- c(self$axisCoord[[i+1]][1], self$axisCoord[[i+1]][2])

                               z <- self$calculDistance(t,y)

                               v <- c(self$axisFunction[i,1],self$axisFunction[i,2])

                               if( i != (length(self$axisCoord)-1)){
                                 if(self$axisCoord[[i]][1] < self$axisCoord[[i+1]][1]){

                                   n <- which(sapply(self$IncrementPointAxis,function(x) x[1]) >= self$axisCoord[[i]][1] & sapply(self$IncrementPointAxis,function(x) x[1]) < self$axisCoord[[i+1]][1])

                                   if(length(n) != 0){
                                     n <- lapply(n, function(k){
                                       self$IncrementPointAxis[[k]][1:2]
                                     })
                                   } else {
                                     n <- NULL
                                   }

                                   n <- n[order(sapply(n,'[[',1))]

                                 } else if(self$axisCoord[[i]][1] > self$axisCoord[[i+1]][1]) {

                                   n <- which(sapply(self$IncrementPointAxis,function(x) x[1]) < self$axisCoord[[i]][1] & sapply(self$IncrementPointAxis,function(x) x[1]) >= self$axisCoord[[i+1]][1])

                                   if(length(n) != 0){
                                     n <- lapply(n, function(k){
                                       self$IncrementPointAxis[[k]][1:2]
                                     })
                                   } else {
                                     n <- NULL
                                   }

                                   n <- n[order(sapply(n,'[[',1), decreasing = T)]

                                 } else {}
                               } else if (i == (length(self$axisCoord)-1)){

                                 if(self$axisCoord[[i]][1] < self$axisCoord[[i+1]][1]){

                                   n <- which(sapply(self$IncrementPointAxis,function(x) x[1]) >= self$axisCoord[[i]][1] & sapply(self$IncrementPointAxis,function(x) x[1]) <= self$axisCoord[[i+1]][1])

                                   if(length(n) != 0){
                                     n <- lapply(n, function(k){
                                       self$IncrementPointAxis[[k]][1:2]
                                     })
                                   } else {
                                     n <- NULL
                                   }

                                   n <- n[order(sapply(n,'[[',1))]

                                 } else if(self$axisCoord[[i]][1] > self$axisCoord[[i+1]][1]) {

                                   n <- which(sapply(self$IncrementPointAxis,function(x) x[1]) <= self$axisCoord[[i]][1] & sapply(self$IncrementPointAxis,function(x) x[1]) >= self$axisCoord[[i+1]][1])

                                   if(length(n) != 0){
                                     n <- lapply(n, function(k){
                                       self$IncrementPointAxis[[k]][1:2]
                                     })
                                   } else {
                                     n <- NULL
                                   }

                                   n <- n[order(sapply(n,'[[',1), decreasing = T)]

                                 } else {}

                               } else {}



                               final[[i]] <- list(i, t, y, z, v, n)
                               names(final[[i]]) <- c("N", "coorFirstPointSegment", "coorLastPointSegment", "sizeSegment", "funcSegment", "points")
                             }
                            names(final) <- paste("segmen", 1:(length(self$axisCoord)-1))

                            self$IncrementFinal <- final
                           },
                           calculDistance = function(x, y){

                             distance <- sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)

                             return(distance)
                           },
                           calculDistanceFinalPixel = function(){

                             mat <- matrix(NA, ncol = 2, nrow = length(self$IncrementPointAxis))
                             colnames(mat) <- c("incrementNumber", "DistanceFromNucleus")

                             k <- 1

                             for(i in 1:length(self$IncrementFinal)){
                               if(!is.null(self$IncrementFinal[[i]][[6]])){
                                 for(j in 1:length(self$IncrementFinal[[i]][[6]])){
                                   if(self$IncrementFinal[[i]][1] == 1){
                                     t <- calculDistance(self$IncrementFinal[[i]][[2]], self$IncrementFinal[[i]][[6]][[j]])
                                   } else{
                                     longPrec <- sum(sapply(1:(i-1), function(x){self$IncrementFinal[[x]][[4]]}))
                                     t <- calculDistance(self$IncrementFinal[[i]][[2]], self$IncrementFinal[[i]][[6]][[j]]) + longPrec
                                   }
                                   mat[k,] <- c(k,t)
                                   k <- k+1
                                 }
                               }

                             }

                             self$distanceFinalPixel <- mat

                           },
                           calculDistanceFinalM = function(facMult){

                             mat <- cbind(self$distanceFinalPixel[,1], self$distanceFinalPixel[,2]*self$DistScale/self$NbPixelScale * facMult)

                             self$distanceFinal <- mat

                             matBis <- matrix(NA, ncol = ncol(mat), nrow = nrow(mat))

                             for(i in 1:nrow(matBis)){
                               if(i == 1){
                                 matBis[i,] <- mat[i,]
                               } else {
                                 matBis[i,1] <- mat[i,1]
                                 matBis[i,2] <- mat[i,2] -  mat[i-1,2]
                               }
                             }

                             self$widthIncrement <- matBis

                           },
                           correctTable = function(x, facMult){

                             vect <- 1:length(self$IncrementPointAxis)
                             vect <- vect[-x]

                             self$IncrementPointAxis <- lapply(vect, function(x){self$IncrementPointAxis[[x]]})

                             self$setIncrementFinalPixel()
                             self$calculDistanceFinalPixel()
                             self$calculDistanceFinalM(facMult)
                           },
                           initialize = function(f, folder){

                             self$format <- str_sub(f, start = regexpr("\\.([[:alnum:]]+)$",f)[[1]], end = str_length(f))

                             self$name <- f

                             tem <- str_split(folder,  pattern = "/")

                             chemin <- paste0(paste0(tem[[1]][1:(length(tem[[1]])-1)], collapse = "/"), "/")

                             self$fPath <- chemin

                             datProv <- paste0(self$fPath,tem[[1]][length(tem[[1]])])

                             if(self$format == ".tif" | self$format == ".tiff" | self$format == ".TIF" | self$format == ".TIFF"){

                               img <- suppressWarnings(readTIFF(datProv))

                               names <- str_sub(folder, start = 1, end = regexpr("\\.([[:alnum:]]+)$",folder)[[1]]-1)

                               writeJPEG(img, target = paste0(names, "_Converted.jpeg"), quality = 1)

                               self$data <- load.image(paste0(names, "_Converted.jpeg"))

                               file.remove(paste0(names, "_Converted.jpeg"))

                             } else {
                               self$data <- load.image(datProv)
                             }


                           }
                         )

)#gRowth_data


############################################################
############################################################
################################# gRowth_project Class
############################################################
############################################################

gRowth_project <- R6Class("elementR_project",
                            public = list(
                              name = NA,  # A character string corresponding to the name of the project
                              folderPath = NA, # A character string corresponding to the path of the project
                              otolith = NA,
                              data = NA,
                              scale = NA, # custom or no
                              nameScale = NULL,
                              NbPixelScale = NULL,
                              DistScale = NULL,
                              unitScale = NULL,
                              flagDone = NULL,
                              tabFinalDistance = NULL,
                              tabFinalWidth = NULL,
                              error = NA,
                              setScale = function(name, pix, dist, unit){
                                self$nameScale <- name
                                self$NbPixelScale <- pix
                                self$DistScale <- dist
                                self$unitScale <- unit

                                for(i in 1:length(self$otolith)){
                                  self$data[[i]]$setScale(name, pix, dist, unit)
                                }
                              },
                              setFinalTable = function(facMult){

                                temp <- sapply(1:length(self$otolith), function(x){

                                  if(!is.null(self$data[[x]]$distanceFinal)){
                                    max(dim(self$data[[x]]$distanceFinal)[1])
                                  } else { NA }


                                  })

                                temp <- max(temp, na.rm = T)
                                
                                print(temp)

                                tabFinal <- matrix(NA, ncol = temp+5, nrow = length(self$otolith))
                                tabFinalWidth <- matrix(NA, ncol = temp+5, nrow = length(self$otolith))
                                
                                
                                print(2)

                                for(x in 1:length(self$otolith)){

                                  if(self$flagDone[x] == 1){
                                    if(!is.null(self$data[[x]]$axisCoord)){

                                      distance <- sum(sapply(1:(length(self$data[[x]]$axisCoord)-1), function(i){
                                        self$data[[x]]$calculDistance(self$data[[x]]$axisCoord[[i]], self$data[[x]]$axisCoord[[i+1]])
                                      })) * facMult[x]

                                      distanceBord <- distance*self$data[[x]]$DistScale/self$data[[x]]$NbPixelScale - self$data[[x]]$distanceFinal[dim(self$data[[x]]$distanceFinal)[1],2]

                                      tabFinal[x,1:(dim(self$data[[x]]$distanceFinal)[1]+5)] <- c(self$otolith[x], dim(self$data[[x]]$distanceFinal)[1], self$data[[x]]$edge, distanceBord, distance*self$data[[x]]$DistScale/self$data[[x]]$NbPixelScale, self$data[[x]]$distanceFinal[1:dim(self$data[[x]]$distanceFinal)[1],2])
                                      tabFinalWidth[x,1:(dim(self$data[[x]]$distanceFinal)[1]+5)] <- c(self$otolith[x], dim(self$data[[x]]$distanceFinal)[1], self$data[[x]]$edge, distanceBord, distance*self$data[[x]]$DistScale/self$data[[x]]$NbPixelScale, self$data[[x]]$widthIncrement[1:dim(self$data[[x]]$widthIncrement)[1],2])
                                    } else {

                                    }
                                  } else {

                                  }

                                }
                                
                                
                                print(3)

                                colnames(tabFinal) <- c("Name", "age", "coloredEdge?", "distanceToEdge", "totalLength", 1:temp)
                                colnames(tabFinalWidth) <- c("Name", "age", "coloredEdge?", "distanceToEdge", "totalLength", 1:temp)

                                self$tabFinalDistance <- tabFinal
                                self$tabFinalWidth <- tabFinalWidth


                              },
                              initialize = function(folderPath=NULL) {

                                self$folderPath <- folderPath

                                tempListe <- list.files(self$folderPath, recursive = T)

                                pattern <- ".tif|.TIF|.TIFF|.tiff|.png|.PNG|.bmp|.BMP|.jpg|.JPG|.JPEG|.jpeg"

                                self$otolith <- tempListe[grep(pattern, tempListe)]

                                if(!identical(self$otolith, character(0))){

                                  temp <- list.files(self$folderPath,  recursive = T, full.names = T)

                                  temp <- temp[grep(pattern, temp)]

                                  pb <- tkProgressBar("Progress bar", "Some information in %",
                                                      0, 100, 20)

                                  info <- sprintf("Import project", round(10))
                                  setTkProgressBar(pb, 10, sprintf("Data import (%s)", info), info)

                                  self$data <- lapply(1: length(self$otolith), function(f){

                                    info <- sprintf("Import project", f/length(self$otolith)*100)
                                    setTkProgressBar(pb, f/length(self$otolith)*100, sprintf("Data import (%s)", info), info)

                                    gRowth_data$new(self$otolith[f], temp[f])

                                    })

                                  info <- sprintf("Import project", 100)
                                  setTkProgressBar(pb, 100, sprintf("Data import (%s)", info), info)

                                  close(pb)

                                  names(self$data) < self$otolith
                                  self$flagDone <- rep(0, length(self$otolith))

                                  self$error <- 0

                                } else {
                                  self$error <- 1
                                }
                                }
                            )
)#gRowth_project

############################################################
############################################################
################################# gRowth_scale Class
############################################################
############################################################

gRowth_scale <- R6Class("gRowth_scale",
                       public = list(
                         name = NA, # A character string corresponding to the name of the considered replicate
                         data = NA, # the image
                         fPath = NA, # A character string corresponding the path of the raw data
                         scaleValue = NA, # real distance
                         scalePixel = NA, # nb of pixel,
                         unit = NA,
                         format = NA,
                         setName = function(x){
                           self$name <- x
                         },
                         setScaleValue = function(x){
                           self$scaleValue <- x
                         },
                         setScalePixel = function(x){
                           self$scalePixel <- x
                         },
                         setUnit = function(x){
                           self$unit <- x
                         },
                         initialize = function(fPath){

                           self$fPath <- fPath

                           self$format <- str_sub(self$fPath, start = regexpr("\\.([[:alnum:]]+)$",self$fPath)[[1]], end = str_length(self$fPath))

                           if(self$format == ".tif" | self$format == ".tiff" | self$format == ".TIF" | self$format == ".TIFF"){

                             img <- suppressWarnings(readTIFF(self$fPath))

                             names <- str_sub(self$fPath, start = 1, end = regexpr("\\.([[:alnum:]]+)$",self$fPath)[[1]]-1)

                             writeJPEG(img, target = paste0(names, "_Converted.jpeg"), quality = 1)

                             self$data <- load.image(paste0(names, "_Converted.jpeg"))

                             file.remove(paste0(names, "_Converted.jpeg"))

                           } else {

                             self$data <- load.image(self$fPath)

                           }
                         }
                       )

)#gRowth_data

