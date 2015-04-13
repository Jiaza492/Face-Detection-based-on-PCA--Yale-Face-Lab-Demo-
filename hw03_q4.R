# Date: Oct 9th 2014
# By Ziang Jia
# W4240
# HomeWork 03 <Due: Oct 14th 2014>
# Problem 4

### preworks
#==============#
library(pixmap)
library(class)
setwd("D://R Workspace")

## get directory structure, store in a list namely dir_list
dir_list <- dir(path="CroppedYale/",all.files=FALSE)

## store the name of folder in a list namely fName_list 
fName_list <- c(1:38)

## store the name of files in a list namely fileName_list
fileName_list <- c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')

## define function ##
# used for centering the data set
ctdata<-function(data){
  ctdata<-matrix(0,nrow(data),ncol(data))
  cmean <- apply(data, 2, mean)
  for(i in 1:ncol(data)){
    ctdata[,i]<-data[,i]-cmean[i]
  }
  ctdata
}
## define end ##

## define function ##
# used for calculate the contribution of eigenvalues
pc.ctrb<-function(eigenvalues){
  ctrb_pc <- vector()
  trace_emp <- 0
  for(i in 1:length(eigenvalues)){
    trace_emp <- trace_emp + eigenvalues[i]
  }
  md_value <- 0
  for(i in 1: length(eigenvalues)){
    md_value <- md_value + eigenvalues[i]
    ctrb_pc[i] <- md_value/trace_emp
  }
  pc.ctrb<-ctrb_pc
}
## define end ##

## define function ##
# use for generate a X * X grid face matrix
grid.XX <- function(data,XX){
  grid.XX <- vector()
  for(i in 1:XX){
    # row matrix
    faces_temp.matrix <- vector()
    for(j in 1:XX){
      faces_temp.vector <- t(data)[(i-1)*XX+j,]
      dim(faces_temp.vector) <- faces.matrix.dimension
      # print(dim(faces_temp.vector))
      faces_temp.matrix <- cbind(faces_temp.matrix, faces_temp.vector)
      # print(dim(faces_temp.matrix))
    }
    
    grid.XX <- rbind(grid.XX, faces_temp.matrix)
    print(dim(grid.XX))
  }
  grid.XX
}
## define end ##

## define function ##
# use for calculate the error between training sample and testing sample
dis <- function(sample,sample1){
  error <- sample - sample1
  dis <- sqrt(t(error)%*%error)# Euclidean Distance
  dis
}
## define end ##



### a)
#==============#
## Find the total number of pixels in a picture
face_example <- read.pnm("CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
str(face_example)
face_example.matrix <- getChannels(face_example)
# this dimension would be used if we want to change the vector back
faces.matrix.dimension <- dim(face_example.matrix)
rm(face_example)
rm(face_example.matrix)

## read all of the pictures and store in a matrix
# initial a matrix with dim (38*4)*(192*168)
sample_4a.matrix <- vector()
names <- vector()
# load pictures and store in a matrix with each row as a picture
for(i in 1:length(fName_list)){
  for(j in 1:length(fileName_list)){
    #store pictures in a vector
    faces_temp.vector <- vector()
    # stringbuffer the fileDir then read it
    fileDir <- sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[fName_list[i]],dir_list[fName_list[i]],fileName_list[j])
    # Store the name
    fileName <- sprintf("%s",dir_list[fName_list[i]])
    names <- cbind(names,fileName)
    #print(fileDir)
    face_temp <- read.pnm(file=fileDir)
    face_temp.matrix <- getChannels(face_temp)
    #print(dim(face_temp.matrix))
    # change the matrix into a vector
    face_temp.vector <- as.vector(face_temp.matrix)
    #print(length(face_temp.vector))
    # put this vector in sample matrix
    sample_4a.matrix <- rbind(sample_4a.matrix, face_temp.vector)
  }
}

# row.names
row.names(sample_4a.matrix)<-names

## Store the size of the matrix 
sample_4a.size <- dim(sample_4a.matrix)
#print(sample_4a.size)

## 4/5 training, 1/5 testing
# Number of training obs
sample_4a.ntrain = floor(sample_4a.size[1]*4/5) 
# Number of testing obs
sample_4a.ntest = sample_4a.size[1]-sample_4a.ntrain 

## Set pseudo-random numbers to gets the same output
set.seed(1) 

## Store the indice of training data and testing data
ind.ntrain = sample(1:sample_4a.size[1],sample_4a.ntrain) 
ind.ntest = c(1:sample_4a.size[1])[-ind.ntrain] 

## output first five files' names in training data
row.names(sample_4a.matrix[ind.ntrain[1:5],])
## output first five files' names in testing data
row.names(sample_4a.matrix[ind.ntest[1:5],])


### b)
#==============#
## training data matrix and testing data matrix
sample_4a.matrix.train <- sample_4a.matrix[ind.ntrain,]
sample_4a.matrix.test <- sample_4a.matrix[ind.ntest,]

## center the data
sample_4a.matrix.train.center <- ctdata(sample_4a.matrix.train)
sample_4a.mean.train <- colMeans(sample_4a.matrix.train)

# center the testing data using meanface from training data
sample_4a.matrix.test.center <- vector()
for(i in 1:dim(sample_4a.matrix.test)[1]){
  sample_4a.matrix.test.center<-rbind(sample_4a.matrix.test.center,sample_4a.matrix.test[i,]-sample_4a.mean.train)
}

## do PCA on training data
sample_4a.train.pca <- prcomp(sample_4a.matrix.train.center, retx=T, center=T)
# scores
sample_4a.train.scores <- sample_4a.train.pca$x
#print(dim(sample_4a.train.scores))
# loadings
sample_4a.train.loadings <- sample_4a.train.pca$rotation
#print(dim(sample_4a.train.loadings))

## scores of testing data
sample_4a.test.scores <- sample_4a.matrix.test.center %*% sample_4a.train.loadings
# sample_4a.matrix.test.recon <- sample_4a.test.scores[,1:25] %*% t(sample_4a.train.loadings)[1:25,]
# row.names(sample_4a.matrix.test.recon) <- row.names(sample_4a.matrix.test)

## 1NN classification
# based on function dis()
count <- 0
for(i in 1:dim(sample_4a.test.scores)[1]){
  distance0 <- 1000000
  class0 <- "0"
  for(j in 1:dim(sample_4a.matrix.train.center)[1]){
    distance <- dis(sample_4a.test.scores[i,1:25], sample_4a.train.scores[j,1:25])
    # print(distance)
    if(distance < distance0){
      distance0 <- distance
      class0 <- row.names(sample_4a.matrix.train)[j]
    }
  }
  if(class0==row.names(sample_4a.matrix.test)[i]){
    print(1)
    count <- count + 1
  }else{
    print(0)
  }
}

# another way
cl<-factor(row.names(sample_4a.matrix.train))
for(i in 1: dim(sample_4a.test.scores)[1]){
  print(row.names(sample_4a.matrix.test)[i])
  class <- knn(sample_4a.train.scores[,1:25],sample_4a.test.scores[i,1:25],cl,k=1,l=0)
  print(class)
}





### c)
#==============#
# Use different lighting conditions
fileName_list = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# load your data and save the images as face_matrix_4a
sample_4c.matrix <- vector()
names <- vector()
# load pictures and store in a matrix with each row as a picture
for(i in 1:length(fName_list)){
  for(j in 1:length(fileName_list)){
    #store pictures in a vector
    faces_temp.vector <- vector()
    # stringbuffer the fileDir then read it
    fileDir <- sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[fName_list[i]],dir_list[fName_list[i]],fileName_list[j])
    # Store the name
    fileName <- sprintf("%s",dir_list[fName_list[i]])
    names <- cbind(names,fileName)
    #print(fileDir)
    face_temp <- read.pnm(file=fileDir)
    face_temp.matrix <- getChannels(face_temp)
    #print(dim(face_temp.matrix))
    # change the matrix into a vector
    face_temp.vector <- as.vector(face_temp.matrix)
    #print(length(face_temp.vector))
    # put this vector in sample matrix
    sample_4c.matrix <- rbind(sample_4c.matrix, face_temp.vector)
  }
}

# row.names
row.names(sample_4c.matrix)<-names

## Store the size of the matrix 
sample_4c.size <- dim(sample_4c.matrix)
#print(sample_4a.size)

## 4/5 training, 1/5 testing
# Number of training obs
sample_4c.ntrain = floor(sample_4c.size[1]*4/5) 
# Number of testing obs
sample_4c.ntest = sample_4c.size[1]-sample_4c.ntrain 

## Set pseudo-random numbers to gets the same output
set.seed(2) 

## Store the indice of training data and testing data
ind.ntrain = sample(1:sample_4c.size[1],sample_4c.ntrain) 
ind.ntest = c(1:sample_4c.size[1])[-ind.ntrain] 

## training data matrix and testing data matrix
sample_4c.matrix.train <- sample_4c.matrix[ind.ntrain,]
sample_4c.matrix.test <- sample_4c.matrix[ind.ntest,]

## center the data
sample_4c.matrix.train.center <- ctdata(sample_4c.matrix.train)
sample_4c.mean.train <- colMeans(sample_4c.matrix.train)

# center the testing data using meanface from training data
sample_4c.matrix.test.center <- vector()
for(i in 1:dim(sample_4c.matrix.test)[1]){
  sample_4c.matrix.test.center<-rbind(sample_4c.matrix.test.center,sample_4c.matrix.test[i,]-sample_4c.mean.train)
}

## do PCA on training data
sample_4c.train.pca <- prcomp(sample_4c.matrix.train.center, retx=T, center=T)
# scores
sample_4c.train.scores <- sample_4c.train.pca$x
#print(dim(sample_4a.train.scores))
# loadings
sample_4c.train.loadings <- sample_4c.train.pca$rotation
#print(dim(sample_4c.train.loadings))

## scores of testing data
sample_4c.test.scores <- sample_4c.matrix.test.center %*% sample_4c.train.loadings
# sample_4c.matrix.test.recon <- sample_4c.test.scores[,1:25] %*% t(sample_4c.train.loadings)[1:25,]
# row.names(sample_4c.matrix.test.recon) <- row.names(sample_4c.matrix.test)

## 1NN classification
# based on function dis()
par(mfrow=c(1,1))
count <- 0
for(i in 1:dim(sample_4c.test.scores)[1]){
  distance0 <- 1000000
  class0 <- "0"
  index <-0
  for(j in 1:dim(sample_4c.matrix.train.center)[1]){
    distance <- dis(sample_4c.test.scores[i,1:25], sample_4c.train.scores[j,1:25])
    # print(distance)
    if(distance < distance0){
      distance0 <- distance
      class0 <- row.names(sample_4c.matrix.train)[j]
      index <- j
    }
  }
  if(class0==row.names(sample_4c.matrix.test)[i]){
    print(class0)
    count <- count + 1
  }else{
    print(0)
    # draw out it
    face_misid.matrix0 <- sample_4c.matrix.test[i,]
    dim(face_misid.matrix0) <- faces.matrix.dimension
    face_misid.matrix1 <- sample_4c.matrix.train[index,]
    dim(face_misid.matrix1) <- faces.matrix.dimension
    face_misid.compare.matrix <- cbind(face_misid.matrix0, face_misid.matrix1)
    face_misid.compare <- pixmapGrey(face_misid.compare.matrix)
    filename = sprintf("%s.compare of %s and %s.png",i, row.names(sample_4c.matrix.test)[i],row.names(sample_4c.matrix.train)[index])
    plot(face_misid.compare, main=filename)
    dev.copy(device=png, file=filename, height=600, width=800)
    dev.off()
  }
}

# another way
cl<-factor(row.names(sample_4c.matrix.train))
for(i in 1: dim(sample_4c.test.scores)[1]){
  print(row.names(sample_4c.matrix.test)[i])
  class <- knn(sample_4c.train.scores[,1:25],sample_4c.test.scores[i,1:25],cl,k=1,l=0)
  print(class)
}

### d)
#==============#
## repeat test 10 times with different seed
# out put count of success
for(k in 3:12){
  ## 4/5 training, 1/5 testing
  # Number of training obs
  sample_4c.ntrain = floor(sample_4c.size[1]*4/5) 
  # Number of testing obs
  sample_4c.ntest = sample_4c.size[1]-sample_4c.ntrain 
  
  ## Set pseudo-random numbers to gets the same output
  set.seed(k) 
  
  ## Store the indice of training data and testing data
  ind.ntrain = sample(1:sample_4c.size[1],sample_4c.ntrain) 
  ind.ntest = c(1:sample_4c.size[1])[-ind.ntrain] 
  # print(ind.ntest)
  ## training data matrix and testing data matrix
  sample_4c.matrix.train <- sample_4c.matrix[ind.ntrain,]
  sample_4c.matrix.test <- sample_4c.matrix[ind.ntest,]
  
  ## center the data
  sample_4c.matrix.train.center <- ctdata(sample_4c.matrix.train)
  sample_4c.mean.train <- colMeans(sample_4c.matrix.train)
  
  # center the testing data using meanface from training data
  sample_4c.matrix.test.center <- vector()
  for(i in 1:dim(sample_4c.matrix.test)[1]){
    sample_4c.matrix.test.center<-rbind(sample_4c.matrix.test.center,sample_4c.matrix.test[i,]-sample_4c.mean.train)
  }
  
  ## do PCA on training data
  sample_4c.train.pca <- prcomp(sample_4c.matrix.train.center, retx=T, center=T)
  # scores
  sample_4c.train.scores <- sample_4c.train.pca$x
  #print(dim(sample_4a.train.scores))
  # loadings
  sample_4c.train.loadings <- sample_4c.train.pca$rotation
  #print(dim(sample_4c.train.loadings))
  
  ## scores of testing data
  sample_4c.test.scores <- sample_4c.matrix.test.center %*% sample_4c.train.loadings
  
  ## 1NN classification
  # based on function dis()
  count <- 0
  for(i in 1:dim(sample_4c.test.scores)[1]){
    distance0 <- 1000000
    class0 <- "0"
    index <-0
    for(j in 1:dim(sample_4c.matrix.train.center)[1]){
      distance <- dis(sample_4c.test.scores[i,1:25], sample_4c.train.scores[j,1:25])
      # print(distance)
      if(distance < distance0){
        distance0 <- distance
        class0 <- row.names(sample_4c.matrix.train)[j]
        index <- j
      }
    }
    if(class0==row.names(sample_4c.matrix.test)[i]){
      # print(1)
      count <- count + 1
    }else{
      # print(0)
    }
  }
  print(count)
}




#################
# End of Script
#################


