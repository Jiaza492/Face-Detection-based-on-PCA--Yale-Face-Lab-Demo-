# Date: Sep 25th 2014
# By Ziang Jia
# W4240
# HomeWork 02 <Due: Sep 30th 2014>
# Problem 2

### preworks
#==============#
library(pixmap)
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




### a)
#==============#
## Find the total number of pixels in a picture
face_example <- read.pnm("CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
str(face_example)
face_example.matrix <- getChannels(face_example)
# this dimension would be used if we want to change the vector back
faces.matrix.dimension <- dim(face_example.matrix)

## read all of the pictures and store in a matrix
# initial a matrix with dim (38*4)*(192*168)
sample.matrix <- vector()
# load pictures and store in a matrix with each row as a picture
for(i in 1:length(fName_list)){
  for(j in 1:length(fileName_list)){
    #store pictures in a vector
    faces_temp.vector <- vector()
    # stringbuffer the fileDir then read it
    fileDir <- sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[fName_list[i]],dir_list[fName_list[i]],fileName_list[j])
    #print(fileDir)
    face_temp <- read.pnm(file  =fileDir)
    face_temp.matrix <- getChannels(face_temp)
    #print(dim(face_temp.matrix))
    # change the matrix into a vector
    face_temp.vector <- as.vector(face_temp.matrix)
    #print(length(face_temp.vector))
    # put this vector in sample matrix
    sample.matrix <- rbind(sample.matrix, face_temp.vector)
  }
}
print(dim(sample.matrix))

# Convert the matrix into a vector via as.vector and back with dim()
# Example:
# A = rbind(c(1,2,3),c(5,3,1))
# print(A)
# original.dimensions.A = dim(A)
# a = as.vector(A)
# print(a)
# dim(a) = original.dimensions.A
# print(a)




### b)
#==============#
## get "mean face" vector
mean_face.vector <- apply(sample.matrix,2,mean)

## center the sample.matrix with mean_face.vector
# use the self-defined function ctdata(). if u find something wrong, please re-run the function-code at the prework section
sample.matrix.centered <- ctdata(sample.matrix)
apply(sample.matrix.centered,2,mean)

# check
#sample.matrix.centered.1<-scale(sample.matrix,center=T,scale=F)
#apply(sample.matrix.centered.1,2,mean)

## convert back to original matrix use faces.matrix.dimensions above
dim(mean_face.vector) <- faces.matrix.dimension

## plot the mean face on the screen
mean_face <- pixmapGrey(mean_face.vector)
plot(mean_face)
title("Figure. Mean face")
filename = 'mean face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()



### c)
#===============#
## use prcomp() to run principal component(prcomp() does not require units at least as many as variables)
# run the prcomp()
pc_sample.result <- prcomp(sample.matrix, retx = T, center = T)
sample.scores <- pc_sample.result$x
sample.loadings <- pc_sample.result$rotation
sample.eigenvalues <- (pc_sample.result$sdev)^2
# check the dimemsion of scores and loadings 
print(dim(sample.scores))
print(dim(sample.loadings))

## plot the contribution of each principal component
# use the self-defined function namely pc.ctrb(). if u find something wrong, please re-run the function-code at the prework section
sample_contribution <- pc.ctrb(sample.eigenvalues)
plot(sample_contribution, xlab="first k principal components", ylab="cumulative contribution", main="Figure. Proportion of variance", type='b')




### d)
#===============#
## pick up the first 9 eigenvector from loadings, each column is a "eigenface"
# initial the matrix which store the 3*3 grid picture
eg_face.matrix <- vector()
# I do some improvement on homework 1, packaging it in a function namely grid.XX(), if u find something wrong, please re-run the function-code at the prework section
eg_face.matrix <- grid.XX(sample.loadings,3)

## plot eigenface
eg_face <- pixmapGrey(eg_face.matrix)
plot(eg_face)
title("Figure. Eigenfaces")
filename = 'eigenfaces.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()




### e)
#===============#
## Find the index of face yaleB01_P00A+010E+00.pgm
# dir_list[fName_list[1]],fileName_list[4], so it should be the fourth row in sample.matrix
face_yaleB01.pgm <- read.pnm("CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm")
plot(face_yaleB01.pgm)
## reconstruct a face 
## by adding in 1 bases at a time
mean_face.vector <- as.vector(mean_face.vector)
re_faces.matrix <- mean_face.vector
re_face.vector <- vector()
for(i in 1:24){
  if(i==1){
    re_face.vector <- mean_face.vector+sample.scores[4,1] * t(sample.loadings)[1,]
  }else{
    re_face.vector <- mean_face.vector+sample.scores[4,1:i] %*% t(sample.loadings)[1:i,]
  }
  print(length(re_face.vector))
  re_faces.matrix <- rbind(re_faces.matrix, re_face.vector)
  print(dim(re_faces.matrix))
}
re_faces.55.matrix <- grid.XX(t(re_faces.matrix),5)
# plot 
re_face.55 <- pixmapGrey(re_faces.55.matrix)
plot(re_face.55)
title("Figure. Use 24 Eigenfaces")
filename = 'eigenfaces_24.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

## by adding in 5 bases at a time
re_faces.matrix2 <- mean_face.vector
re_face.vector2 <- vector()
for(i in 1:24){
  re_face.vector2 <- mean_face.vector+sample.scores[4,1:(i*5)]%*%t(sample.loadings)[1:(i*5),]
  print(length(re_face.vector2))
  re_faces.matrix2 <- rbind(re_faces.matrix2, re_face.vector2)
  print(dim(re_faces.matrix2))
}
re_faces.55_2.matrix <- grid.XX(t(re_faces.matrix2),5)
# plot 
re_face.55_2 <- pixmapGrey(re_faces.55_2.matrix)
plot(re_face.55_2)
title("Figure. Use 120 Eigenfaces")
filename = 'eigenfaces_120.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()




### f)
#================#
## remove the picture and do principal component analysis
# dir_list[fName_list[5]],fileName_list[1:4], so it should be the [17:20] row in sample.matrix
sub_sample.matrix <- sample.matrix[-(17:20),]
sub_sample.matrix.mean <- apply(sub_sample.matrix,2,mean)
sub_sample.matrix.centered <- ctdata(sub_sample.matrix)
# run prcomp()
pc_sub_sample.result <- prcomp(sub_sample.matrix.centered, retx = T, center = T)
sub_sample.loadings <- pc_sub_sample.result$rotation
sub_sample.scores <- pc_sub_sample.result$x

## find the index of face yaleB05_P00A+010E+00.pgm
#
subject05.vector <- sample.matrix[20,]
subject05.vector.centered <- subject05.vector-sub_sample.matrix.mean
# calculate the scores of subject05.vector.centered
subject05.scores <- subject05.vector.centered %*% sub_sample.loadings

# use the fisrt 80 eigenface recontruct the picture
re_subject05.vector <- sub_sample.matrix.mean + subject05.scores[,1:80] %*% t(sub_sample.loadings)[1:80,]
dim(re_subject05.vector) <- faces.matrix.dimension

# plot
re_subject05 <- pixmapGrey(re_subject05.vector)

# compare the picture with original picture
par(mfrow=c(1,2))
plot(re_subject05)
face2 <- read.pnm("CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")
plot(face2)
#title("Figure. Compare the rec-face and the original one")
filename = 'compare_B05.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()



#################
# # End of Script
#################