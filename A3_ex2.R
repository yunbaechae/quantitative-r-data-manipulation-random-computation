library(tidyverse)
library(ggplot2)
library(purrr)
library(readr)
library(patchwork)
library(glue)
library(stringr)
library(sloop)
library(broom)
library(mycor)
#I have made the code so that all you have to do is press the Run button. Thank you!

#Exercise 2
#0.
set.seed(2048)

#1.
X <- vector("list",100)
for(i in 1:100){
  X[[i]] <- list(paste("n",i,sep=""),rnorm(5,23,5))
}

#2.
str(X)

#3.
map_dbl(1:100, ~sum(X[[.x]][[2]]))

#4. I'd rather have to rely on a loop for generating a matrix.
#a. make a dummy matrix of size 100 X 5 filled with dummy integers 1.
M <- matrix(rep(1,500),100,5)
M

#b. change the values of the matrix M, as requested by the prompt.
for(i in 1:100){
  M[i,1] <- X[[i]][[2]][1]
  M[i,2] <- X[[i]][[2]][2]
  M[i,3] <- X[[i]][[2]][3]
  M[i,4] <- X[[i]][[2]][4]
  M[i,5] <- X[[i]][[2]][5]
}
M

#5. My plan is to run one main loop. I will use 5 variables for each column.
c1 <- 0
c2 <- 0
c3 <- 0
c4 <- 0
c5 <- 0

for(i in 1:100){
  c1 <- c1 + M[i,1]
  c2 <- c2 + M[i,2]
  c3 <- c3 + M[i,3]
  c4 <- c4 + M[i,4]
  c5 <- c5 + M[i,5]
}
sum <- c(c1,c2,c3,c4,c5)
sum
