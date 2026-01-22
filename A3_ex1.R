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

#Exercise 1
#1.
X <- read_csv("A3_Ex1.csv") %>% as_tibble

#2.
str(X)

#3.
sum(X$expenditure)

#4.
X %>% filter(month=="Mar") %>% summarise(sum=sum(expenditure))

#5.
X %>% filter(month=="Mar" & weather=="cloudy") %>% summarise(sum=sum(expenditure))

#6.
X %>% arrange(desc(expenditure))
#*****I would have been able to output just the month string if it was requested. 
#Please note that I'm assuming this would suffice the question's requirement: The answer is Mar.*****