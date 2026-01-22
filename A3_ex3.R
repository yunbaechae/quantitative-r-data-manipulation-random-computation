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

#Exercise 3
#1. Hypothesis
hypothesis <- function(x){
  dat <- read_csv(x,show_col_types = FALSE)
  cat("Data being tested: ", toString(x), "\nWe are testing:    H0 : B = 0 against H1 : B != 0","\nIs there a significant linear relationship between X and Y?", fill=FALSE, sep = "")
}

#2. Assumptions
#a)
assumptions <- function(x){
  dat <- read_csv(x,show_col_types = FALSE)
  D <- lm(Y ~ X,dat)
  a <- ggplot(dat)+geom_point(mapping=aes(x=X,y=Y))+geom_abline(intercept = D$coefficients[[1]],slope=D$coefficients[[2]],lwd=2)+ggtitle("a) Y vs X")

#b)
  b <- ggplot(dat)+geom_point(mapping=aes(x=D$fitted.values ,y=D$residuals)) + geom_hline(yintercept=0,lwd=2)+ggtitle("b) Residual plot")+ylab("Residuals")
  
#c)
  c <- ggplot(dat)+geom_histogram(mapping=aes(x=Y),bins=40) +ggtitle("c) Distribution is normal")
  
  print((a+b)/c)
}

#3. Fit
fit <- function(x){
  
  #Direct from lm
  dat <- read_csv(x,show_col_types = FALSE)
  B <- lm(Y~X,dat)$coefficients[[2]]
  df <- lm(Y~X,dat)$df.residual
  t <- qt(0.025,df,lower.tail=TRUE)
  p <- glance(lm(Y~X,dat))$p.value[[1]]
  SE <- coef(summary(lm(Y~X,dat)))[,"Std. Error"][[2]]
  
  #output
  out <- list(estimated_slope=B,CI=t*SE,t_value=t,degree_of_freedom=df,p_value=p)
  out$CI <- list(min=B-t*SE,max=B+t*SE)
  
  class(out) <- "mylm"
  out
}

#4. Decision
decision.mylm <- function(x){
  if(x$p_value<0.05){
    output <- cat("\nREJECT the Null Hypothesis.\n")
  } else {
    output <-  cat("\nDO NOT REJECT the Null Hypothesis.\n")
  }
  output
}

decision <- function(x){
  UseMethod("decision")
}

#5. Conclusion
conclusion.mylm <- function(x){
  if (x$p_value<0.05){
    output1 <- cat("As the p-value (" , x$p_value, ") is very small, we have sufficient evidence against H0, \ni.e. we have evidence that the slope parameter is significant and there is a relationship between X and Y." ,"\n" ,fill=TRUE, sep="")
  } else{
    output1 <- cat("As the p-value (" , x$p_value, ") is large, we do not have evidence to reject H0, \ni.e. we do not have evidence that the slope parameter is significant. We can not argue for any relationship between X and Y." ,"\n", fill=TRUE, sep="")
  }
  output1
}

conclusion <- function(x){
  UseMethod("conclusion")
}

#. Mytest
mytest <- function(x){
  hypothesis(x)
  assumptions(x)
  fit(x)
  fit1 <- fit(x)
  decision(fit1)
  #Please note: I could have written this one as decision.mylm. 
  #This is to demonstrate that decision(fit1) calls the method named decision.mylm()
  conclusion(fit1)
}


#For examiners
hypothesis("A3_Ex3_signif.csv")
assumptions("A3_Ex3_signif.csv")
fit("A3_Ex3_signif.csv")

#Just to show that I understand the updated assignment version!
fit1 <- fit("A3_Ex3_signif.csv")

decision.mylm(fit("A3_Ex3_signif.csv"))
#equivalent
decision.mylm(fit1)

conclusion.mylm(fit("A3_Ex3_signif.csv"))
#equivalent
conclusion.mylm(fit1)

#I can call the methods with these:
decision(fit1)
conclusion(fit1)

mytest("A3_Ex3_signif.csv")
mytest("A3_Ex3_not_signif.csv")

#For HD marks!
sloop::s3_class(fit1)
sloop::s3_dispatch(decision(fit1))
sloop::s3_get_method(decision.mylm)
