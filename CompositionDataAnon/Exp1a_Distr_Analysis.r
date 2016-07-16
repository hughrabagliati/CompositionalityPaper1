library(gamlss.dist)
library(retimes)
library(rstan)
library(ggplot2)

library(ggplot2)
library(gridExtra)
library(grid)
library(jsonlite)
library(ez)
# This script is used to read in all the csv files in a folder.

library(doBy)


## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #- mean(x,na.rm=na.rm)}

Catch_Import= function(path_name){
  library(jsonlite)
  
  list.files(path = path_name,full.names = T, pattern = ".txt") -> file_list
  comp = c()
  for (x in file_list){
    file_name = x
    df <- fromJSON(file_name)
    d <- df$data$trialdata[1:2,]   ##df$data[4]$trialdata$key_press %in% c(71,32),]
    d$Subj <- unique(df$data$trialdata[df$data$trialdata$Screen == "Real-Response",]$Subj)[2]
    output <- cbind(key = d$key_press,Subj = d$Subj)
    comp = rbind(comp,output)
    print(x)
  }
  return(comp)
}




Comp_Import = function(path_name){
  library(jsonlite)
  
  list.files(path = path_name,full.names = T, pattern = ".txt") -> file_list
  comp = c()
  for (x in file_list){
    file_name = x
    df <- fromJSON(file_name)
    d <- df$data$trialdata[df$data$trialdata$Screen == "Real-Response",]
    d <- d[d$stims$Cond %in% c("Mismatch-Mask-List","Mismatch-List","Mismatch-Mask-Adj", "Mismatch-Color", "Mismatch-Noun" ,"Match-Mask-List","Match-List","Match-Mask-Adj", "Match-Adj"),]
    #d <- d[d$stims$Cond %in% c("Match-Mask-List","Match-List","Match-Mask-Adj", "Match-Adj" ),]
    d$Cond <- as.factor(d$stims$Cond)
    d$Item <- as.factor(d$stims$Item)
    d$Task <- "List"
    d[d$Cond %in% c("Match-Adj", "Match-Mask-Adj","Mismatch-Mask-Adj", "Mismatch-Color", "Mismatch-Noun" ),]$Task <- "Phrase"
    d$Task <- as.factor(d$Task)
    d$Stim <- "One Word"
    d[d$Cond %in% c("Match-Adj", "Match-List","Mismatch-List","Mismatch-Color", "Mismatch-Noun"),]$Stim <- "Two Words"
    d$Stim <- ordered(d$Stim, levels = c("One Word", "Two Words"))
    d$Match <- "Match"
    d[d$stims$Cond %in% c("Mismatch-Mask-List","Mismatch-List","Mismatch-Mask-Adj", "Mismatch-Color", "Mismatch-Noun"),]$Match <- "MisMatch"
    d$Match <- as.factor(d$Match)
    output <- data.frame(rt = as.numeric(as.character(d$rt)), key_press = d$key_press, Subj = d$Subj, Item = d$Item, Cond = d$Cond, Task = d$Task, Stim = d$Stim, Match = d$Match)
    output$rt <- as.numeric(as.character(output$rt))
    comp = rbind(comp,output)
    print(x)
  }
  return(comp)
}

# Function for plotting data using bar plots
Comp_Graph = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
  DV.se <- DV.se/(sqrt( (length(unique(Subj))) ))
  comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
  comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
  if (leg == TRUE){
    x <- barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab, legend = T, xpd = FALSE, names.arg = c("Composition Task\nPink Tree","List Task\nCup, Tree"),  col = c("gray47"), density = c(40,100), args.legend = list(bty = "n", x = 5), tck = -0.01)
  } else{
    x <- barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab,  legend = F, xpd = FALSE, names.arg = c("Composition Task\nPink Tree","List Task\nCup Tree"),   col = c("gray47"), density = c(40,100), tick = FALSE, axes = FALSE)
    axis(2, at = c(0.5,0.75,1), labels = c(0.5,0.75,1.0), tck = -0.03)
  }
  arrows(x, (c(comp.graph.mean) + c(comp.graph.se)+0.01), x, (c(comp.graph.mean) - c(comp.graph.se)-0.01), code = 0)
}


library(lme4)
catch <- Catch_Import("./Exp1a")
print(catch)
comp <- Comp_Import("./Exp1a")
contrasts(comp$Stim) <- c(-0.5,0.5)
contrasts(comp$Task) <- c(-0.5,0.5)
comp$Acc <- 0
comp[comp$key_press == 77 & comp$Match == "Match",]$Acc <- 1
comp[comp$key_press == 90 & comp$Match == "MisMatch",]$Acc <- 1
comp$Task <- factor(comp$Task, levels(comp$Task)[c(2,1)])

comp<- subset(comp, rt <= 4000)
comp$N_Task <- ifelse(comp$Task == "Phrase",-1,1)
comp$N_Stim <- ifelse(comp$Stim == "One Word",-1,1)
comp$N_T_S_Interact <- comp$N_Task * comp$N_Stim

ggplot(comp,aes(x=rt,..density..,col=Stim))+ geom_freqpoly(alpha=1,lwd =1.5,binwidth=150)+xlab("Response Time (ms)")+facet_wrap(~Task)


# Initial values at 1
initf1 <- function() {
  list(beta = c(500,rep(0,3)), beta_t = c(100,rep(0,3)),beta_s = c(300,rep(0,3)))
}
stanDat_full <- list(rt = comp$rt,factor1 = comp$N_Task,factor2 = comp$N_Stim,factor3 = comp$N_T_S_Interact, N = nrow(comp), J = nlevels(as.factor(comp$Subj)), Subj = as.integer(as.factor(comp$Subj)))

eg_stan_full <- stan(file="fixEf_Interaction.stan",
                     data=stanDat_full,
                     iter=600, warmup = 250, chains = 1, init = initf1)

print(eg_stan_full, pars = c("beta","beta_s","beta_t"), probs = c(0.025,0.5,0.975))

