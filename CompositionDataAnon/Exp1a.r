library(reshape2)
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
comp <- comp[comp$rt > 300 & comp$rt <1500,]
comp$Acc <- 0
comp[comp$key_press == 77 & comp$Match == "Match",]$Acc <- 1
comp[comp$key_press == 90 & comp$Match == "MisMatch",]$Acc <- 1
comp$Task <- factor(comp$Task, levels(comp$Task)[c(2,1)])

# Calcs for within subj SEs
comp$rtAdj <- NA
comp$AccAdj <- NA
for (i in unique(comp$Subj)){
  comp[comp$Subj == i,]$rtAdj <- ((comp[comp$Subj == i,]$rt - mean(comp[comp$Subj == i,]$rt, na.rm = T)) + mean(comp$rt, na.rm = T))
  comp[comp$Subj == i,]$AccAdj <- ((comp[comp$Subj == i,]$Acc - mean(comp[comp$Subj == i,]$Acc, na.rm = T)) + mean(comp$Acc, na.rm = T))
}

comp$base_item <- as.factor(colsplit(comp$Item,"_",names = c("word1","word2"))[,2])
contrasts(comp$Stim)[1] <- -1
contrasts(comp$Task)[1] <- -1
contrasts(comp$Stim)[2] <- 1
contrasts(comp$Task)[2] <- 1
# RT Analyses

lmer(rt ~ Stim * Task + (1+Stim*Task|Subj)+ (1+Stim*Task|base_item) , data = subset(comp, Acc ==1 & Match == "Match")) -> exp1a.full
lmer(rt ~ Stim + Task + (1+Stim*Task|Subj)+ (1+Stim*Task|base_item) , data = subset(comp, Acc ==1 & Match == "Match")) -> exp1a.noint
anova(exp1a.full, exp1a.noint)

# Stim removed for convergence
lmer(rt ~ Stim  +  (1+Stim|Subj)+ (1|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Task == "Phrase")) -> exp1a.full.phrase
lmer(rt ~ 1 + (1+Stim|Subj)+ (1|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Task == "Phrase")) -> exp1a.noint.phrase
anova(exp1a.full.phrase, exp1a.noint.phrase)

lmer(rt ~ Stim  + (1+Stim|Subj)+ (1|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Task != "Phrase")) -> exp1a.full.nophrase
lmer(rt ~ 1 + (1+Stim|Subj)+ (1|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Task != "Phrase")) -> exp1a.noint.nophrase
anova(exp1a.full.nophrase, exp1a.noint.nophrase)

ezANOVA(subset(comp, Acc ==1 & Match == "Match" ), rt, wid = .(Subj), within = .(Stim, Task))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Task == "List"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Task != "List"), rt, wid = .(Subj), within = .(Stim))$ANOVA
# Acc Analyses
ezANOVA(comp, Acc, wid = .(Subj), within = .(Stim,Task))$ANOVA

# GLMER Analysis, Lots of RanEf removed for convergence
glmer(Acc ~ Stim * Task + (1+Stim + Task|Subj) , data = comp, family = "binomial") -> exp1a.acc.full
glmer(Acc ~ Stim + Task + (1+Stim+ Task|Subj) , data = comp, family = "binomial") -> exp1a.acc.noint
anova(exp1a.acc.full, exp1a.acc.noint)

# Prepare variables for bar graph
comp$DetailedTask <- "List (Cup,Tree)"
comp[comp$Task == "Phrase",]$DetailedTask <- "Phrase (Pink Tree)"
comp$DetailedTask <- ordered(comp$DetailedTask, levels = c("Phrase (Pink Tree)", "List (Cup,Tree)"))
comp$Stim <- ordered(comp$Stim, levels = c("Two Words", "One Word"))

comp.rt <- summaryBy(rt + rtAdj ~ DetailedTask + Stim  + Subj, , data = subset(comp, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
comp.rt <- summaryBy(rt + rtAdj ~ DetailedTask + Stim  , data = comp.rt, FUN = c(mean,sd), na.rm = T )
print(comp.rt)

comp.Acc <- summaryBy(Acc + AccAdj~  DetailedTask + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc <- summaryBy(Acc + AccAdj~  DetailedTask + Stim   , data = comp, FUN = c(mean,sd), na.rm = T )
print(comp.Acc)

par(fig = c(0,1,0.35,1),mar = c(3,4,2,2))
Comp_Graph(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$DetailedTask, comp$Subj, paste("Two Words", "Reaction Time", sep = " "), c(700,1000),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Comp_Graph(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$DetailedTask, comp$Subj, paste("Two Words", "Accuracy", sep = " "), c(0.5,1),"Accuracy")

# Prepare variables for bar graph
comp$DetailedTask <- ordered(comp$DetailedTask, levels = c("Phrase (Pink Tree)","List (Cup,Tree)"))
comp$Stim <- ordered(comp$Stim, levels = c("One Word", "Two Words"))

comp.rt <- summaryBy(rt + rtAdj ~ DetailedTask + Stim  + Subj, , data = subset(comp, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
# Print RT means and c.i.s
ci.m <- aggregate(rt ~  Stim + DetailedTask , comp.rt, mean); ci.m
ci.l <- aggregate(rt ~  Stim + DetailedTask , comp.rt, ci.low); ci.l
ci.h <- aggregate(rt ~  Stim + DetailedTask , comp.rt, ci.high); ci.h

comp.rt <- summaryBy(rt + rtAdj ~ DetailedTask + Stim  , data = comp.rt, FUN = c(mean,sd), na.rm = T )
comp.Acc <- summaryBy(Acc + AccAdj~  DetailedTask + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc <- summaryBy(Acc + AccAdj~  DetailedTask + Stim   , data = comp, FUN = c(mean,sd), na.rm = T )

# Function for plotting data as a ling graph
Comp_Graph_l = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
  theme_set(theme_bw())
  DV.se <- DV.se/(sqrt(length(unique(Subj))))
  #comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
  #comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
  graph_data <- data.frame(Stim = IV1, Task = IV2, DV = DV.mean, SE = DV.se)
  print((graph_data))
  if (leg == TRUE){
    #x <- barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab, legend = T, xpd = FALSE, names.arg = c("Composition Task\nPink Tree","List Task\nCup, Tree"),  col = c("gray47"), density = c(40,100), args.legend = list(bty = "n", x = 2.8), tck = -0.01)
    # PRetty ggplot2 code drawn from https://github.com/langcog/KTE/blob/master/full%20analysis%20all%20experiments.R
    
    #		
    x <- ggplot(graph_data, aes(x=Stim, y=DV,group = Task, linetype = Task)) + 
      ylim(ylimit) +
      ylab(ylab) +
      geom_line(aes(group = Task, linetype = Task),position=position_dodge(width=.1),stat="identity") + 
      geom_linerange(aes(ymin=DV - SE, ymax=DV + SE), position=position_dodge(width=.1))+ 
      
      guides(colour=guide_legend()) +
      theme(strip.background = element_rect(fill="#FFFFFF"), 
            strip.text = element_text(size=12), 
            axis.text = element_text(size=12),
            axis.title = element_text(size=14),
            legend.text = element_text(size=12),
            legend.key = element_blank(),
            legend.title=element_blank(),
            title = element_text(size=16),
            panel.grid = element_blank(),
            axis.title.x=element_blank(),
            legend.position=c(0.3,0.8))
  } else{
    x <- ggplot(graph_data, aes(x=Stim, y=DV,group = Task, linetype = Task)) + 
      ylim(ylimit) +
      ylab(ylab) +
      geom_line(aes(group = Task, linetype = Task),position=position_dodge(width=.1),stat="identity") + 
      geom_linerange(aes(ymin=DV - SE, ymax=DV + SE), position=position_dodge(width=.1))+
      theme(strip.background = element_rect(fill="#FFFFFF"), 
            strip.text = element_text(size=12), 
            axis.text = element_text(size=12),
            axis.title = element_text(size=14),
            title = element_text(size=16),
            panel.grid = element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.position= "none")}
  #arrows(x, (c(comp.graph.mean) + c(comp.graph.se)+0.01), x, (c(comp.graph.mean) - c(comp.graph.se)-0.01), code = 0)
  return(x)
}
RT <- Comp_Graph_l(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$DetailedTask, comp$Subj, paste("Two Words", "Reaction Time", sep = " "), c(700,1000),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Acc <- Comp_Graph_l(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$DetailedTask, comp$Subj, paste("Two Words", "Accuracy", sep = " "), c(0.7,1.03),"Accuracy")

# Get the gtables
gRT <- ggplotGrob(RT)
gAcc <- ggplotGrob(Acc)

# Set the widths
gAcc$widths <- gRT$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gAcc,gRT, nrow = 2, heights = c(1,2))