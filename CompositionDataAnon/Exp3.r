library(ggplot2)
library(gridExtra)
library(grid)
library(jsonlite)

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
	

	d <- d[ grep("match",d$stims$Cond, ignore.case = TRUE),]
	d$Cond <- as.factor(d$stims$Cond)
	d$Type <- as.factor(d$stims$Type)
	d$Phrase <- as.factor(d$stims$Phrase)
	d$Length <- as.factor(d$stims$Length)
	d$Task <- "Phrase"
	d$Task <- as.factor(d$Task)
	d$Stim <- "Two Words"
	d[d$Length == "1",]$Stim <- "One Word"
	d[d$Length == "3",]$Stim <- "Three Words"
	d$Stim <- ordered(d$Stim, levels = c("One Word", "Two Words", "Three Words"))
	d$PicType <- "Striped"
	d[grep("spotted",d$stims$Pic, ignore.case = TRUE),]$PicType <- "Spotted"
	d$Match <- "Match"
	d[grep("mismatch",d$Cond, ignore.case = TRUE),]$Match <- "MisMatch"
	d$Block = rep(c(1,2), each = 150)
	
	d$Match <- as.factor(d$Match)
	output <- data.frame(rt = as.numeric(as.character(d$rt)), key_press = d$key_press, Subj = d$Subj, Cond = d$Cond, Type = d$Type, Task = d$Task, Stim = d$Stim, Pic = d$stims$Pic, Phrase = d$Phrase, Match = d$Match, PicType = d$PicType, Block = d$Block, File = d$stimulus)
	#print(summary(d))
	output$rt <- as.numeric(as.character(output$rt))
	comp = rbind(comp,output)
	
	print(x)
	}
	return(comp)
}

# Function for plotting data
Comp_Graph = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
DV.se <- DV.se/(sqrt(length(unique(Subj))))
comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
if (leg == TRUE){
barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab, legend = T, xpd = FALSE, names.arg = c("Simple Structure\nBig Striped Tree","Complex Structure\nBig Striped Tree"),  col = c("gray47"), density = c(40,65,100), args.legend = list(bty = "n", x = 3.5), tck = -0.01)
} else{
barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab,  legend = F, xpd = FALSE, names.arg = c("Simple Structure\nBig Striped Tree","Complex Structure\nBig Striped Tree"),   col = c("gray47"), density = c(40,65,100), tick = FALSE, axes = FALSE)
axis(2, at = c(0.5,0.75,1), labels = c(0.5,0.75,1.0), tck = -0.03)
}
arrows(c(1.5,2.5,3.5,5.5,6.5,7.5), (c(comp.graph.mean) + c(comp.graph.se)+0.01), c(1.5,2.5,3.5,5.5,6.5,7.5), (c(comp.graph.mean) - c(comp.graph.se)-0.01), code = 0)
}


library(lme4)
library(ez)
catch <- Catch_Import("./Exp3")
print(catch)
comp <- Comp_Import("./Exp3")


comp <- subset(comp, Type %in% c("Adj3","Adj4", "Adv4"))



comp$Acc <- 0
comp[comp$key_press == 77 & comp$Match == "Match",]$Acc <- 1
comp[comp$key_press == 90 & comp$Match == "MisMatch",]$Acc <- 1
comp$Task <- factor(comp$Task, levels(comp$Task)[c(2,1)])
comp <- comp[comp$rt > 300 & comp$rt <1500,]


comp$rtAdj <- NA
comp$AccAdj <- NA
for (i in unique(comp$Subj)){
	comp[comp$Subj == i,]$rtAdj <- ((comp[comp$Subj == i,]$rt - mean(comp[comp$Subj == i,]$rt, na.rm = T)) + mean(comp$rt, na.rm = T))
	comp[comp$Subj == i,]$AccAdj <- ((comp[comp$Subj == i,]$Acc - mean(comp[comp$Subj == i,]$Acc, na.rm = T)) + mean(comp$Acc, na.rm = T))
	}


# Reaction Times
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type %in% c("Adj3","Adj4", "Adv4")), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type %in% c("Adj3","Adj4", "Adv4") & Stim != "One Word"), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type %in% c("Adj3","Adj4", "Adv4") & Stim != "Three Words"), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA

ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj3" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj4"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adv4"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj3" & Stim != "Three Words" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj4" & Stim != "Three Words" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adv4" & Stim != "Three Words" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj3" & Stim != "One Word" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj4" & Stim != "One Word" ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adv4" & Stim != "One Word" ), rt, wid = .(Subj), within = .(Stim))$ANOVA

# Accuracy
ezANOVA(subset(comp, Type %in% c("Adj3","Adj4", "Adv4")), Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Type %in% c("Adj3","Adj4", "Adv4") & Stim != "One Word"), Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Type %in% c("Adj3","Adj4", "Adv4") & Stim != "Two Words"), Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Type %in% c("Adj3","Adj4", "Adv4") & Stim != "Three Words"), Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA



# Prep for bar graph
comp$DetailedType <- "Complex Adjective (Big Spotted Tree)"
comp[comp$Type == "Adj4",]$DetailedType <- "Simple (Big Spotted Tree)"
comp[comp$Type == "Adv4",]$DetailedType <- "Complex Compound Adj. (Big Spotted Tree)"
comp$DetailedType <- ordered(comp$DetailedType, levels = c("Simple (Big Spotted Tree)","Complex Adjective (Big Spotted Tree)","Complex Compound Adj. (Big Spotted Tree)"))
comp$Stim <- ordered(comp$Stim, levels = c("Three Words", "Two Words","One Word"))

comp.rt1 <- summaryBy(rt + rtAdj ~ Type + DetailedType + Stim + Subj, , data = subset(comp, Acc ==1 & Match == "Match" & Type %in% c("Adj3","Adj4","Adv4")), FUN = c(mean), na.rm = T , keep.names = T)
comp.rt1$Type <- ordered(comp.rt1$Type, levels = c("Adj4", "Adj3","Adv4"))
comp.rt <- summaryBy(rt + rtAdj ~ Type + DetailedType  + Stim , data = comp.rt1, FUN = c(mean,sd), na.rm = T )
print(comp.rt)

comp.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType  + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc$Type <- ordered(comp.Acc$Type, levels = c("Adj4", "Adj3","Adv4"))
comp.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType  + Stim   , data = comp.Acc, FUN = c(mean,sd), na.rm = T )
print(comp.Acc)


par(fig = c(0,1,0.35,1),mar = c(3,4,2,2))
Comp_Graph(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$Type, comp$Subj, paste("Three Words", "Reaction Time", sep = " "), c(750,1050),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Comp_Graph(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$Type, comp$Subj, paste("Three Words", "Accuracy", sep = " "), c(0.5,1),"Accuracy")

# Quick analysis on whether the effect differs depending on whether the size of the image and the size of the texture are congruent
comp$Cong <- 0
comp[grep("big_big",comp$Pic, ignore.case = TRUE),]$Cong <- 1
comp[grep("small_small",comp$Pic, ignore.case = TRUE),]$Cong <- 1
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj3" & Cong ==1 ), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Adj3" & Cong ==0 ), rt, wid = .(Subj), within = .(Stim))$ANOVA


# Prep for line graph
comp$DetailedType <- ordered(comp$DetailedType, levels = c("Complex Adjective (Big Spotted Tree)","Complex Compound Adj. (Big Spotted Tree)","Simple (Big Spotted Tree)"))
comp$Stim <- ordered(comp$Stim, levels = c("One Word", "Two Words", "Three Words"))
comp.rt1 <- summaryBy(rt + rtAdj ~ Type + DetailedType + Stim + Subj, , data = subset(comp, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
ci.m <- aggregate(rt ~  Stim + DetailedType , comp.rt1, mean); ci.m
ci.l <- aggregate(rt ~  Stim + DetailedType , comp.rt1, ci.low); ci.l
ci.h <- aggregate(rt ~  Stim + DetailedType , comp.rt1, ci.high); ci.h
comp.rt1$Type <- ordered(comp.rt1$Type, levels = c("Adj4", "Adj3"))
comp.rt <- summaryBy(rt + rtAdj ~ Type + DetailedType  + Stim , data = comp.rt1, FUN = c(mean,sd), na.rm = T )
print(comp.rt)

comp.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType  + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc$Type <- ordered(comp.Acc$Type, levels = c("Adj4", "Adj3"))
comp.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType  + Stim   , data = comp.Acc, FUN = c(mean,sd), na.rm = T )
print(comp.Acc)
# Function for plotting data
Comp_Graph_l = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
theme_set(theme_bw())
DV.se <- DV.se/(sqrt((length(unique(Subj))/3)))
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
        legend.position=c(0.35,0.8))
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
RT <- Comp_Graph_l(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$DetailedType, comp$Subj, paste("Three Words", "Reaction Time", sep = " "), c(700,1050),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Acc <- Comp_Graph_l(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$DetailedType, comp$Subj, paste("Three Words", "Accuracy", sep = " "), c(0.7,1),"Accuracy")
# Get the gtables
gRT <- ggplotGrob(RT)
gAcc <- ggplotGrob(Acc)

# Set the widths
gAcc$widths <- gRT$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gAcc, gRT, nrow = 2, heights = c(1,2))