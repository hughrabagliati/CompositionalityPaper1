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
	d <- df$data$trialdata[1:2,]   ##df$data$trialdata$key_press %in% c(71,32),]
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
	#d <- d[d$stims$Cond %in% c("Mismatch-Mask-List","Mismatch-List","Mismatch-Mask-Adj", "Mismatch-Color", "Mismatch-Noun" ,"Match-Mask-List","Match-List","Match-Mask-Adj", "Match-Adj", "Mismatch-Disjunc", "Match-Noun", "Match-Color"),]
	#d <- d[d$stims$Cond %in% c("Match-Mask-List","Match-List","Match-Mask-Adj", "Match-Adj" ),]
	d$Cond <- as.factor(d$stims$Cond)
	d$Type <- as.factor(d$stims$Type)
	d$Phrase <- as.factor(d$stims$Phrase)
	d$Length <- as.factor(d$stims$Length)
	d$Task <- "Phrase"
	d$Task <- as.factor(d$Task)
	d$Stim <- "Two Words"
	d[d$Length == "1",]$Stim <- "One Word"
	d$Stim <- ordered(d$Stim, levels = c("Two Words", "One Word"))
	d$PicType <- ifelse(d$Type == "Color", "Variable Colors (as Experiment 1)","Fixed Colors")
	d$PicType <- ordered(d$PicType, levels = c("Variable Colors (as Experiment 1)", "Fixed Colors"))
	d$Match <- "Match"
	d[grep("mismatch",d$Cond, ignore.case = TRUE),]$Match <- "MisMatch"
	d$Block <- rep(c(1,2), each = 100)
	d$Match <- as.factor(d$Match)
	output <- data.frame(rt = as.numeric(as.character(d$rt)), key_press = d$key_press, Subj = d$Subj, Cond = d$Cond, Type = d$Type, Task = d$Task, Stim = d$Stim, Pic = d$stims$Pic, Phrase = d$Phrase, Match = d$Match, PicType = d$PicType, Length = d$Length, Block = d$Block)
	#print(summary(d))
	output$rt <- as.numeric(as.character(output$rt))
	comp = rbind(comp,output)
	print(x)
	}
	return(comp)
}

# Function for plotting data
Comp_Graph = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
DV.se <- DV.se/(sqrt( (length(unique(Subj))/2) ))
comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
if (leg == TRUE){
x <- barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab, legend = T, xpd = FALSE, names.arg = c("Mismatched Predictability\nPink Tree","Matched Predictability\nPink Tree"),  col = c("gray47"), density = c(40,100), args.legend = list(bty = "n", x = 5), tck = -0.01)
} else{
x <- barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab,  legend = F, xpd = FALSE, names.arg = c("Mismatched Predictability\nPink Tree","Matched Predictability\nPink Tree"),   col = c("gray47"), density = c(40,100), tick = FALSE, axes = FALSE)
axis(2, at = c(0.5,0.75,1), labels = c(0.5,0.75,1.0), tck = -0.03)
}
arrows(x, (c(comp.graph.mean) + c(comp.graph.se)+0.01), x, (c(comp.graph.mean) - c(comp.graph.se)-0.01), code = 0)
}


library(lme4)
library(ez)
catch <- Catch_Import("./Exp1c")
print(catch)
comp <- Comp_Import("./Exp1c")


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
	
comp$base_item <- as.factor(colsplit(comp$Pic,"_",names = c("prop1","prop2","prop3","prop4"))[,1])
contrasts(comp$Stim)[1] <- -1
contrasts(comp$Type)[1] <- -1
contrasts(comp$Stim)[2] <- 1
contrasts(comp$Type)[2] <- 1

# RT Analyses
lmer(rt ~ Stim * Type + (1+Stim|Subj)+ (1+Stim*Type|base_item) , data = subset(comp, Acc ==1 & Match == "Match")) -> exp1c.full
lmer(rt ~ Stim + Type + (1+Stim|Subj)+ (1+Stim*Type|base_item) , data = subset(comp, Acc ==1 & Match == "Match")) -> exp1c.noint
anova(exp1c.full, exp1c.noint)

lmer(rt ~ Stim  + (1+Stim|Subj)+ (1+Stim|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Type == "Color")) -> exp1c.full.color
lmer(rt ~ 1 + (1+Stim|Subj)+ (1+Stim|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Type == "Color")) -> exp1c.noint.color
anova(exp1c.full.color, exp1c.noint.color)

lmer(rt ~ Stim  + (1+Stim|Subj)+ (1+Stim|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Type != "Color")) -> exp1c.full.nocolor
lmer(rt ~ 1 + (1+Stim|Subj)+ (1+Stim|base_item) , data = subset(comp, Acc ==1 & Match == "Match" & Type != "Color")) -> exp1c.noint.nocolor
anova(exp1c.full.nocolor, exp1c.noint.nocolor)


ezANOVA(subset(comp, Acc ==1 & Match == "Match"), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "Color"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp, Acc ==1 & Match == "Match" & Type == "FixedColor"), rt, wid = .(Subj), within = .(Stim))$ANOVA

ezANOVA(comp, Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA

# LMER Analyis (Type between Subj, Stim within)
glmer(Acc ~ Type * Stim + (1+Stim|Subj) + (1+Type|base_item), data = comp, family = "binomial") -> exp1c.acc.full
glmer(Acc ~ Type + Stim + (1+Stim|Subj) + (1+Type|base_item), data = comp, family = "binomial") -> exp1c.acc.noint
anova(exp1c.acc.full, exp1c.acc.noint)

# Prep for bar graph
comp$DetailedTask <- "Matched Predictability (Pink Tree)"
comp[comp$Type == "Color",]$DetailedTask <- "Mismatched Predictability (Pink Tree)"
comp$DetailedTask <- ordered(comp$DetailedTask, levels = c("Mismatched Predictability (Pink Tree)","Matched Predictability (Pink Tree)"))

comp.rt <- summaryBy(rt + rtAdj ~ PicType + DetailedTask + Stim + Subj, , data = subset(comp, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
comp.rt <- summaryBy(rt + rtAdj ~ PicType + DetailedTask + Stim , data = comp.rt, FUN = c(mean,sd), na.rm = T )
print(comp.rt)
comp.Acc <- summaryBy(Acc + AccAdj~  PicType + DetailedTask + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc <- summaryBy(Acc + AccAdj~  PicType + DetailedTask + Stim   , data = comp.Acc, FUN = c(mean,sd), na.rm = T )
print(comp.Acc)

par(fig = c(0,1,0.35,1),mar = c(3,4,2,2))
Comp_Graph(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$DetailedTask, comp$Subj, paste("Two Words", "Reaction Time", sep = " "), c(650,900),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Comp_Graph(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$DetailedTask, comp$Subj, paste("Two Words", "Accuracy", sep = " "), c(0.5,1),"Accuracy")

# Prep for line graph
comp$DetailedTask <- ordered(comp$DetailedTask, levels = c("Matched Predictability (Pink Tree)","Mismatched Predictability (Pink Tree)"))
comp$Stim <- ordered(comp$Stim, levels = c("One Word", "Two Words"))

comp.rt <- summaryBy(rt + rtAdj ~ PicType + DetailedTask + Stim + Subj, , data = subset(comp, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)

# Show subject means
ggplot(comp.rt,aes(x = Stim, y = rt, color = Subj, group = Subj)) + facet_wrap(~DetailedTask)+
  geom_path()+guides(color = FALSE, group = FALSE)+ ylab("Reaction Time (ms)")+xlab("Stimulus Length")
  
ci.m <- aggregate(rt ~  Stim + DetailedTask , comp.rt, mean); ci.m
ci.l <- aggregate(rt ~  Stim + DetailedTask , comp.rt, ci.low); ci.l
ci.h <- aggregate(rt ~  Stim + DetailedTask , comp.rt, ci.high); ci.h
comp.rt <- summaryBy(rt + rtAdj ~ PicType + DetailedTask + Stim , data = comp.rt, FUN = c(mean,sd), na.rm = T )
comp.Acc <- summaryBy(Acc + AccAdj~  PicType + DetailedTask + Stim  +Subj, , data = comp, FUN = c(mean), na.rm = T , keep.names = T)
comp.Acc <- summaryBy(Acc + AccAdj~  PicType + DetailedTask + Stim   , data = comp.Acc, FUN = c(mean,sd), na.rm = T )

# Function for plotting data
Comp_Graph_l = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
theme_set(theme_bw())
S <- length(unique(Subj))/2
DV.se <- DV.se/(sqrt(S))
#comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
#comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
graph_data <- data.frame(Stim = IV1, Task = IV2, DV = DV.mean, SE = DV.se)
print((S))
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
        legend.position=c(0.43,0.8))
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
RT <- Comp_Graph_l(comp.rt$rt.mean,comp.rt$rtAdj.sd, comp.rt$Stim, comp.rt$DetailedTask, comp$Subj, paste("Two Words", "Reaction Time", sep = " "), c(650,1000),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Acc <- Comp_Graph_l(comp.Acc$Acc.mean,comp.Acc$AccAdj.sd, comp.Acc$Stim, comp.Acc$DetailedTask, comp$Subj, paste("Two Words", "Accuracy", sep = " "), c(0.7,1.05),"Accuracy")

# Get the gtables
gRT <- ggplotGrob(RT)
gAcc <- ggplotGrob(Acc)

# Set the widths
gAcc$widths <- gRT$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gAcc,gRT, nrow = 2, heights = c(1,2))