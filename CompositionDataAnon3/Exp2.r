library(ggplot2)
library(gridExtra)
library(grid)
library(ez)
library(lme4)
library(doBy)

library(doBy)
## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #- mean(x,na.rm=na.rm)}



Comp_Import_light= function(path_name){
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
	d$Item <- as.factor(d$stims$Item)
	d$Task <- "Phrase"
	if (length(d[grep("list",d$Cond, ignore.case = TRUE),]$Task) > 0){
		d[grep("list",d$Cond, ignore.case = TRUE),]$Task <- "List"
		}
	d$Task <- as.factor(d$Task)
	d$Stim <- "Two Words"
	d[grep("full-mask",d$Cond, ignore.case = TRUE),]$Stim <- "One Word"
	d[grep("no-mask",d$Cond, ignore.case = TRUE),]$Stim <- "Three Words"
	d$Stim <- ordered(d$Stim, levels = c("Three Words", "Two Words", "One Word"))
	d$PicType <- "Dark"
	d[grep("light_",d$Item, ignore.case = TRUE),]$PicType <- "Light"
	d$Match <- "Match"
	d[grep("mismatch",d$Cond, ignore.case = TRUE),]$Match <- "MisMatch"
	d$Match <- as.factor(d$Match)
	output <- data.frame(rt = as.numeric(as.character(d$rt)), key_press = d$key_press, Subj = d$Subj, Item = d$Item, Cond = d$Cond, Task = d$Task, Stim = d$Stim, Pic = d$stimulus, Match = d$Match, PicType = d$PicType)
	output$rt <- as.numeric(as.character(output$rt))
	comp = rbind(comp,output)
	
	print(x)
	}
	return(comp)
}



Comp_Import_Big = function(path_name){
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
	d$Item <- as.factor(d$stims$Item)
	d$Task <- "Phrase"
	if (length(d[grep("list",d$Cond, ignore.case = TRUE),]$Task) > 0){
		d[grep("list",d$Cond, ignore.case = TRUE),]$Task <- "List"
		}
	d$Task <- as.factor(d$Task)
	d$Stim <- "Two Words"
	d[grep("full-mask",d$Cond, ignore.case = TRUE),]$Stim <- "One Word"
	d[grep("no-mask",d$Cond, ignore.case = TRUE),]$Stim <- "Three Words"
	d$Stim <- ordered(d$Stim, levels = c("One Word", "Two Words", "Three Words"))
	d$PicType <- "Big"
	d[grep("small_",d$Item, ignore.case = TRUE),]$PicType <- "Small"
	d$Match <- "Match"
	d[grep("mismatch",d$Cond, ignore.case = TRUE),]$Match <- "MisMatch"
	d$Match <- as.factor(d$Match)
	output <- data.frame(rt = as.numeric(as.character(d$rt)), key_press = d$key_press, Subj = d$Subj, Item = d$Item, Cond = d$Cond, Task = d$Task, Stim = d$Stim, Pic = d$stimulus, Match = d$Match, PicType = d$PicType)
	output$rt <- as.numeric(as.character(output$rt))
	comp = rbind(comp,output)
	
	print(x)
	}
	return(comp)
}

comp_process = function(comp){
		#contrasts(comp$Stim) <- c(-0.5,0.5)
		#contrasts(comp$Task) <- c(-0.5,0.5)
		comp <- comp[comp$rt > 300 & comp$rt <1500,]
		comp$Acc <- 0
		comp[comp$key_press == 77 & comp$Match == "Match",]$Acc <- 1
		comp[comp$key_press == 90 & comp$Match == "MisMatch",]$Acc <- 1
		comp$Task <- factor(comp$Task, levels(comp$Task)[c(2,1)])
		comp$rtAdj <- NA
		comp$AccAdj <- NA
		for (i in unique(comp$Subj)){
			comp[comp$Subj == i,]$rtAdj <- ((comp[comp$Subj == i,]$rt - mean(comp[comp$Subj == i,]$rt, na.rm = T)) + mean(comp$rt, na.rm = T))
			comp[comp$Subj == i,]$AccAdj <- ((comp[comp$Subj == i,]$Acc - mean(comp[comp$Subj == i,]$Acc, na.rm = T)) + mean(comp$Acc, na.rm = T))
			}
	return(comp)
	}
	
# Function for plotting data
# Function for plotting data
Comp_Graph = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
DV.se <- DV.se/(sqrt(length(unique(Subj))))
comp.graph.mean <- tapply(DV.mean,list(IV1, IV2), mean)
comp.graph.se <- tapply(DV.se,list(IV1, IV2), mean)
if (leg == TRUE){
barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab, legend = T, xpd = FALSE, names.arg = c("Unstructured Composition\nBig Pink Tree","Structured Composition\nDark Pink Tree"),  col = c("gray47"), density = c(40,65,100), args.legend = list(bty = "n", x = 3.2), tck = -0.01)
} else{
barplot(comp.graph.mean, beside = T, ylim = ylimit, ylab = ylab,  legend = F, xpd = FALSE, names.arg = c("Unstructured Composition\nBig Pink Tree","Structured Composition\nDark Pink Tree"),   col = c("gray47"), density = c(40,65,100), tick = FALSE, axes = FALSE)
axis(2, at = c(0.5,0.75,1), labels = c(0.5,0.75,1.0), tck = -0.03)
}
arrows(c(1.5,2.5,3.5,5.5,6.5,7.5), (c(comp.graph.mean) + c(comp.graph.se)+0.01), c(1.5,2.5,3.5,5.5,6.5,7.5), (c(comp.graph.mean) - c(comp.graph.se)-0.01), code = 0)
}



comp.1 <- comp_process(Comp_Import_Big("./Exp2/Exp2Big/data"))
comp.1$Type <- "Big"
comp.2 <- comp_process(Comp_Import_light("./Exp2/Exp2Dark/data"))
comp.2$Type <- "Dark"

comp.omni <- rbind(comp.1,comp.2)
comp.omni$PicType = NA
for (colors in c("big", "small","dark","light")){
	comp.omni[grep(colors,comp.omni$Pic, ignore.case = TRUE),]$PicType <- colors
	}
comp.omni$PicType <- as.factor(comp.omni$PicType)

comp.omni$Type <- as.factor(comp.omni$Type)
summary(comp.omni)

#Reaction Times
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase"), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Stim != "Three Words"), rt, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA

ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Big"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Dark"), rt, wid = .(Subj), within = .(Stim))$ANOVA

ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Big" & Stim != "One Word"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Dark" & Stim != "One Word"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Big" & Stim != "Three Words"), rt, wid = .(Subj), within = .(Stim))$ANOVA
ezANOVA(subset(comp.omni, Acc ==1 & Match == "Match" & Task == "Phrase" & Type == "Dark" & Stim != "Three Words"), rt, wid = .(Subj), within = .(Stim))$ANOVA


# Accuracy
ezANOVA(comp.omni, Acc, wid = .(Subj), within = .(Stim), between = .(Type))$ANOVA


comp.omni$rtAdj <- NA
comp.omni$AccAdj <- NA
for (i in unique(comp.omni$Subj)){
	comp.omni[comp.omni$Subj == i,]$rtAdj <- ((comp.omni[comp.omni$Subj == i,]$rt - mean(comp.omni[comp.omni$Subj == i,]$rt, na.rm = T)) + mean(comp.omni$rt, na.rm = T))
	comp.omni[comp.omni$Subj == i,]$AccAdj <- ((comp.omni[comp.omni$Subj == i,]$Acc - mean(comp.omni[comp.omni$Subj == i,]$Acc, na.rm = T)) + mean(comp.omni$Acc, na.rm = T))
	}

# Prep for bar graph
comp.omni$DetailedType <- "Complex (Dark Pink Tree)"
comp.omni[comp.omni$Type == "Big",]$DetailedType <- "Simple (Big Pink Tree)"
comp.omni$DetailedType <- ordered(comp.omni$DetailedType, levels = c("Simple (Big Pink Tree)", "Complex (Dark Pink Tree)"))
comp.omni$Stim <- ordered(comp.omni$Stim, levels = c( "One Word","Two Words","Three Words"))

comp.omni.rt <- summaryBy(rt + rtAdj ~ Type+ DetailedType + Stim + Subj, , data = subset(comp.omni, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
comp.omni.rt <- summaryBy(rt + rtAdj ~ Type+ DetailedType + Stim , , data = comp.omni.rt, FUN = c(mean,sd), na.rm = T )
print(comp.omni.rt)

comp.omni.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType+ Stim  +Subj, , data = comp.omni, FUN = c(mean), na.rm = T , keep.names = T)
comp.omni.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType+ Stim  , , data = comp.omni.Acc, FUN = c(mean,sd), na.rm = T )
print(comp.omni.Acc)

par(fig = c(0,1,0.35,1),mar = c(3,4,2,2))
Comp_Graph(comp.omni.rt$rt.mean,comp.omni.rt$rtAdj.sd, comp.omni.rt$Stim, comp.omni.rt$Type, comp.omni$Subj, paste("Three Words", "Reaction Time", sep = " "), c(650,900),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Comp_Graph(comp.omni.Acc$Acc.mean,comp.omni.Acc$AccAdj.sd, comp.omni.Acc$Stim, comp.omni.Acc$Type, comp.omni$Subj, paste("Three Words", "Accuracy", sep = " "), c(0.5,1),"Accuracy")


# Prep for line graph
comp.omni$Stim <- ordered(comp.omni$Stim,  levels = c("One Word", "Two Words", "Three Words"))
comp.omni$DetailedType <- ordered(comp.omni$DetailedType, levels = c("Complex (Dark Pink Tree)","Simple (Big Pink Tree)"))
comp.omni.rt <- summaryBy(rt + rtAdj ~ Type+ DetailedType + Stim + Subj, , data = subset(comp.omni, Acc ==1 & Match == "Match"), FUN = c(mean), na.rm = T , keep.names = T)
ci.m <- aggregate(rt ~  Stim + DetailedType , comp.omni.rt, mean); ci.m
ci.l <- aggregate(rt ~  Stim + DetailedType , comp.omni.rt, ci.low); ci.l
ci.h <- aggregate(rt ~  Stim + DetailedType , comp.omni.rt, ci.high); ci.h

comp.omni.rt <- summaryBy(rt + rtAdj ~ Type+ DetailedType + Stim , , data = comp.omni.rt, FUN = c(mean,sd), na.rm = T )

comp.omni.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType+ Stim  +Subj, , data = comp.omni, FUN = c(mean), na.rm = T , keep.names = T)
comp.omni.Acc <- summaryBy(Acc + AccAdj~  Type + DetailedType+ Stim  , , data = comp.omni.Acc, FUN = c(mean,sd), na.rm = T )

# Function for plotting data
Comp_Graph_l = function(DV.mean, DV.se, IV1, IV2, Subj, title,ylimit,ylab,leg = FALSE){
theme_set(theme_bw())
DV.se <- DV.se/(sqrt((length(unique(Subj))/2)))
graph_data <- data.frame(Stim = IV1, Task = IV2, DV = DV.mean, SE = DV.se)
print((graph_data))
if (leg == TRUE){
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
        legend.position=c(0.2,0.8))
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
return(x)
}



RT <- Comp_Graph_l(comp.omni.rt$rt.mean,comp.omni.rt$rtAdj.sd, comp.omni.rt$Stim, comp.omni.rt$DetailedType, comp.omni$Subj, paste("Three Words", "Reaction Time", sep = " "), c(650,800),"Reaction Time (ms)",leg = TRUE)
par(fig = c(0,1,0,0.35),mar = c(3,4,2,2), new = TRUE)
Acc <- Comp_Graph_l(comp.omni.Acc$Acc.mean,comp.omni.Acc$AccAdj.sd, comp.omni.Acc$Stim, comp.omni.Acc$DetailedType, comp.omni$Subj, paste("Three Words", "Accuracy", sep = " "), c(0.7,1),"Accuracy")



# Get the gtables
gRT <- ggplotGrob(RT)
gAcc <- ggplotGrob(Acc)

# Set the widths
gAcc$widths <- gRT$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gAcc, gRT, nrow = 2, heights = c(1,2))



