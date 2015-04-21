#
# Author: evberghe
###############################################################################

setwd("dssurvey")
source("readdata.R"); mydata <- readdata("data/Gegevens_Allemaal_150413.zip")

library(ggplot2)
library(ggmap)
library(dplyr)
library(reshape2)
library(NMF)

x11()

skills <- mydata[, names(mydata)[grepl("^Skills", names(mydata)) & !grepl("Other$", names(mydata))]]
names(skills) <- sub("^Skills", "", names(skills))
test1 <- apply(skills, 1, var)
skills <- skills[(test1>0), ]
test2 <- rep(0, nrow(skills))
for(i in 1:nrow(skills)) test2[i] <- sum(is.na(skills[i,]))
skills <- skills[test2==0,]
scaledSkills <- skills
for(i in 1:nrow(skills)) scaledSkills[i, ] <- rank(skills[i, ])
op <- par(mar = c(12,4,4,2) + 0.1)
cm <- cor(scaledSkills, use="complete.obs", method="pearson")

ggplot(melt(cm), aes(Var1, Var2, fill=value)) + geom_tile() +
		scale_fill_gradient2(limits=c(-1, 1)) +
		theme(axis.text.x=element_text(angle=-90, hjust=0))

aheatmap(cm)

d <- dist(scaledSkills)
h <- hclust(d)
plot(h)
dsScore <- apply(scaledSkills, 1, sum) # all sum to 253 (=sum(1:22)), as wanted

plot(hclust(dist(t(scaledSkills))))

dim(scaledSkills) # 489x22, which we'll factorise to 489x5 * 5x22
res <- nmf(scaledSkills, 5, nrun=512, seed=123456)
h <- coef(res); dim(h)
h <- t(scale(t(h))) # scaling - only done because Harris did it; seems to make little difference
H <- data.frame(t(h), Group=sapply(1:ncol(h), function(i) which.max(t(h)[i,])))
skillsH <- H
# group 1: Business, Prod, GIS, Surveys
# group 2: Stats, DataMan, Temp, Viz
# group 3: BEP, FEP, SQL, Admin
# group 4: Algorithms, BigData, Graphs, NoSQL
# group 5: BMCS, ML, Math, Optim, Science, Simulation
sGN <- c("Business", "Programming", "BigData", "Science", "Statistics")
# note: Harris' category "Math" now renamed as "Science"

w <- basis(res); dim(w)
w <- scale(w)
W <- data.frame(w, Group=sapply(1:nrow(w), function(i) which.max(w[i, ])))
skillsW <- W


########################################################################################
# now the types

types <- mydata[, names(mydata)[grepl("^Type", names(mydata)) & !grepl("Other$", names(mydata))]]
for(i in 1:ncol(types)) {
	types[, i] <- as.character(types[, i])
	types[types[, i]=="", i] <- "0" 
	types[types[, i]=="Maybe", i] <- "3" 
	types[types[, i]=="Maybe a bit", i] <- "2" 
	types[types[, i]=="Not at all", i] <- "1" 
	types[types[, i]=="Yes, but not completely", i] <- "4" 
	types[types[, i]=="Yes, definitely", i] <- "5"
	types[, i] <- as.numeric(types[, i])
}
colnames(types) <- sub("^Type", "", names(types))
colnames(types)[1] <- "Developer"
colnames(types)[5] <- "Statistician"
colnames(types)[6] <- "Jack-of-all-trades"
test1 <- apply(types, 1, var)
types <- types[(test1>0), ]
test2 <- apply(types, 1, sum)
types <- types[(test2>0), ]
scaledTypes <- types
for(i in 1:nrow(types)) scaledTypes[i, ] <- rank(types[i, ])

cm <- cor(scaledTypes, use="complete.obs", method="pearson")

ggplot(melt(cm), aes(Var1, Var2, fill=value)) + geom_tile() +
		scale_fill_gradient2(limits=c(-1, 1)) +
		theme(axis.text.x=element_text(angle=-90, hjust=0))

aheatmap(cm)

d <- dist(scaledTypes)
h <- hclust(d)
op <- par(mar = c(8,4,4,2) + 0.1)
plot(h, xlab="")

d <- dist(t(scaledTypes))
h <- hclust(d)
op <- par(mar = c(8,4,4,2) + 0.1)
plot(h, xlab="")

aheatmap(scaledTypes)

dim(scaledTypes) # 482x11, which we'll factorise to 482x4 * 4x11
res <- nmf(scaledTypes, 4, nrun=512, seed=123456)
h <- coef(res); dim(h)
h <- t(scale(t(h))) # scaling - only done because Harris did it; seems to make little difference
H <- data.frame(t(h), Group=sapply(1:ncol(h), function(i) which.max(t(h)[i,])))
typesH <- H
# group 1: Researcher, Scientist, Statistician
# group 2: Jack-of-all-trades, Hacker
# group 3: Artist, Leader, Business, Enterpreneur
# group 4: Developer, Engineer
tGN <- c("Researcher", "Hacker", "Business", "Developer")

w <- basis(res); dim(w)
w <- scale(w)
W <- data.frame(w, Group=sapply(1:nrow(w), function(i) which.max(w[i, ])))
typesW <- W


############################################################################################
# and finally the mosaic

# First create the relevant data frame, linking data from the skills (489) and types (482)

nrow(skillsW); nrow(typesW)
sW <- data.frame(respID=rownames(skillsW), sGroup=skillsW$Group); 
tW <- data.frame(respID=rownames(typesW), tGroup=typesW$Group)
jW <- merge(sW, tW, by="respID", all=FALSE)
jW$sGroup <- as.factor(jW$sGroup); levels(jW$sGroup) <- sGN
jW$sGroup <- factor(jW$sGroup, levels(jW$sGroup)[c(1, 3, 4, 2, 5)]) 
jW$tGroup <- as.factor(jW$tGroup); levels(jW$tGroup) <- tGN 
jW$tGroup <- factor(jW$tGroup, levels(jW$tGroup)[c(2, 3, 4, 1)]) 

mosaicplot(tGroup ~ sGroup, data=jW, 
		color=c("red", "orange", "yellow", "green", "blue"), 
		xlab="'type' group", ylab="'skills' group", 
		main="Mosaic: skills vs types",
		las=1)

###########################################################################################
# a couple of stats; 
# following http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf

library(gmodels)
library(vcd)
tble <- table(jW$sGroup, jW$tGroup)
CrossTable(tble, prop.t=FALSE, prop.r=FALSE, prop.c=FALSE)
chisq.test(tble)
assocstats(tble)

library(ca)
ca(tble) # alternatively ca(~sGroup + tGroup, data=jW)
plot(ca(tble), dim=c(1,2)) # dim here the default values
