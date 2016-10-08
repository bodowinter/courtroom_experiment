## Bodo Winter
## Created: August 31,  2014; Edits October 23,  2014; October 24,  2014
## Added additional data for E6A & E6B on November 15,  2014; November 16,  2014
## Cleaned up code and looped, October 8, 2016
## Analysis of Courtroom Experiment

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

library(dplyr)

## Define function:

emptyplot <- function(x, y, xaxt = 'n', yaxt = 'n',
	ylab = '', xlab = '', type = 'n', main = '', ...) {
	plot(x, y, xaxt = xaxt, yaxt = yaxt, ylab = ylab,
		xlab = xlab, type = type, main = main, ...)
	}

## Load in data:

setwd('/Users/winterb/Research/josh_jury_courtroom/new_analysis/dataframes/')
options(stringsAsFactors = F)
all_files <- list.files()
obj_names <- gsub('.csv', '', all_files)
for (i in 1:length(all_files)) {
	this_file <- all_files[i]
	assign(obj_names[i], read.csv(this_file))
	}

## Description of experiments:

## E1 == 3D Version with no controls for font size
## (E2) == 3D Version with grey tables
# (chose not to talk about this one,  it's weird)
## E3 / 2 == 3D Version with orange labels
## E4 / 3 == 2D Version
## E5A / 4A == 3D Version with language
## E5B / 4B == 2D Version with language
## E6A / 5A == 3D Version with language + late reponse time
## E6B / 5B == 2D Version with language + late reponse time

## How much data per experiment:

for (i in 1:length(obj_names)) {
	this_obj <- get(obj_names[i])
	print(paste(obj_names[i],
		nrow(this_obj), sep = ' = '))
	}


##------------------------------------------------------------------
## Tabulate data:
##------------------------------------------------------------------

## Make summary tables:

for (i in 1:length(obj_names)) {
	this_obj <- get(obj_names[i])
	print(obj_names[i])
	table_name <- paste0(obj_names[i], '.tab')
	assign(x = table_name, value = table(this_obj$distance,
		this_obj$choice))
	print(get(table_name))
	}

## Make proportions:

E1.tab[1, ] <- E1.tab[1, ] / rowSums(E1.tab)[1]	# 3D
E1.tab[2, ] <- E1.tab[2, ] / rowSums(E1.tab)[2]
E1.tab
E2.tab[1, ] <- E2.tab[1, ] / rowSums(E2.tab)[1]	# 3D with grey tables
E2.tab[2, ] <- E2.tab[2, ] / rowSums(E2.tab)[2]
E2.tab
E3.tab[1, ] <- E3.tab[1, ] / rowSums(E3.tab)[1]	# 3D with orange labels
E3.tab[2, ] <- E3.tab[2, ] / rowSums(E3.tab)[2]
E3.tab
E4.tab[1, ] <- E4.tab[1, ] / rowSums(E4.tab)[1]	# 2D
E4.tab[2, ] <- E4.tab[2, ] / rowSums(E4.tab)[2]
E4.tab
E5A.tab[1, ] <- E5A.tab[1, ] / rowSums(E5A.tab)[1]	# 3D with alnguage
E5A.tab[2, ] <- E5A.tab[2, ] / rowSums(E5A.tab)[2]
E5A.tab
E5B.tab[1, ] <- E5B.tab[1, ] / rowSums(E5B.tab)[1]	# 2D with language
E5B.tab[2, ] <- E5B.tab[2, ] / rowSums(E5B.tab)[2]
E5B.tab
E6A.tab[1, ] <- E6A.tab[1, ] / rowSums(E6A.tab)[1]	# 3D orange with language + late
E6A.tab[2, ] <- E6A.tab[2, ] / rowSums(E6A.tab)[2]
E6A.tab
(E6B.tab <- table(E6B$distance, E6B$choice))		# 2D with language + late
E6B.tab[1, ] <- E6B.tab[1, ] / rowSums(E6B.tab)[1]
E6B.tab[2, ] <- E6B.tab[2, ] / rowSums(E6B.tab)[2]
E6B.tab


##------------------------------------------------------------------
## Coding predictors and performing logistic regression:
##------------------------------------------------------------------

## Coding (deviation coding):

E1 <- mutate(E1, distance = as.factor(distance),
	experience = as.factor(experience),
	room_orientation = as.factor(room_orientation))
contrasts(E1$distance) <- contr.sum(2) / 2
contrasts(E1$experience) <- contr.sum(2) / 2
contrasts(E1$room_orientation) <- contr.sum(2) / 2
E2 <- mutate(E2, distance = as.factor(distance),
	experience = as.factor(experience),
	room_orientation = as.factor(room_orientation))
contrasts(E2$distance) <- contr.sum(2) / 2
contrasts(E2$experience) <- contr.sum(2) / 2
contrasts(E2$room_orientation) <- contr.sum(2) / 2
E3 <- mutate(E3, distance = as.factor(distance),
	experience = as.factor(experience))
contrasts(E3$distance) <- contr.sum(2) / 2
contrasts(E3$experience) <- contr.sum(2) / 2
E4 <- mutate(E4, distance = as.factor(distance),
	experience = as.factor(experience),
	room_orientation = as.factor(room_orientation))
contrasts(E4$distance) <- contr.sum(2) / 2
contrasts(E4$experience) <- contr.sum(2) / 2
contrasts(E4$room_orientation) <- contr.sum(2) / 2
E5A <- mutate(E5A, distance = as.factor(distance),
	experience = as.factor(experience),
	language_perspective = as.factor(language_perspective),
	language_evidence = as.factor(language_evidence))
contrasts(E5A$distance) <- contr.sum(2) / 2
contrasts(E5A$experience) <- contr.sum(2) / 2
contrasts(E5A$language_perspective) <- contr.sum(2) / 2
contrasts(E5A$language_evidence) <- contr.sum(2) / 2
E5B <- mutate(E5B, distance = as.factor(distance),
	experience = as.factor(experience),
	language_perspective = as.factor(language_perspective),
	language_evidence = as.factor(language_evidence))
contrasts(E5B$distance) <- contr.sum(2) / 2
contrasts(E5B$experience) <- contr.sum(2) / 2
contrasts(E5B$language_perspective) <- contr.sum(2) / 2
contrasts(E5B$language_evidence) <- contr.sum(2) / 2
E6A <- mutate(E6A, distance = as.factor(distance),
	experience = as.factor(experience),
	language_perspective = as.factor(language_perspective),
	language_evidence = as.factor(language_evidence))
contrasts(E6A$distance) <- contr.sum(2) / 2
contrasts(E6A$experience) <- contr.sum(2) / 2
contrasts(E6A$language_perspective) <- contr.sum(2) / 2
contrasts(E6A$language_evidence) <- contr.sum(2) / 2
E6B <- mutate(E6B, distance = as.factor(distance),
	experience = as.factor(experience),
	language_perspective = as.factor(language_perspective),
	language_evidence = as.factor(language_evidence))
contrasts(E6B$distance) <- contr.sum(2) / 2
contrasts(E6B$experience) <- contr.sum(2) / 2
contrasts(E6B$language_perspective) <- contr.sum(2) / 2
contrasts(E6B$language_evidence) <- contr.sum(2) / 2

## Centering confidence and make 'choice' into factor:

for (i in 1:length(obj_names)) {
	assign(obj_names[i],
		mutate(get(obj_names[i]),
			confidence = confidence - mean(confidence),
			choice = as.factor(choice)))
	}

## Analysis:

summary(E1.mdl <- glm(choice ~ distance + confidence + experience +
	room_orientation + room_orientation:distance,
	E1, family = 'binomial'))
summary(E2.mdl <- glm(choice ~ distance + confidence + experience +
	room_orientation + room_orientation:distance,
	E2, family = 'binomial'))
summary(E3.mdl <- glm(choice ~ distance + confidence + experience,
	E3, family = 'binomial'))
summary(E4.mdl <- glm(choice ~ distance + confidence + experience +
	room_orientation + room_orientation:distance,
	E4, family = 'binomial'))
summary(E5A.mdl <- glm(choice ~ distance + confidence + experience +
	language_perspective + language_evidence +
	language_perspective:language_evidence,
	E5A, family = 'binomial'))
summary(E5B.mdl <- glm(choice ~ distance + confidence + experience +
	language_perspective + language_evidence +
	language_perspective:language_evidence,
	E5B, family = 'binomial'))
summary(E6A.mdl <- glm(choice ~ distance + confidence + experience +
	question_orientation +  language_perspective + language_evidence +
	language_perspective:language_evidence,
	E6A, family = 'binomial'))
summary(E6B.mdl <- glm(choice ~ distance + confidence + experience +
	question_orientation + language_perspective + language_evidence +
	language_perspective:language_evidence,
	E6B, family = 'binomial'))

## Combined analysis for E5A-E6B:

xdata <- cbind(data.frame(experiment = rep('E5A', nrow(E5A))),
	select(E5A, -bias))
xdata <- rbind(xdata,
	cbind(data.frame(experiment = rep('E5B', nrow(E5B))), 
	select(E5B, -bias)))
xdata <- rbind(xdata, cbind(data.frame(experiment = rep('E6A', nrow(E6A))), 
	select(E6A, -bias, -question_orientation)))
xdata <- rbind(xdata, cbind(data.frame(experiment = rep('E6B', nrow(E6B))), 
	select(E6B, -bias, -question_orientation)))

## Name the between-experiment factors:

xdata$dimensionality <- '2D'
xdata[xdata$experiment %in% c('E5A', 'E6A'), ]$dimensionality <- '3D'
xdata$delay <- 'none'
xdata[xdata$experiment %in% c('E6A', 'E6B'), ]$delay <- 'delayed'

## Make a big analysis for everything:

summary(bigmodel <- glm(choice ~ distance + confidence +
	experience + language_perspective + 
	language_evidence + language_perspective:language_evidence +
	dimensionality + delay +
	dimensionality:distance + delay:distance, 
		xdata, family = 'binomial'))

## Make an even bigger analysis:

xdata2 <- rbind(xdata, cbind(data.frame(experiment = rep('E3', nrow(E3)), 
	E3[, c('choice', 'distance', 'confidence', 'experience')], 
	data.frame(language_perspective = rep('no_language', nrow(E3)), 
	language_evidence = rep('no_language', nrow(E3)), 
	dimensionality = rep('3D', nrow(E3)), delay = rep('none', nrow(E3))))))
xdata2 <- rbind(xdata2, cbind(data.frame(experiment = rep('E4', nrow(E4)), 
	E4[, c('choice', 'distance', 'confidence', 'experience')], 
	data.frame(language_perspective = rep('no_language', nrow(E4)), 
	language_evidence = rep('no_language', nrow(E4)), 
	dimensionality = rep('2D', nrow(E4)), delay = rep('none', nrow(E4))))))

## Make a variable for the language effect:

xdata2$language <- 'no_language'
xdata2[xdata2$experiment %in% c('E5A', 'E5B', 'E6A', 'E6B'), ]$language <- 'language'

summary(bigmodel2 <- glm(choice ~ distance + confidence + experience + 
	dimensionality + delay + language + language:distance + 
	dimensionality:distance + delay:distance, 
		xdata2, family = 'binomial'))

## Make a table for the overall effect:

table(xdata2$choice, xdata2$distance)



##------------------------------------------------------------------
## Graph with averages across all effects:
##------------------------------------------------------------------

## April 23,  2015
## Adding a graph that averages all effects

## How many data points in total?

sum(sapply(obj_names[-2], FUN = function(x) nrow(get(x))))	# 970

## Add all on top of each other:

xtab <- table(E1$distance, E1$choice)
for (i in 1:length(obj_names[-2])) {
	this_obj <- get(obj_names[-2][i])
	xtab <- xtab + table(this_obj$distance, this_obj$choice)
	}

## Overall proportions:

defendant_far <- xtab[1, ] / rowSums(xtab)[1]
defendant_close <- xtab[2, ] / rowSums(xtab)[2]

## Plot of this:

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '54%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '46%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '35%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '65%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
## Add legend:
legend('topright', pt.bg = c('wheat3', 'orange3'), legend = c('picked defendant', 'picked prosecutor'), pch = 22, cex = 1.15, pt.cex = 1.25, box.lwd = 2)





#### Make nice graphs:


###### MAKE NICE PLOTS FOR ESLP WITH PERCENTAGES (TEENIE's REQUEST):
pplot <- function(myxlim, myylim, 
	xlabel, ylabel, 
	myxaxis, myyaxis, axes = T, ...){
	quartz('', 9, 7);par(mai = c(1.5, 1.5, 1.5, 0.8))
	plot(1, 1, type = 'n', xlab = '', ylab = '', 
		xaxt = 'n', yaxt = 'n', 
		bty = 'n', 
		xlim = myxlim, ylim = myylim, ...)
	box(lwd = 4)
	if(axes){
		axis(1, at = myxaxis, font = 2, lwd = 4, lwd.ticks = 4, cex.axis = 1.75)
		axis(2, at = myyaxis, font = 2, lwd = 4, lwd.ticks = 4, las = 2, cex.axis = 1.75)
		}
	mtext(text = xlabel, side = 1, line = 4.5, font = 2, cex = 2.5)
	mtext(text = ylabel, side = 2, line = 4.5, font = 2, cex = 2.5)
	}

####### Experiment 1

xtab <- table(E1$choice, E1$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '67%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '33%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '31%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '69%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)



####### Experiment 2 in talk (E3)

xtab <- table(E3$choice, E3$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '57%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '43%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '36%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '64%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)



####### Experiment 3 in talk (E4)

xtab <- table(E4$choice, E4$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '61%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '39%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '27%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '73%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)




####### Experiment 4 in talk (E5A / E5B)

xtab <- table(E5B$choice, E5B$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '61%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '39%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '27%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '73%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)




####### Effect with delay:

xdata_delayed <- xdata[xdata$delay  =  =  'delayed', ]
xtab <- table(xdata_delayed$choice, xdata_delayed$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 100), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '55%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '45%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'wheat3')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '40%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'orange3')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '60%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)





########################################################################
######################## Plot for paper ... Experiment 1
########################################################################

xtab <- table(E1$choice, E1$distance)
defendant_far <- xtab[, 1] / colSums(xtab)[1]
defendant_close <- xtab[, 2] / colSums(xtab)[2]

gap_between <- 0.1
big_gap <- 0.5
above_factor <- 7
quartz('', 8, 6);par(mai = c(1, 2, 1, 0.5))
plot(1, 1, type = 'n', bty = 'n', xlim = c(0, 7), ylim = c(0, 120), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 2, seq(0, 100, 25), labels = paste0(seq(0, 100, 25), '%'), lwd = 4, lwd.ticks = 4, las = 2, font = 2, cex.axis = 2)
mtext(side = 2, line = 6, text = 'Percent chosen', cex = 2.5, font = 2)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
axis(side = 1, at = c(2, 5+big_gap), labels = c('Defendant close', 'Defendant far'), cex.axis = 1.5, font = 2, tick = F)
## left two bars
rect(1-gap_between, 0, 2-gap_between, defendant_close[1]*100, lwd = 2, col = 'lightgray')
text(x = mean(c(1-gap_between, 2-gap_between)), y = defendant_close[1]*100+above_factor, labels = '67%', font = 2, cex = 2)
rect(2+gap_between, 0, 3+gap_between, defendant_close[2]*100, lwd = 2, col = 'darkgrey')
text(x = mean(c(2+gap_between, 3+gap_between)), y = defendant_close[2]*100+above_factor, labels = '33%', font = 2, cex = 2)
## right two bars
rect(4-gap_between+big_gap, 0, 5-gap_between+big_gap, defendant_far[1]*100, lwd = 2, col = 'lightgray')
text(x = mean(c(4-gap_between+big_gap, 5-gap_between+big_gap)), y = defendant_far[1]*100+above_factor, labels = '30%', font = 2, cex = 2)
rect(5+gap_between+big_gap, 0, 6+gap_between+big_gap, defendant_far[2]*100, lwd = 2, col = 'darkgrey')
text(x = mean(c(5+gap_between+big_gap, 6+gap_between+big_gap)), y = defendant_far[2]*100+above_factor, labels = '70%', font = 2, cex = 2, xpd = NA)
segments(x0 = 0, x1 = 7, y0 = 0, lwd = 4, xpd = NA)
## add legend
legend('topright', pch = 15, col = c('lightgray', 'darkgrey'), legend = c('Picked defendant', 'Picked prosecutor'), cex = 1.5)


