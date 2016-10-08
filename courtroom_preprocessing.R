## Bodo Winter
## Created: August 23,  2014; Edited & Finished: August 31,  2014
## Cleaned and adapted to new style October 8, 2016
## Preprocessing of Courtroom Experiment

## Load library

library(dplyr)

##------------------------------------------------------------------
## Define functions:
##------------------------------------------------------------------

## Function for batch-loading in .txt files from web experiments:

load_files <- function(filenames, new = F){
	xdata <- c()
	for(i in 1:length(filenames)) {
			if(!new){
				xdata <- rbind(xdata, read.table(filenames[i]))
				}
			if(new){
				xdata <- rbind(xdata, read.table(filenames[i], sep = ';'))
				}
		}
	return(xdata)
	}

## Function that gets rid of semicolons:

rid_semi <- function(x) as.numeric(gsub(';', '', x))


##------------------------------------------------------------------
## Load in data and append IP:
##------------------------------------------------------------------

## Set master folder:

mainFolder <- '/Users/winterb/Research/josh_jury_courtroom/new_analysis'

## Folder names and names of future data frames:

foldernames <- c('E1', 'E2', 'E3', 'E4',
	'E5A', 'E5B', 'E6A', 'E6B')

## Loop through names, load data in and assign to data frames:

for (i in 1:length(foldernames)) {
	setwd(file.path(mainFolder, foldernames[i]))
	
	## Variable for whether it is the new experiment coding scheme:
	
	if (foldernames[i] %in% c('E3', 'E5A', 'E5B', 'E6A', 'E6B')) {
		newyes <- T
		} else { newyes <- F }
	
	assign(foldernames[i],
		load_files(filenames = list.files(), new = newyes))
	}



##------------------------------------------------------------------
## Delete unwanted columns:
##------------------------------------------------------------------

## Define unwanted columns:

E1 <- select(E1,
	-V1, -V4, -V7, -(V9:V11))
E2 <- select(E2,
	-V1, -V4, -V7, -(V9:V10))	
E4 <- select(E4,
	-V1, -V4, -V7, -(V9:V11))

## Rename columns:

colnames(E1) <- c('condition', 'choice', 'confidence', 'bias', 'experience')
colnames(E2) <- c('condition', 'choice', 'confidence', 'bias', 'experience')
colnames(E3) <- c('distance', 'choice', 'confidence', 'bias', 'experience')
colnames(E4) <- c('condition', 'choice', 'confidence', 'bias', 'experience')
colnames(E5A) <- c('language_perspective', 'language_evidence',
	'distance', 'choice', 'confidence', 'bias', 'experience')
colnames(E5B) <- c('language_perspective', 'language_evidence',
	'distance', 'choice', 'confidence', 'bias', 'experience')
colnames(E6A) <- c('language_perspective', 'language_evidence',
	'distance', 'question_orientation', 'choice', 'confidence',
	'bias', 'experience')
colnames(E6B) <- c('language_perspective', 'language_evidence',
	'distance', 'question_orientation', 'choice', 'confidence',
	'bias', 'experience')

## Get rid of semicolons:

E1 <- mutate(E1, confidence = rid_semi(confidence))
E2 <- mutate(E2, confidence = rid_semi(confidence))
E4 <- mutate(E4, confidence = rid_semi(confidence))

## Code whether defendant is far or close:

E1$distance <- 'far'
nears <- (E1$condition == '[D,P,J]' | E1$condition == '[P,D,J]')
# weird condition codes (but consistent with script):
E1[nears, ]$distance <- 'near'
E2$distance <- 'near'
fars <- (E2$condition == '[L,P,D]' | E2$condition == '[R,P,D]')
E2[fars, ]$distance <- 'far'
E4$distance <- 'near'
fars <- (E4$condition == '[J,P,D]' | E4$condition == '[D,P,J]')
E4[fars, ]$distance <- 'far'

## Code whether jury is left or right:

E1$room_orientation <- 'jury_right'
lefts <- (E1$condition == '[J,P,D]' | E1$condition == '[D,P,J]')
E1[lefts, ]$room_orientation <- 'jury_left'
E2$room_orientation <- 'jury_right'
lefts <- (E2$condition == '[L,P,D]' | E2$condition == '[L,D,P]')
E2[lefts, ]$room_orientation <- 'jury_left'
E4$room_orientation <- 'jury_left'
lefts <- (E4$condition == '[P,D,J]' | E4$condition == '[D,P,J]')
E4[lefts, ]$room_orientation <- 'jury_right'

## Get rid of 'condition' columns:

E1 <- select(E1, -condition)
E2 <- select(E2, -condition)
E4 <- select(E4, -condition)

## Name 'choice' columns consistently throughout:

# E3
E3$choice <- as.character(E3$choice)
E3[E3$choice == 'picked_defendant', ]$choice <- 'defendant'
E3[E3$choice == 'picked_prosecutor', ]$choice <- 'plaintiff'
E3$choice <- as.factor(E3$choice)
# E4
E4$choice <- as.character(E4$choice)
E4[E4$choice == 'defandent', ]$choice <- 'defendant'
E4$choice <- as.factor(E4$choice)
# E5A
E5A$choice <- as.character(E5A$choice)
E5A[E5A$choice == 'picked_defendant', ]$choice <- 'defendant'
E5A[E5A$choice == 'picked_plaintiff', ]$choice <- 'plaintiff'
E5A$choice <- as.factor(E5A$choice)
# E5B
E5B$choice <- as.character(E5B$choice)
E5B[E5B$choice == 'picked_defendant', ]$choice <- 'defendant'
E5B[E5B$choice == 'picked_plaintiff', ]$choice <- 'plaintiff'
E5B$choice <- as.factor(E5B$choice)
# E6A
E6A$choice <- as.character(E6A$choice)
E6A[E6A$choice == 'picked_defendant', ]$choice <- 'defendant'
E6A[E6A$choice == 'picked_prosecutor', ]$choice <- 'plaintiff'
E6A$choice <- as.factor(E6A$choice)
# E6B
E6B$choice <- as.character(E6B$choice)
E6B[E6B$choice == 'picked_defendant', ]$choice <- 'defendant'
E6B[E6B$choice == 'picked_prosecutor', ]$choice <- 'plaintiff'
E6B$choice <- as.factor(E6B$choice)

## Name 'distance' columns consistently throughout:

E3$distance <- as.character(E3$distance)
E3[E3$distance == 'defendant_close', ]$distance <- 'near'
E3[E3$distance == 'prosecutor_close', ]$distance <- 'far'
E5A$distance <- as.character(E5A$distance)
E5A[E5A$distance == 'defendant_close', ]$distance <- 'near'
E5A[E5A$distance == 'prosecutor_close', ]$distance <- 'far'
E5B$distance <- as.character(E5B$distance)
E5B[E5B$distance == 'defendant_close', ]$distance <- 'near'
E5B[E5B$distance == 'prosecutor_close', ]$distance <- 'far'
E6A$distance <- as.character(E6A$distance)
E6A[E6A$distance == 'defendant_close', ]$distance <- 'near'
E6A[E6A$distance == 'defendant_far', ]$distance <- 'far'
E6B$distance <- as.character(E6B$distance)
E6B[E6B$distance == 'defendant_close', ]$distance <- 'near'
E6B[E6B$distance == 'defendant_far', ]$distance <- 'far'

## Name 'experience' columns consistently throughout:

E3$experience <- as.character(E3$experience)
E3[E3$experience == 'no jury experience', ]$experience <- 'no'
E3[E3$experience == 'yes jury experience', ]$experience <- 'yes'
E5A$experience <- as.character(E5A$experience)
E5A[E5A$experience == 'no_jury', ]$experience <- 'no'
E5A[E5A$experience == 'yes_jury', ]$experience <- 'yes'
E5B$experience <- as.character(E5B$experience)
E5B[E5B$experience == 'no_jury', ]$experience <- 'no'
E5B[E5B$experience == 'yes_jury', ]$experience <- 'yes'
E6A$experience <- as.character(E6A$experience)
E6A[E6A$experience == 'no_jury', ]$experience <- 'no'
E6A[E6A$experience == 'yes_jury', ]$experience <- 'yes'
E6B$experience <- as.character(E6B$experience)
E6B[E6B$experience == 'no_jury', ]$experience <- 'no'
E6B[E6B$experience == 'yes_jury', ]$experience <- 'yes'

## Name 'bias' column consistently throughout:

E3$bias <- as.character(E3$bias)
E3[E3$bias == ' no bias', ]$bias <- 'no'
E3[E3$bias == ' prosecutor bias', ]$bias <- 'prosecutor'
E3[E3$bias == ' defendant bias', ]$bias <- 'defendant'
E5A$bias <- as.character(E5A$bias)
E5A[E5A$bias == 'no_bias', ]$bias <- 'no'
E5A[E5A$bias == 'prosecutor_bias', ]$bias <- 'prosecutor'
E5A[E5A$bias == 'defendant_bias', ]$bias <- 'defendant'
E5B$bias <- as.character(E5B$bias)
E5B[E5B$bias == 'no_bias', ]$bias <- 'no'
E5B[E5B$bias == 'prosecutor_bias', ]$bias <- 'prosecutor'
E5B[E5B$bias == 'defendant_bias', ]$bias <- 'defendant'
E6A$bias <- as.character(E6A$bias)
E6A[E6A$bias == 'no_bias', ]$bias <- 'no'
E6A[E6A$bias == 'prosecutor_bias', ]$bias <- 'prosecutor'
E6A[E6A$bias == 'defendant_bias', ]$bias <- 'defendant'
E6B$bias <- as.character(E6B$bias)
E6B[E6B$bias == 'no_bias', ]$bias <- 'no'
E6B[E6B$bias == 'prosecutor_bias', ]$bias <- 'prosecutor'
E6B[E6B$bias == 'defendant_bias', ]$bias <- 'defendant'

## Change order of columns:

E1 <- select(E1,
	distance, choice, confidence, bias, experience, room_orientation)
E2 <- select(E2,
	distance, choice, confidence, bias, experience, room_orientation)
E4 <- select(E4,
	distance, choice, confidence, bias, experience, room_orientation)
E5A <- select(E5A,
	distance, choice, confidence, bias, experience,
	language_perspective, language_evidence)
E5B <- select(E5B,
	distance, choice, confidence, bias, experience,
	language_perspective, language_evidence)
E6A <- select(E6A,
	distance, choice, confidence, bias, language_perspective,
	language_evidence, question_orientation, experience)
E6B <- select(E6B,
	distance, choice, confidence, bias, language_perspective,
	language_evidence, question_orientation, experience)

## Export data frames:

setwd(file.path(mainFolder, 'dataframes'))
for (i in 1:length(foldernames)) {
	write.table(x = get(foldernames[i]),
		file = paste(foldernames[i], 'csv', sep = '.'),
		row.names = F, sep = ',')
	}



