setwd('~/Downloads/OwlHacks\ 2020\ Submission/OwlHacks\ Proj') 

# Call install.packages(<package string>) if not installed 

library(purrr)
library(predictrace)
library(dplyr)

data <- read.csv('all.csv')
# Thx Wikipedia 
phila_race_dem <- data.frame(
		'race' = c('Black', 'White', 'Hispanic', 'Asian', 'Islander', 'Native American', '2+'),
		'percentage' = c(0.426, 0.416, 0.141, 0.071, 0.0005, 0.004, 0.028))

# x is column of names, u is unique bool 
nameProbs <- function (x, u) {
	retVals <- vector()
	looper <- strsplit(x, ',')
	
	if (u) {
		looper <- unique(looper)
	}
	
	for (name in looper) {
		retVals <- append(unlist(name)[1], retVals)
	}
	return(predict_race(retVals) %>% filter(!is.na(likely_race)))
}

# Predicted race of ppl setting bail unweighted 
bail_unw <- colSums(nameProbs(data$bail_set_by, TRUE)[4:9])
# Predicted race of ppl setting bail weighted by # of occurances 
bail_w <- colSums(nameProbs(data$bail_set_by, FALSE)[4:9])

# Predicted race of arresting officer unweighted 
off_unw <-colSums(nameProbs(data$arresting_officer, TRUE)[4:9]) 
# Predicted race of arresting officer weighted by occurances 
off_w <- colSums(nameProbs(data$arresting_officer, FALSE)[4:9])

write.csv(bail_unw, 'bail_unw.csv')
write.csv(bail_w, 'bail_w.csv')
write.csv(off_unw, 'off_unw.csv')
write.csv(off_w, 'off_w.csv')
write.csv(phila_race_dem, 'phila_race_dem.csv')
