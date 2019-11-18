#-----  Preliminary Analysis  -----

# Check to see if we can use smaller grids with lower numbers of 
#   wasps in place of a 1000x1000 grid and 200000 wasps.  
# Compared using single nest experiments in gallicus.

# Need to check:
#   - proportion of nests filled, remaining
#   - number of surviving wasps
#   - average distances travelled
#   - maximum distance travelled
#   - total number of wasps alive per sim
#   - total number of viable nests filled
# 


library(tidyverse)

#-----  1. Load files  -----
list.files(path = "folder") # results in really long list
list.files(path = "wasp_data_output/100x100/", pattern = "^gallicus_RSF_50(.*)txt")

gallfinder <- function(x, pathname) {
  gallfiles <<- list.files(path = pathname,
                          pattern = paste0("^", x, "(.*)txt"))
  
  gallData <<- lapply(paste0(pathname,gallfiles),
                     read.delim2,
                     header = TRUE, sep = "\t")
  
}



gallfinder("gallicus_RSF_50", "wasp_data_output/100x100/")



#-----  Notes -----
# eval(parse(text = paste0("stuff")))
# 
# xy <- strsplit(x, "")
# 
# https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/
#   
#   https://www.masterdataanalysis.com/r/working-with-files-and-folders-in-r/
#   
#   https://rstudio-pubs-static.s3.amazonaws.com/9496_124e01082df74acd80a55674068b8756.html
# 
# https://stackoverflow.com/questions/30094843/import-data-in-r-from-multiple-files-with-name-pattern
# 
# https://earthdatascience.org/courses/earth-analytics/automate-science-workflows/use-apply-functions-for-efficient-code-r/
#   
#   https://mvuorre.github.io/toolbox/import.html
# 
# 
# https://stackoverflow.com/questions/15104046/passing-a-character-vector-as-arguments-to-a-function-in-plyr
# https://stackoverflow.com/questions/7836972/use-character-string-as-function-argument
# 




#-----

#-----  2. Get information  -----
# So need to obtain max numbers and averages for:
#   - each individual wasp within each round - i.e. average across wasp 1,
#       within all sims where number of wasps = 50, 60, 70 ...
#   - average number of survival within each sim for number of wasps
#   - total distances for same as above
#   - average distances for each wasp across all sims
#   - average distance across each sim irrespective of wasp number

# Combine data files into a single large data file.
# Need stats for each run - as a collective. e.g. mean 1-maxW in run 1, run 2
# Combine individual wasps into own data files - mutate?


#---- Firstly, split and combine files as needed
gall50_1 <- gallData[[1]]

# Change column name of X to N_steps
for (i in seq_along(gallData)) {
  colnames(gallData[[i]])[2] <- "N_steps"
}

# Collate all into a single data frame - for stats of 50 wasps
gall50 <- do.call("rbind", gallData)


# Collate into multiple data frames for each wasp ID
gallWasps <- list()
for (i in seq_along(gallData)) {
  gallWasps[[1]] <- lapply(gall50, function(x) filter(gall50, w == 1))
}




# need mean of steps, max of steps, number of survivors, number of nesters
# number of usurps
# should we get means of alive and nested, or just include results when alive?

gall50 %>%
  filter(w == 1) %>%
  summarise(mean(N_steps), mean(alive), mean(nested), mean(usurp))

a <- gall50 %>%
  group_by(w) %>%
  summarise(mean(N_steps), mean(alive), mean(nested), mean(usurp),
            max(N_steps))


gall50_w1 <- gall50 %>%
  filter(w == 1)


# Total survivors, nesters, usurpers in each simulation




#-----  3. Run stats to analyse data  -----