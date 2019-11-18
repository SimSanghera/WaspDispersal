#-----  Wasp Analysis -----

# Need to collate each wasp numbers and get average
# Get average number of steps moved in all models - individual habitat layers
# Average number of steps for each wasp across all habitats
# maximum distance travelled
# total number of wasps alive per run, average across habitats
# total number of viable nests filled - ratio filled:available
# percentage usurp vs join vs moving on
# compare above between different numbers of wasps

# ANOVA on means and max distance

# Need to add nest as a column - so we know which wasps came from which nests
# Need to add a spp identifier column


#-----  What is needed? -----
# 1. function to combine all files correctly
# 2. function to get averages for each wasp/nest
# 3. plot of survivals 
# 4. plot of viable nests vs actual nests
# 5. Need to add treatment column - FSF, RSR etc.. to all



#----- Loading multiple files -----
#1
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}





#-----  Single Nests  ------
# Need to average individual wasps and compare
# Comparing distance, survival, nested, usurp, co-nest numbers between
#  fixed & random patch, fixed & random start
# Numbers of above vs number of wasps starting

# starting number of wasps = x
# ave number of steps until nest = y

# number of nested wasps vs number of viable nests

# number of wasps vs number usurps

# number of wasps vs conested

# Load Libraries & Data
library(tidyverse)
library(ggplot2)
library(reshape2)
library(survival)
library(survminer)
library(lme4)


gall_FSF <- read.table("wasp_data_clean/gallicus_FPSNFS_all_clean.txt", 
                       header = TRUE, sep = "\t")

gall_FSR <- read.table("wasp_data_clean/gallicus_FPSNRS_all_clean.txt", 
                       header = TRUE, sep = "\t") 

gall_RSF <- read.table("wasp_data_clean/gallicus_RPSNFS_all_clean.txt", 
                       header = TRUE, sep = "\t")

gall_RSR <- read.table("wasp_data_clean/gallicus_RPSNRS_all_clean.txt", 
                       header = TRUE, sep = "\t")


dom_FSF <- read.table("wasp_data_clean/dominula_FPSNFS_all_clean.txt", 
                      header = TRUE, sep = "\t")

dom_FSR <- read.table("wasp_data_clean/dominula_FPSNRS_all_clean.txt", 
                      header = TRUE, sep = "\t")

dom_RSF <- read.table("wasp_data_clean/dominula_RPSNFS_all_clean.txt", 
                      header = TRUE, sep = "\t")

dom_RSR <- read.table("wasp_data_clean/dominula_RPSNRS_all_clean.txt", 
                      header = TRUE, sep = "\t")

# Gallicus
# Split by number of starting wasps
# maximum steps travelled by each wasp; average survival for each wasp
# average survival for each wasp then total
# average usurp for each wasp then get total usurps
# get total viable nests - load all habitats (UGH)

# https://dplyr.tidyverse.org/articles/programming.html
# https://edwinth.github.io/blog/nse/
# https://dplyr.tidyverse.org/articles/programming.html
# https://medium.com/optima-blog/writing-your-own-dplyr-functions-a1568720db0d
# https://cyberhelp.sesync.org/data-manipulation-in-R-lesson/2016/07/26/
# http://benbestphd.com/dplyr-tidyr-tutorial/#grouping,-summarizing,-and-mutating-data
# https://uoftcoders.github.io/studyGroup/lessons/r/dplyrmagrittr/lesson/
# https://seananderson.ca/2014/09/13/dplyr-intro/
# https://datacarpentry.org/R-ecology-lesson/03-dplyr.html#pipes


gallicus_output <- gall_FSF %>%
  group_by(number_wasps, w) %>%
  summarise(max_steps = max(s),
            mean_steps = round(mean(s)),
            mean_nested = round(mean(nested)),
            mean_survival = round(mean(alive)),
            mean_usurp = round(mean(usurp)))

gallicus_totals %>%
  mutate(total_usurp)


# Stats needed for single species analysis - per wasp
# 1. Max number of steps per trial conditions
# 2. mean number of steps per wasp
# 3. min number of steps
# 4. mean survival
# 5. mean usurp
# 6. total survival
# 7. total usurps
# 8. need proportion of viable and non-viable cells

gall_FSF_output <- gall_FSF %>%
  group_by(number_wasps, w) %>%
  summarise(max_steps = max(s),
            min_steps = min(s),
            mean_steps = round(mean(s)),
            mean_survival = round(mean(alive)),
            mean_nested = round(mean(nested)),
            mean_usurp = round(mean(usurp)))


get_gall_stats_rounded <- function(data) {
  data %>%
    group_by(number_wasps, w) %>%
    summarise(max_steps = max(s),
              min_steps = min(s),
              mean_steps = round(mean(s)),
              mean_survival = round(mean(alive)),
              mean_nested = round(mean(nested)),
              mean_usurp = round(mean(usurp)))
}


get_gall_stats <- function(data) {
  data %>%
    group_by(number_wasps, w) %>%
    summarise(max_steps = max(s),
              min_steps = min(s),
              mean_steps = mean(s),
              mean_survival = mean(alive),
              mean_nested = mean(nested),
              mean_usurp = mean(usurp))
}

gall_FSF_output <- get_gall_stats(gall_FSF)
gall_FSR_output <- get_gall_stats(gall_FSR)
gall_RSF_output <- get_gall_stats(gall_RSF)
gall_RSR_output <- get_gall_stats(gall_RSR)



get_gall_totals <- function(data) {
  data %>%
    group_by(number_wasps, w) %>%
    summarise(sum(alive), sum(usurp), sum(nested)) %>%
    rename(total_alive = "sum(alive)",
           total_nested = "sum(nested)",
           total_usurp = "sum(usurp)")
}

gall_FSF_totals <- get_gall_totals(gall_FSF)
gall_FSR_totals <- get_gall_totals(gall_FSR)
gall_RSF_totals <- get_gall_totals(gall_RSF)
gall_RSR_totals <- get_gall_totals(gall_RSR)





# function to replace -2 holders in dominula X column
dominula_tidy <- function (data) {
  data %>%
    mutate(X = replace(X, X == -2, 1))
}

# dom_FSF_2 <- dom_FSF %>%
#   mutate(X = replace(X, X == -2, 1))
list_dom <- list(dom_FSF, dom_FSR, dom_RSF, dom_RSR)

dom_tidy <- lapply(list_dom, dominula_tidy)

# name data frames in list
names(dom_tidy) <- c("dom_FSF", "dom_FSR", "dom_RSF", "dom_RSR")
list2env(dom_tidy, .GlobalEnv)

#

get_dom_stats_rounded <- function(data) {
  data %>%
    group_by(Species, number_wasps, w) %>%
    summarise(max_steps = max(s),
              min_steps = min(s),
              mean_steps = round(mean(s)),
              mean_survival = round(mean(alive)),
              mean_nested = round(mean(nested)),
              mean_usurp = round(mean(usurp)),
              mean_coNested = round(mean(coNested)),
              mean_coNesters = round(mean(X)) )
}

get_dom_stats <- function(data) {
  data %>%
    group_by(Species, number_wasps, w) %>%
    summarise(max_steps = max(s),
              min_steps = min(s),
              mean_steps = mean(s),
              mean_survival = mean(alive),
              mean_nested = mean(nested),
              mean_usurp = mean(usurp),
              mean_coNested = mean(coNested),
              mean_coNesters = mean(X) )
}


dom_FSF_output <- get_dom_stats(dom_FSF)
dom_FSR_output <- get_dom_stats(dom_FSR)
dom_RSF_output <- get_dom_stats(dom_RSF)
dom_RSR_output <- get_dom_stats(dom_RSR)



get_dom_totals <- function(data) {
  data %>%
    group_by(Species, number_wasps, w) %>%
    summarise(sum(alive),
              sum(nested),
              sum(usurp),
              sum(coNested)) %>%
    rename(total_alive = "sum(alive)",
           total_nested = "sum(nested)",
           total_usurp = "sum(usurp)",
           total_coNested = "sum(coNested)")
}

dom_FSF_totals <- get_dom_totals(dom_FSF)
dom_FSR_totals <- get_dom_totals(dom_FSR)
dom_RSF_totals <- get_dom_totals(dom_RSF)
dom_RSR_totals <- get_dom_totals(dom_RSR)





# write out current files
write.table(dom_FSF_output, "wasp_data_clean/dom_FSF_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_FSR_output, "wasp_data_clean/dom_FSR_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_RSF_output, "wasp_data_clean/dom_RSF_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_RSR_output, "wasp_data_clean/dom_RSR_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_FSF_totals, "wasp_data_clean/dom_FSF_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_FSR_totals, "wasp_data_clean/dom_FSR_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_RSF_totals, "wasp_data_clean/dom_RSF_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(dom_RSR_totals, "wasp_data_clean/dom_RSR_totals.txt",
            row.names = FALSE, sep = "\t")


write.table(gall_FSF_output, "wasp_data_clean/gall_FSF_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_FSR_output, "wasp_data_clean/gall_FSR_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_RSF_output, "wasp_data_clean/gall_RSF_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_RSR_output, "wasp_data_clean/gall_RSR_stats.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_FSF_totals, "wasp_data_clean/gall_FSF_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_FSR_totals, "wasp_data_clean/gall_FSR_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_RSF_totals, "wasp_data_clean/gall_RSF_totals.txt",
            row.names = FALSE, sep = "\t")
write.table(gall_RSR_totals, "wasp_data_clean/gall_RSR_totals.txt",
            row.names = FALSE, sep = "\t")


#####
https://rpkgs.datanovia.com/survminer/index.html
https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

https://rpubs.com/euclid/343644
https://drsimonj.svbtle.com/plotting-individual-observations-and-group-means-with-ggplot2
https://ggplot2.tidyverse.org/reference/aes_group_order.html
https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization
https://www.r-bloggers.com/part-3a-plotting-with-ggplot2/
  https://ggplot2.tidyverse.org/reference/geom_smooth.html
