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

"
https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html
https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/
https://www.r-bloggers.com/grouped-means-again/
https://onunicornsandgenes.blog/2014/03/26/using-r-quickly-calculating-summary-statistics-with-dplyr/
https://datacarpentry.org/R-genomics/04-dplyr.html
https://rafalab.github.io/dsbook/summarizing-data-with-dplyr.html
https://genomicsclass.github.io/book/pages/dplyr_tutorial.html
https://rstudio-pubs-static.s3.amazonaws.com/46399_ae360f3ec8644c9d9892994a12b0df8d.html
https://cran.r-project.org/web/packages/tibbletime/vignettes/TT-04-use-with-dplyr.html
http://kbroman.org/datacarpentry_R_2016-06-01/03-dplyr.html
https://suzan.rbind.io/2018/04/dplyr-tutorial-4/
https://www.r-bloggers.com/estimating-arrival-times-of-people-in-a-shop-using-r/

"

#-----  What is needed? -----
# 1. function to combine all files correctly
# 2. function to get averages for each wasp/nest
# 3. plot of survivals 
# 4. plot of viable nests vs actual nests
# 5. 



#----- Loading multiple files -----
"
https://mvuorre.github.io/toolbox/import.html
https://lieselspangler.com/2017/11/16/how-to-load-and-append-multiple-files-in-r/
https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
https://stackoverflow.com/questions/43858448/how-to-load-and-merge-multiple-csv-files-in-r/43858605
https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
http://r.789695.n4.nabble.com/Import-Multiple-csv-files-and-merge-into-one-Master-file-td2967823.html
https://stats.idre.ucla.edu/r/codefragments/read_multiple/
https://www.reed.edu/data-at-reed/resources/R/read_and_summarize_multiple_txt.html
https://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R
https://uoftcoders.github.io/studyGroup/lessons/r/dplyrmagrittr/lesson/
https://seananderson.ca/2014/09/13/dplyr-intro/
http://benbestphd.com/dplyr-tidyr-tutorial/#grouping,-summarizing,-and-mutating-data
https://stackoverflow.com/questions/37448062/create-function-with-dplyr
https://dplyr.tidyverse.org/articles/programming.html
https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr
http://www.datasciencemadesimple.com/filter-subsetting-rows-r-using-dplyr/
https://overdodactyl.github.io/ShadowFox/#
"


#1
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}

"wasp_data_output/mixed_spp/"
"d10g10"

data2 <- batch_read(path = "wasp_data_output/mixed_spp/", 
           pattern = "dominula_mix_d20g10", 
           read_fun = read.delim2,
           header = T, sep = "\t")

#2.
library(stringr)
library(reshape2)
library(tidyverse)
library(ggplot2)

folder <- path
file_list <- list.files(path = folder, pattern = "*.txt")

data <- do.call("rbind",
                lapply(file_list,
                       function(x)
                         read,table(paste0(folder, x, sep = ""),
                                    header = TRUE,
                                    stringsAsFactors = FALSE)))

#-----  Analysing Single Nests  -----

# Need to average individual wasps and compare
# Comparing distance, survival, nested, usurp, co-nest numbers between
#  fixed & random patch, fixed & random start
# Numbers of above vs number of wasps starting

# starting number of wasps = x
# ave number of steps until nest = y

# number of nested wasps vs number of viable nests

# number of wasps vs number usurps

# number of wasps vs conested



# Load data - single nests = FPSNRS, FPSNFS dom & gall

# 1. Compare fixed to random starting positions
c("50", "60", "70", "80", "90", "100", 
  "110", "120", "130", "140", "150",
  "160", "170", "180", "190", "200")
# Gallicus

gall_FPSNRS_100 <- 
  batch_read(path = "wasp_data_output/gallicus_FPSNRS/", 
             pattern = "gallicus_FSR_1", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

gall_FPSNRS_100 <- gall_FPSNRS_100 %>%
  mutate(number_wasps = rep(seq(100, 190, by = 10),
                            each = seq(100, 190, by = 10)), 10)

gall_FPSNRS <- rbind(gall_FPSNRS_50, gall_FPSNRS_60,
                     gall_FPSNRS_70, gall_FPSNRS_80,
                   gall_FPSNRS_90, gall_FPSNRS_100, gall_FPSNRS_200)

gall_FPSNRS <- gall_FPSNRS %>%
  mutate(Species = rep("gallicus", 20000))

write.table(gall_FPSNRS,
            "wasp_data_clean/gallicus_FPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")


gall_RPSNRS_dd <- 
  batch_read(path = "wasp_data_output/gallicus_RPSNRS/", 
             pattern = "gallicus_RSR_[5-9]", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

gall_RPSNRS_dd <- gall_RPSNRS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

gall_RPSNRS_200 <- gall_RPSNRS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

gall_RPSNRS_100 <- gall_RPSNRS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
         rep(120, 1200), rep(130, 1300),
         rep(140, 1400), rep(150, 1500), rep(160, 1600),
         rep(170, 1700), rep(180, 1800), rep(190, 1900)))

gall_RPSNRS <- rbind(gall_RPSNRS_dd, gall_RPSNRS_100, gall_RPSNRS_200)

gall_RPSNRS <- gall_RPSNRS %>%
  mutate(Species = rep("gallicus", 20000))

write.table(gall_RPSNRS,
            "wasp_data_clean/gallicus_RPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")



gall_RPSNFS_200 <- 
  batch_read(path = "wasp_data_output/gallicus_RPSNFS/", 
             pattern = "gallicus_RSF_200", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

gall_RPSNFS_dd <- gall_RPSNFS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

gall_RPSNFS_200 <- gall_RPSNFS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

gall_RPSNFS_100 <- gall_RPSNFS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

gall_RPSNFS <- rbind(gall_RPSNFS_dd, gall_RPSNFS_100, gall_RPSNFS_200)

gall_RPSNFS <- gall_RPSNFS %>%
  mutate(Species = rep("gallicus", 20000))

write.table(gall_RPSNFS,
            "wasp_data_clean/gallicus_RPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")



gall_FPSNFS_200 <- 
  batch_read(path = "wasp_data_output/gallicus_FPSNFS/", 
             pattern = "gallicus_FSF_200", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

gall_FPSNFS_dd <- gall_FPSNFS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

gall_FPSNFS_200 <- gall_FPSNFS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

gall_FPSNFS_100 <- gall_FPSNFS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

gall_FPSNFS <- rbind(gall_FPSNFS_dd, gall_FPSNFS_100, gall_FPSNFS_200)

gall_FPSNFS <- gall_FPSNFS %>%
  mutate(Species = rep("gallicus", 20000))

write.table(gall_FPSNFS,
            "wasp_data_clean/gallicus_FPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")





# Dominula
dom_FPSNRS_100 <- 
  batch_read(path = "wasp_data_output/dominula_FPSNRS/", 
             pattern = "dominula_RSF_1", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

dom_FPSNRS_dd <- dom_FPSNRS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

dom_FPSNRS_200 <- dom_FPSNRS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

dom_FPSNRS_100 <- dom_FPSNRS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

dom_FPSNRS <- rbind(dom_FPSNRS_dd, dom_FPSNRS_100, dom_FPSNRS_200)

dom_FPSNRS <- dom_FPSNRS %>%
  mutate(Species = rep("dominula", 20000))

write.table(dom_FPSNRS,
            "wasp_data_clean/dominula_FPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")

dom_FPSNFS_100 <- 
  batch_read(path = "wasp_data_output/dominula_FPSNFS/", 
             pattern = "dominula_RSF_200", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

dom_FPSNFS_dd <- dom_FPSNFS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

dom_FPSNFS_200 <- dom_FPSNFS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

dom_FPSNFS_100 <- dom_FPSNFS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

dom_FPSNFS <- rbind(dom_FPSNFS_dd, dom_FPSNFS_100, dom_FPSNFS_200)

dom_FPSNFS <- dom_FPSNFS %>%
  mutate(Species = rep("dominula", 20000))

write.table(dom_FPSNFS,
            "wasp_data_clean/dominula_FPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")


dom_RPSNRS_200 <- 
  batch_read(path = "wasp_data_output/dominula_RPSNRS/", 
             pattern = "dominula_RSR_200", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

dom_RPSNRS_dd <- dom_RPSNRS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

dom_RPSNRS_200 <- dom_RPSNRS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

dom_RPSNRS_100 <- dom_RPSNRS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

dom_RPSNRS <- rbind(dom_RPSNRS_dd, dom_RPSNRS_100, dom_RPSNRS_200)

dom_RPSNRS <- dom_RPSNRS %>%
  mutate(Species = rep("dominula", 20000))

write.table(dom_RPSNRS,
            "wasp_data_clean/dominula_RPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")


dom_RPSNFS_200 <- 
  batch_read(path = "wasp_data_output/dominula_RPSNFS/", 
             pattern = "dominula_RSF_200", 
             read_fun = read.delim2, 
             header = T, sep = "\t")

dom_RPSNFS_dd <- dom_RPSNFS_dd %>%
  mutate(number_wasps = c(rep(50, 500), rep(60, 600), rep(70, 700),
                          rep(80, 800), rep(90, 900)))

dom_RPSNFS_200 <- dom_RPSNFS_200 %>%
  mutate(number_wasps = rep(200, 2000)) 

dom_RPSNFS_100 <- dom_RPSNFS_100 %>%
  mutate(number_wasps = c(rep(100, 1000), rep(110, 1100),
                          rep(120, 1200), rep(130, 1300),
                          rep(140, 1400), rep(150, 1500), rep(160, 1600),
                          rep(170, 1700), rep(180, 1800), rep(190, 1900)))

dom_RPSNFS <- rbind(dom_RPSNFS_dd, dom_RPSNFS_100, dom_RPSNFS_200)

dom_RPSNFS <- dom_RPSNFS %>%
  mutate(Species = rep("dominula", 20000))

write.table(dom_RPSNFS,
            "wasp_data_clean/dominula_RPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")




#gall_FPSNRS <- gall_FPSNRS %>%
 # mutate(Nest = rep(1:10, each = 100, 10),
#         Species = rep("gallicus", 10000))


nums2 <- c(rep(100, 1000), rep(110, 1100), rep(120, 1200), rep(130, 1300),
           rep(140, 1400), rep(150, 1500), rep(160, 1600), rep(170, 1700),
           rep(180, 1800), rep(190, 1900))

#-----  Analysing Multiple Nests  -----

# Need to group by nest and individual wasp and average
# Comparing distance, survival, nested, usurp, co-nest numbers between
#  fixed & random patch
# Numbers of above vs number of wasps starting



#-----  Analysing Mixed Species -----

# Need to group by nest and individual wasps and average
# Find number of viable cells remaining after dominula

summary_stats2 <-
  data %>%
  group_by(w) %>%
  summarise(count = n(),
            meanNesting <- mean(nested),
            maxSteps <- max(s))

data <- data %>%
  mutate(Nest = rep(1:10, each = 100, 10))




# Load habitats and get viable nests

"wasp_data_output/dominula_FPSNRS/patchCoords_FSR_1.txt"


dFSR_hab <- as.data.frame(
  read.table("wasp_data_output/dominula_FPSNRS/patchCoords_FSR_1.txt",
             header = TRUE))

dim(dFSR_hab)

# need to load all habitats separately and then get number of rows
#  then store in new table.


