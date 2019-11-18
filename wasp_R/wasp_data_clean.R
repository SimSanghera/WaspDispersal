#-----  Clean Raw Data  -----#

# Clean text output files
# 1. Load all relevant files together
# 2. Add starting number of wasps column
# 3. Add a species column


# Load Libraries
library(tidyverse)
library(reshape2)

# Load batch files function
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}



#-----  Single Species Trials -----#

#-----  1. Single Nests -----
# These include:
#   Fixed Patch Single Nest Fixed Start - FPSNFS
#   Fixed Patch Single Nest Random Start - FPSNRS
#   Random Patch Single Nest Fixed Start - RPSNFS
#   Random Patch Single Nest Random Start - RPSNRS
# For both species - dominula & gallicus
# For loading files - due to my lack of ability (for now), loading the 
#   files is pretty messy. Have to keep changing the pattern within the 
#   function to match the numbers in the files
#   So for 50-90 use 5-9, 100-190 use 1, for 200 use 200.
# Then we have to edit the columns - adding numbers & species, but sadly
#   I couldn't figure out a functional way to do it - so again, messy.


#---- Gallicus  -----

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



gall_RSF_2 <- gall_RSF %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp,
         currentX, currentY)

gall_RSR_2 <- gall_RSR %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp,
         currentX, currentY)

gall_FSF_2 <- gall_FSF %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp,
         currentX, currentY)

gall_FSR_2 <- gall_FSR %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp,
         currentX, currentY)


write.table(gall_FSF_2,
            "wasp_data_clean/gallicus_FPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")
write.table(gall_FSR_2,
            "wasp_data_clean/gallicus_FPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")
write.table(gall_RSR_2,
            "wasp_data_clean/gallicus_RPSNRS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")
write.table(gall_RSF_2,
            "wasp_data_clean/gallicus_RPSNFS_all_clean.txt",
            row.names = FALSE,
            sep = "\t")






#-----  Dominula  -----
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


colnames(dom_RSF)
colnames(gall_RSF_2)

dom_RPSNFS <- dom_RSF %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp, coNested, 
         X, currentX, currentY)

dom_RPSNRS <- dom_RSR %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp, coNested, 
         X, currentX, currentY)

dom_FPSNFS <- dom_FSF %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp, coNested, 
         X, currentX, currentY)

dom_FPSNRS <- dom_FSR %>%
  select(Species, number_wasps, w, s, startX, startY,
         alive, nested, usurp, coNested, 
         X, currentX, currentY)



#

#----- 2. Multiple Nests -----
# These include: 
#   Random Patch Multiple Nests Random Start - RPMNRS
# For both species - dominula, gallicus
# Number of nests increased from 10 to 50
# File names include which trial (number of nests) and number of wasps
#   e.g. n10
# Could be possible to load all into a single large file
# NOTE - this has loaded files organising by n (nest number) - so it goes,
#   nest 1 ten times, then nest 10 ten times, then nest 2 ten times.
#   But still within order of number of nests - so should be ok.

gall_Multi <-
  batch_read(path = "wasp_data_output/gallicus_RPMNRS/",
             pattern = "gallicus_RMR_n",
             read_fun = read.delim2,
             header = T, sep = "\t")


gall_Multi <- gall_Multi %>%
  mutate(number_nests = c(rep(10, 10000),
                          rep(15, 15000),
                          rep(20, 20000), 
                          rep(30, 30000), 
                          rep(40, 40000), 
                          rep(50, 50000)),
         species = rep("gallicus", 165000))

write.table(gall_Multi, "wasp_data_clean/gallicus_RMR.txt",
            row.names = FALSE,
            sep = "\t")



dom_Multi <-
  batch_read(path = "wasp_data_output/dominula_RPMNRS/",
             pattern = "dominula_RMR_n",
             read_fun = read.delim2,
             header = T, sep = "\t")


dom_Multi <- dom_Multi %>%
  mutate(number_nests = c(rep(10, 10000),
                          rep(15, 15000),
                          rep(20, 20000), 
                          rep(30, 30000), 
                          rep(40, 40000), 
                          rep(50, 50000)),
         species = rep("dominula", 165000))
colnames(dom_Multi[11])
colnames(dom_Multi)[11] <- "num_wasps_nest"

write.table(dom_Multi, "wasp_data_clean/dominula_RMR.txt",
            row.names = FALSE,
            sep = "\t")

#
# Maybe add num_wasps_nest column to gall for uniformity - except
#  i made it hard because I am an idiot
# where (nested & usurp == 1) - check if final cell values are found 
#  somewhere else and then 

gall_RMR <- read.delim2("wasp_data_clean/gallicus_RMR.txt",
                        header = TRUE,
                        sep = "\t")

gall_RMR_2 <- gall_RMR %>%
  mutate(coNested = rep(0, 165000),
         num_wasps_nest = ifelse(nested == 1, 1, 0))



# Dom need to add nest number - n - to dataframe. Order is 1, 10s, 20s, 30s, 40s, 50
# Can I just shift the column from gall to dom?

dom_RMR <- read.delim2("wasp_data_clean/dominula_RMR.txt",
                       header = TRUE,
                       sep = "\t")
# get n from gall_RMR
n2 <- gall_RMR$n

dom_RMR_2 <- dom_RMR %>% 
  mutate(n = n2)


# Reorder columns
colnames(gall_RMR_2)
colnames(dom_RMR_2)

gall_RMR_3 <- gall_RMR_2 %>%
  select(species, number_nests, n:usurp, coNested,
         num_wasps_nest, currentX, currentY)

dom_RMR_3 <- dom_RMR_2 %>%
  select(species, number_nests, n, w, s, startX, startY,
         alive, nested, usurp, coNested, num_wasps_nest,
         currentX, currentY)

colnames(gall_RMR_3)
colnames(dom_RMR_3)

write.table(dom_RMR_3, "wasp_data_clean/dominula_RMR_clean.txt",
            row.names = FALSE,
            sep = "\t")


write.table(gall_RMR_3, "wasp_data_clean/gallicus_RMR_clean.txt",
            row.names = FALSE,
            sep = "\t")



#-----  3. Mixed Species  -----
