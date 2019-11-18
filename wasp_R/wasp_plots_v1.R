# Wasp Plots

# Library
library(tidyverse)
library(ggplot2)
library(wesanderson)

#-----  Single Species  -----

# Gallicus
# 1. Plot of steps vs survival and nesting
# 2. nested vs viable nests
# 3. nested vs usurp
# 4. number of starting wasps vs average number of steps to nest

# Maybe for plots, combine all stats and totals and take average
# Saves having to analyse between treatments

gall_FSF_stats <- read.table("wasp_data_clean/gall_FSF_stats.txt",
                             header = TRUE, sep = "\t")
gall_FSR_stats <- read.table("wasp_data_clean/gall_FSR_stats.txt",
                             header = TRUE, sep = "\t")
gall_RSF_stats <- read.table("wasp_data_clean/gall_RSF_stats.txt",
                             header = TRUE, sep = "\t")
gall_RSR_stats <- read.table("wasp_data_clean/gall_RSR_stats.txt",
                             header = TRUE, sep = "\t")

gall_FSF_totals <- read.table("wasp_data_clean/gall_FSF_totals.txt",
                             header = TRUE, sep = "\t")
gall_FSR_totals <- read.table("wasp_data_clean/gall_FSR_totals.txt",
                             header = TRUE, sep = "\t")
gall_RSF_totals <- read.table("wasp_data_clean/gall_RSF_totals.txt",
                             header = TRUE, sep = "\t")
gall_RSR_totals <- read.table("wasp_data_clean/gall_RSR_totals.txt",
                             header = TRUE, sep = "\t")


# Analysed difference between treatments using glmer and Tukey
# factors - FSF, FSR, RSF, RSR, random = wasp and step.
# variables - nested, usurp, conested, num_wasps_in_nest
# Compare models with anova - chisq

# get data of viable nests
gall_hab_FSF <- read.table("wasp_data_output/gallicus_FPSNFS/FSF_arena_1.txt",
                       header = TRUE, sep = "\t")
patchCoords <- which(habitat1 == 0, arr.ind = T)
plot(which(habitat1 == 0, arr.ind = T),
     xlim = c(0, 100),
     ylim = c(0, 100))

viable_nests <- function(data) {
  data <- which(data == 0, arr.ind = T)
  v_nests <- dim(data)[1]
  return(v_nests)
}

gall_FSF_viable_nests <- viable_nests(gall_hab_FSF)

gall_hab_FSR <- read.table("wasp_data_output/gallicus_FPSNRS/FSR_arena_1.txt",
                       header = TRUE, sep = "\t")
gall_FSR_viable_nests <- viable_nests(gall_hab_FSR)



# gall RSF RSR
path <- "wasp_data_output/gallicus_RPSNFS/"
files <- list.files(path = "wasp_data_output/gallicus_RPSNFS",
                    pattern = "RSF_arena")
gall_RSF_habs <- list()
gall_RSF_habs <- lapply(paste0(path,files), read.table, header = TRUE, sep = "\t")


gall_RSF_viable <- lapply(gall_RSF_habs, viable_nests)


pathRSR <- "wasp_data_output/gallicus_RPSNRS/"
files <- list.files(path = "wasp_data_output/gallicus_RPSNRS",
                    pattern = "RSR_arena")
gall_RSR_habs <- list()
gall_RSR_habs <- lapply(paste0(pathRSR, files), read.table, header = TRUE, sep = "\t")

gall_RSR_viable <- lapply(gall_RSR_habs, viable_nests)



#---- Combine Gall tables -----
#  add a treatment factor - FSF, FSR, RSR, RSF
gall_FSF_stats["Treatment"] <- "FSF"
gall_FSR_stats["Treatment"] <- "FSR"
gall_RSF_stats["Treatment"] <- "RSF"
gall_RSR_stats["Treatment"] <- "RSR"

gall_output <- rbind(gall_FSF_stats, gall_FSR_stats,
                     gall_RSF_stats, gall_RSR_stats)


gall_FSF_totals["Treatment"] <- "FSF"
gall_FSR_totals["Treatment"] <- "FSR"
gall_RSF_totals["Treatment"] <- "RSF"
gall_RSR_totals["Treatment"] <- "RSR"

gall_totals <- rbind(gall_FSF_totals, gall_FSR_totals,
                     gall_RSF_totals, gall_RSR_totals)

write.table(gall_output, "wasp_data_clean/gallicus_stats_all.txt",
            sep = "\t",
            row.names = FALSE)

write.table(gall_totals, "wasp_data_clean/gallicus_total_all.txt",
            sep = "\t",
            row.names = FALSE)


#----- Obtain percentages/proportions for plotting -----
# How do we want the totals/means to be?
# Have 10 reps per wasp... so mean of totals is sum/num
#  
gall_FSF_pcts <- gall_FSF %>%
  group_by(number_wasps, w) %>%
  summarise(mean(alive),
            mean(nested),
            mean(usurp))

gall_FSF_pcts_2 <- gall_FSF_totals %>%
  group_by(Treatment, number_wasps, w) %>%
  summarise(sum(total_alive)/10,
            sum(total_nested)/10,
            sum(total_usurp)/10) %>%
  rename(mean_alive = "sum(total_alive)/10",
         mean_nested = "sum(total_nested)/10",
         mean_usurp = "sum(total_usurp)/10")



# plot 1 - for each wasp starting N, total nested vs viable nests
#  so need to get the proportions/percentage of nested vs viable first!!!
# idiot
wes_pal_1 <- wes_palette("Moonrise3", 16, type = "continuous")
wes_pal_2 <- wes_palette("Zissou1", 4)

# NEED TO SORT THIS TABLE -HAS TO BE SEPARATE BC SEPARATE VIABLES
# OR CALC ALL PCTS BEFORE COMBINE

gall_pcts <- gall_totals %>%
  group_by(Treatment, number_wasps) %>%
  summarise(pct_nested = 
              ((sum(mean(mean_nested) - mean(mean_usurp)))/gall_FSF_viable_nests)*100,
            pct_usurp = 
              ((sum(mean_usurp))/gall_FSF_viable_nests)*100)

gall_pcts <- gall_totals %>%
  group_by(Treatment, number_wasps) %>%
  summarise(nested = ((mean(total_nested) - mean(total_usurp))
                      /gall_FSF_viable_nests)*100)


nest_via_2 <- ggplot(gall_pcts, aes(x = factor(number_wasps), y = nested)) +
  geom_point(aes(colour = factor(number_wasps), size = 2)) +
  scale_color_manual(values = wes_pal_1) + 
  geom_line()
nest_via

# in geom_line, use different treatments, FSF, FSR, RSF, RSR



#-----  Plot 1: Number of Wasps vs Nested -----
# x = number of starting wasps
# y = nested
# Using totals
# Using means

wes_pal_1 <- wes_palette("Moonrise3", 4)

total_nest <- ggplot(gall_totals,
                     aes(x = factor(number_wasps),
                         y = total_nested)) +
  geom_point(aes(colour = Treatment))
  
gall_NWvN <- gall_totals %>%
  group_by(Treatment, number_wasps) %>%
  summarise(total_nested = sum(mean(total_nested)) - mean(total_usurp))

total_nest <- ggplot(gall_NWvN,
                     aes(x = factor(number_wasps),
                         y = total_nested,
                         colour = Treatment)) +
  geom_point() + 
  scale_colour_manual(values = wes_pal_1)
total_nest

total_nest2 <- ggplot(gall_NWvN,
                     aes(x = factor(number_wasps),
                         y = total_nested)) +
  geom_point(aes(shape = Treatment), size = 2) + 
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) + 
  scale_colour_manual(values = wes_pal_1)
total_nest2

gall_mean_NWvN <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  summarise(mean_nested = sum(mean(mean_nested) - mean(mean_usurp)))

mean_nest <- ggplot(gall_mean_NWvN,
                    aes(x = factor(number_wasps),
                        y = mean_nested)) +
  geom_point(aes(shape = Treatment), size = 2) + 
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) + 
  scale_colour_manual(values = wes_pal_1)
mean_nest

# Plot shows random start and patches give rise to more random
# trends



#----- Plot 2: Number of wasps vs Usurp -----
# x = number of wasps
# y = usurp
# using totals and means
gall_NWvU <- gall_totals %>%
  group_by(Treatment, number_wasps) %>%
  summarise(total_usurp = sum(mean(total_usurp)))

total_usurp <- ggplot(gall_NWvU,
                      aes(x = factor(number_wasps),
                          y = total_usurp)) +
  geom_point(aes(shape = Treatment), size = 2) + 
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) + 
  scale_colour_manual(values = wes_pal_1)
total_usurp

gall_mean_NWvU <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  summarise(mean_usurp = mean(mean_usurp))

mean_usurp <- ggplot(gall_mean_NWvU,
                    aes(x = factor(number_wasps),
                        y = mean_usurp)) +
  geom_point(aes(shape = Treatment), size = 2) + 
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) + 
  scale_colour_manual(values = wes_pal_1) + 
  theme_classic()
mean_usurp




#-----  Plot 3: Number of Steps vs Number of Wasps  -----
# x = number of wasps
# y = number of steps
# using mean

gall_mean_NWvS <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  summarise(mean_steps = mean(mean_steps))

mean_steps <- ggplot(gall_mean_NWvS,
                     aes(x = factor(number_wasps),
                         y = mean_steps)) +
  geom_point(aes(shape = Treatment), size = 2) +
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) +
  scale_colour_manual(values = wes_pal_1)
mean_steps

mean_steps_box <- ggplot(gall_output,
                         aes(x = factor(number_wasps),
                             y = mean_steps)) +
  geom_boxplot()

mean_steps_box

# Better as a boxplot


#-----  Plot 4: Number of steps to Nest for NW -----
# x = number of wasps
# y = number of steps
# filter out wasps that have nested
# using mean steps

gall_SvNW <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  filter(mean_nested > 0.5) %>%
  summarise(mean_steps = mean(mean_steps)) %>%
  ggplot(aes(x = factor(number_wasps),
             y = mean_steps)) +
  geom_point(aes(shape = Treatment), size = 2) +
  geom_line(aes(group = Treatment)) +
  aes(colour = Treatment) +
  scale_colour_manual(values = wes_pal_1)

gall_SvNW


# boxplot
gall_SvNW_box <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  filter(mean_nested > 0.5) %>%
  ggplot(aes(x = factor(number_wasps),
             y = mean_steps,
             fill = Treatment)) +
  geom_boxplot()
# facet
gall_SvNW_box <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  filter(mean_nested > 0.5) %>%
  ggplot(aes(x = factor(number_wasps),
             y = mean_steps,
             fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~as.factor(Treatment))

gall_SvNW_box

# need to split boxplots by treatment

# There is some difference between steps for actual nested and death



#-----  Plot 5: Number of Steps to Usurp  -----
gall_SvU_point <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  filter(mean_usurp >= 0.5) %>%
  summarise(mean_usurp = sum(mean(mean_usurp))) %>%
  ggplot(aes(x = factor(number_wasps),
             y = mean_usurp)) +
  geom_point() +
  geom_line(aes(group = Treatment)) + 
  aes(colour = Treatment)
gall_SvU_point

gall_SvU_box <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  filter(mean_usurp >= 0.5) %>%
  ggplot(aes(x = factor(number_wasps),
             y = mean_usurp)) +
  geom_boxplot()

gall_SvU_box

gall_SvU <- gall_output %>%
  group_by(Treatment, number_wasps) %>%
  summarise(mean_usurp = sum(mean(mean_usurp)))

gall_SvU_box_2 <- ggplot(gall_SvU,
                         aes(x = factor(number_wasps),
                             y = mean_usurp)) +
  geom_boxplot()
gall_SvU_box_2

#need to split boxplots by treatment

# Dominula
# 1. steps vs survival and nesting
# 2. nested vs viable nests
# 3. nested vs usurp
# 4. cohabited vs viable nests
# 5. steps vs cohabited
# 6. number of species in nest vs steps
# 7. number of starting wasps vs average number of steps to nest

dom_FSF_stats <- read.table("wasp_data_clean/dom_FSF_stats.txt",
                             header = TRUE, sep = "\t")
dom_FSR_stats <- read.table("wasp_data_clean/dom_FSR_stats.txt",
                             header = TRUE, sep = "\t")
dom_RSF_stats <- read.table("wasp_data_clean/dom_RSF_stats.txt",
                             header = TRUE, sep = "\t")
dom_RSR_stats <- read.table("wasp_data_clean/dom_RSR_stats.txt",
                             header = TRUE, sep = "\t")

dom_FSF_totals <- read.table("wasp_data_clean/dom_FSF_stats.txt",
                              header = TRUE, sep = "\t")
dom_FSR_totals <- read.table("wasp_data_clean/dom_FSR_stats.txt",
                              header = TRUE, sep = "\t")
dom_RSF_totals <- read.table("wasp_data_clean/dom_RSF_stats.txt",
                              header = TRUE, sep = "\t")
dom_RSR_totals <- read.table("wasp_data_clean/dom_RSR_stats.txt",
                              header = TRUE, sep = "\t")

