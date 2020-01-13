#-----  Wasps Mixed Species Data Cleaning -----

# 1. Load all relevant files together
# 2. Make sure starting number of wasps column is correct
# 3. Make sure species column is correct
# 4. Make sure repetition column is there - or averaged correctly

library(tidyverse)
library(reshape2)


#-----  Loading files -----

# Model output stored in single species, single treatment, single nest,
#   single rep text files
#   e.g. dom_mix_d10g10_w100_nest1_1.txt
#   dominula only, 10 starting dom nests, 10 gallicus nests, 100 wasps in each, 
#     nest 1, rep 1

# Need to add cols for species, number_nests, which nest
# Rename num_wasps_nest column from V11
# Reorder
# Get viable nests from patch files

# Can combine into a single text file - but still species separate due to conested
# When comparing species survival, can combine species files but need to remove conested
#   from table
# When combining, get average and totals from the 10 trials, same as single species
# Could add artificial columns to gallicus for coNested = 0 and number wasps in nest = 1
# That way, species data could be combined.. but not sure


# Load batch files function
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}



#-----  Dominula first  -----

# Dom10 gall10
dom10gall10 <- batch_read(path = "wasp_data_output/mixed_spp/",
                          pattern = "dominula_mix_d10g10_w100_",
                          )
