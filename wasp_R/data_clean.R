#----- Averaging Wasps Data


allGallFiles <- list.files("wasp_data_output/gallicusRSF50",
                           pattern = "*.txt",
                           full.names = TRUE)

allGallFiles

gallFiles <- lapply(allGallFiles, read.table)

# have a list of 10 files, need to be able to put them in the same rows,
#   linked by wasp

# need cleaning first

gallFiles[[2]]
dim(gallFiles[[1]])

rownames(gallFiles[[1]]) <- c()

gallicus501 <- gallFiles[[1]]
rownames(gallicus501) <- c()
