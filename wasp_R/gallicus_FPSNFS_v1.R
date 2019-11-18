#-----  Gallicus -----

#- Fixed Patch Size, Single Nest, Fixed Start
#   
#   In this set of simulations, number of wasps will increase by 10,
#     from 50-200.
#   All wasps will originate from a single nest.
#   The starting coordinates generated in the first simulation
#   (i.e. 50 wasps) will be used for every subsequent simulation.
#   A habitat will be generated only once & for each simulation.
#   Each simulation will be repeated 10 times.
#   The fieldArena will need to be reset before each nest loop
#   Move resetting of fieldArena into loop
#     initiation


# P gallicus has 2 possible choices when encountering a nest:
#   1. Disperse further to find a new nest
#   2. Attempt to usurp  the nest if already occupied - rare & potentially 
#     highly costly
# If the cell is empty, the indivdiual always stops moving and occupies 
#   the nest
# If the cell is already occupied, there is a chance (e.g. 5%) it will 
#   compete 
#   to take over the nest
#   ASSUMPTION - once competition starts, the new individual always wins - 
#     obviously this is not biologically accurate
# Individuals appear in numbers up to 100 - at the start of the simulation


#----- Environment:  -----
#
# A standard matrix x, y: 100 x 100
# A lattice grid structure, using x, y but allowing for movement in 6 possible
#   directions  
# Set original starting cell for 1st hatching
# Record cells in which nests are made 
#
# The arena will be created as a hexagonal lattice, with the vertical
#   sides acting as rows and the diagonals leading off into columns.
#   Columns are vertical but with every other cell in the column 
#   offset in alignment.
#   This means there are 6 possible directions a wasp can move in.



#-----  Model Assumptions -----

# At the start of the model, all wasps assumed to be alive.
#   This needs to be set before the simulation starts, as 
#   there is an initial check to see if wasps are alive 
#   before moving on.
# Wasps are also assumed to have not nested at the start 
#   of the model - to initiate movement.
# All wasps assumed to have same probability of behavioural
#   traits - movement, nesting, usurping, mortality.






#----- Setup  -----
library(tictoc)

# Import scripts containing necessary functions for the model
#   - createHabitat
#   - nextStep
source("wasp_R/build_Landscape_v5.R")
source("wasp_R/waspFunctions.R")


# 1. Create a list to store all wasp information from the model
gallicus <- list()
totalGallicus <- list() # store all gallicus lists from sims

# 2. Create a list to store all individual path histories
gallPath <- list()
simPath <- list() # store all paths from sims

# 3. Create a data frame to store wasp data once the wasp has nested
gallData <- data.frame(waspID = NA,
                       nSteps = NA,
                       nSteps = NA,
                       startX = NA,
                       startY = NA,
                       waspAlive = NA,
                       waspNested = NA,
                       waspUsurp = NA,
                       finalX = NA,
                       finalY = NA)



# Number of wasps in the model
#gallN <- 100

# Probability of mortality at any given point in time
pgMort <- 0.002

# Probability of usurping
pgUsurp <- 0.05



#-----  Simulations -----

# Create habitat
a <- createHabitat(100, 100, 10, 0.4)

a <- fieldArena   # cheap hack to store fieldArena for repeated use

# Explore habitat
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)

plot(which(fieldArena == 0, arr.ind = T),
     ylim = c(0, 100),
     xlim = c(0, 100))

# Write table to file
write.table(fieldArena,
            "wasp_data_output/gallicus_FPSNFS/FSF_arena_1.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/gallicus_FPSNFS/patchCoords_FSF_1.txt",
            row.names = FALSE)

# read field arena when needed
fieldArena <- as.matrix(read.delim2("wasp_data_output/gallicus_FPSNFS/FSF_arena_1.txt"),
                        sep = "\t")

# Get field dimension
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])


# Generate starting coordinates
startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldX), 1, replace = T)

points(startX, startY, col = "red", pch = 4)

# Loop

for (g in seq(50, 200, 10)) {
  
  gallN <- g        # number of wasps in nest
  fieldArena  <- a  # reset fieldArena
  
  for (w in 1:gallN) {
    
    # initiate path history for each wasp
    pathHist <- data.frame(step = 0, X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    #set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # set starting conditions of each wasp
    alive   <- 1
    nested  <- 0
    usurp   <- 0
    s       <- 1
    
    # start search & move loop
    while (alive == 1 & nested == 0) {
      
      # check if wasp is alive
      # IF ALIVE, then run the loop
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      if (alive == 1) {
        
        # movement stage
        newCells <- nextStep()
        
        # add new cells to path history
        newPath <- c(s, newCells[[1]], newCells[[2]])
        gallPath[[w]][s+1, ] <- newPath
        
        # move wasp to new cell - new cells become current cells
        currentX <- newCells[[1]]
        currentY <- newCells[[2]]
        
        
        # check if cell is occupied
        if (fieldArena[currentX, currentY] == 0) {
          
          # if the cell is empty, then the wasp nests
          nested <- 1
          fieldArena[currentX, currentY] <- 1
          
        } else {
          
          if (fieldArena[currentX, currentY] == 1) {
            
            # if the cell is already occupied
            # check if usurp
            usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                            1, replace = T)
            
            # if the wasp usurps, then replace in nest
            if (usurp == 1) {
              
              nested <- 1
              fieldArena[currentX, currentY] <- 1
              
            }
            
          } 
          
        } # end cell occupied
        
      } # end if alive
      
      # store results
      gall <- cbind(w, s, startX, startY,
                    alive, nested, usurp,
                    currentX, currentY)
      
      #print(cbind(w, s, alive, nested))
      
      # move to next step 
      s <- s + 1
      
    } # end while
    
    gallicus[[w]] <- gall
    
  }
  
  simPath[[g]]        <- gallPath
  totalGallicus[[g]]  <- gallicus
  
  
}




#----- Convert output -----

totalGallicus2 <- totalGallicus[!sapply(totalGallicus, is.null)]

gallicusOutList <- list()


for(i in 1:length(totalGallicus2)) {
  gallicusOutList[[i]] <- do.call("rbind", totalGallicus2[[i]])
}


simPath2 <- simPath[!sapply(simPath, is.null)]

simPathOutList <- list()
for (i in 1:length(simPath2)) {
  simPathOutList[[i]] <- do.call("rbind", simPath2[[i]])
}


#----- Write output -----
# Just have to change the number in the file name

for (i in 1:length(gallicusOutList)) {
  write.table(gallicusOutList[[i]],
              paste0("wasp_data_output/gallicus_FPSNFS/gallicus_",
                     "FSF_",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/gallicus_FPSNFS/gallicus_",
                     "FSF_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


rm(list = setdiff(ls(), lsf.str()))
