#-----  Dominula  -----

#- Random Patch Size, Single Nest, Fixed Start
#   
#   In this set of simulations, number of wasps will increase by 10,
#     from 50-200.
#   All wasps will originate from a single nest.
#   The starting coordinates generated in the first simulation
#   (i.e. 50 wasps) will be used for every subsequent simulation.
#   A habitat will be generated for each simulation.
#   Each simulation will be repeated 10 times.
#   The fieldArena will need to be reset before each nest loop
#   For fixed starting point, move resetting of fieldArena into loop
#     initiation


# P dominula as 3 possible choices when encountering a nest:
#   1. Disperse further to find a new spot
#   2. Attempt to usurp the nest
#   3. Join a pre-existing nest. Multiple females (2-25) can join one nest
# If the cell is empty, they will always stop moving and occupy
# If the cell is occupied, then it will either JOIN or MOVE on
#   Assumption: There is an equal chance of moving on or joining a nest
#   Assumption: Usurping only occurs if there is only a single current occupant
#   Possible percentages of actions:
#     1. Encounters 1 individual = 5% usurp, 47.5% move, 47.5% join
#     2. 2 - 8 individuals = 65% join, 35% move
#     3. 9 + individuals = 25% join, 75% move
# The optimum number of unrelated individuals in a nest ~ 8
#   TO incorporate into the model, simulate a higher probability of joining
#     when there are < 8 individuals in the nest - e.g. 67.5%
# Average number of foundresses on a nest, in reality, is less than 8 (~5). So,
#   a second simulation could be with 5 as the optimal number for a switch in %
# Max individuals on a nest = 25
# 
#     


#----- Environment:  -----
#
# A matrix x, y: 1000 x 1000
# A lattice grid structure, using x, y but allowing for movement in 6 possible
#   directions  
# Set original starting cell for 1st hatching
# Record cells in which nests are made
# Number of time steps: 100, 1000, 10000  
#
# The arena will be created as a hexagonal lattice, with the vertical
#   sides acting as rows and the diagonals leading off into columns.
#   Columns are vertical but with every other cell in the column 
#   offset in alignment.
#   This means there are 6 possible directions a wasp can move in.



#----- Paradigm   -----
#
# While a wasp is alive & un-nested, it is driven to move step-by-step
#   in search of an empty, viable nesting site (cell)
# It can move in all possible directions with equal probability
#
# When a viable nesting site is encountered, dominula has 4 possible 
#   options - NEST if EMPTY, CO-NEST if OCCUPIED, USURP, MOVE ON
# Once nested, record cell values & wasp status



#----- Set up the Parameters   -----
#
# At the start of the simulation, all wasps assumed to be alive.
#   This needs to be set before simulation starts, as there is an initial
#   check to see if wasps are alive before moving on.
# The same needs to be done for "nested", assuming wasps have not nested 
#   at the start of the simulation to initiate movement.
# Probabilities for certain actions vary within dominula dependent upon
#   ecological assumptions - so multiple models should be run
#


#-----  Setup -----
library(tictoc)

# import scripts containing necessary functions for the model
#   - createHabitat
#   - nextStep
source("wasp_R/build_Landscape_v5.R")
source("wasp_R/waspFunctions.R")


# 1. Create a list to store all wasp information from the model
dominula <- list()
totalDominula <- list() # store all dominula lists from sims

# 2. Create a list to store all individual path histories
domPath <- list()
simPath <- list() # store all paths from sims

# 3. Create a data frame to store wasp data once the wasp has nested
domData <- data.frame(waspID = NA,
                      nSteps = NA,
                      nSteps = NA,
                      startX = NA,
                      startY = NA,
                      waspAlive = NA,
                      waspNested = NA,
                      waspUsurp = NA,
                      finalX = NA,
                      finalY = NA)


# Number of wasps in model
domN <- 100

# Probability of mortality
pdMort <- 0.02

# Prob of usurping
pdUsurp <- 0.05

# Prob of joining any nest 
probJoinMany <- 0.25

# Prob joining nest with less than 8 wasps
probJoinOpt <- 0.65

# Optimal number of wasps & threshold
domOpt <- 8
domMax <- 25



#-----  Simulations -----

# Create Habitat
a <- createHabitat(100, 100, 10, 0.4)

a <- fieldArena

# Explore
# Explore the habitat if required
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)

plot(which(fieldArena == 0, arr.ind = T),
     ylim = c(0, 100),
     xlim = c(0, 100))


# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

# Generate starting coordinates
startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldX), 1, replace = T)


# Loop
for (d in seq(50, 200, 10)) { # for each nest
  
  domN <- d
  fieldArena <- a   # reset fieldArena
  
  for (w in 1:domN) { # for each wasp
    
    # initiate path history for each wasp
    pathHist      <- data.frame(step = 0, X = startX, Y = startY)
    domPath[[w]]  <- data.frame(pathHist)
    
    # set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # set starting conditions for wach wasp
    alive     <- 1
    nested    <- 0
    usurp     <- 0
    s         <- 1
    coNest    <- 0
    
    # start search & move loop
    while (alive == 1 & nested == 0) {
      
      # check if wasp is alive
      alive <- sample(c(0, 1), prob = c(pdMort, (1 - pdMort)),
                      1, replace = T)
      
      # if the wasp is alive, continue
      if (alive == 1) {
        
        # movement stage
        newCells <- nextStep()
        
        # add new cells to path history
        newPath   <- c(s, newCells[[1]], newCells[[2]])
        domPath[[w]][s+1, ] <- newPath
        
        # move wasp to new cell
        currentX <- newCells[[1]]
        currentY <- newCells[[2]]
        
        
        # check if cell is occupied
        if (fieldArena[currentX, currentY] == 0) {
          
          # 1. if the cell is empty, wasp nests
          nested <- 1
          fieldArena[currentX, currentY] <- 1
          
        } else {
          
          # 2. check if occupied by only a single wasp
          #  options - usurp, join, move on
          if (fieldArena[currentX, currentY] == 1) {
            
            # check if wasp usurps
            usurp <- sample(c(0, 1), prob = c((1 - pdUsurp), pdUsurp),
                            1, replace = T)
            
            if (usurp == 1) {
              
              nested <- 1
              fieldArena[currentX, currentY] <- 1
              
            } else {
              
              # if wasp doesn't usurp, check if joins
              if (usurp == 0) {
                
                pdJoin <- sample(c(0, 1), prob = c(0.475, 0.475),
                                 1, replace = T)
                
                if (pdJoin == 1) {
                  
                  nested <- 1
                  coNest <- 1
                  fieldArena[currentX, currentY] <- fieldArena[currentX, currentY] + 1
                  
                } # end join
                
              } 
              
            } # end if not usurp
            
          } # end of single wasp
          
        } else {
          
          # 3. More than 1 wasp
          if (fieldArena[currentX, currentY] >= 2 &&
              fieldArena[currentX, currentY] < domMax) {
            
            # split statement due to optimal join threshold
            if (fieldArena[currentX, currentY] < domOpt) {
              
              probJoin <- probJoinOpt
              
            } else {
              
              probJoin <- probJoinMany
              
            }
            
            # now check if wasp joins
            pdJoin <- sample(c(0, 1), prob = c((1 - probJoin), probJoin),
                             1, replace = T)
            
            if (pdJoin == 1) {
              
              nested <- 1
              coNest <- 1
              fieldArena[currentX, currentY] <- fieldArena[currentX, currentY] + 1
              
            }
            
          } 
          
        } # end multi wasp
        
      } # end if alive
      
      dom <- cbind(w, s, startX, startY,
                   alive, nested, usurp,
                   coNest, currentX, currentY,
                   fieldArena[currentX, currentY])
      
      print(cbind(w, s, alive, nested, coNest, usurp,
                  fieldArenap[currentX, currentY]))
      
      # move to next step
      s <- s + 1
      
      
    } # end while loop
    
    dominula[[w]] <- dom
    
  }
  
  simPath[[d]]        <- domPath
  totalDominula[[d]]  <- dominula
  
}




#-----  Convert output  -----

totalDominula2 <- totalDominula[!sapply(totalDominula, is.null)]

dominulaOutlist <- list()

for (i in 1:length(totalDominula2)) {
  dominulaOutlist[[i]] <- do.call("rbind", totalDominula2[[i]])
}


simPath2 <- simPath[!sapply(simPath, is.null)]

simPathOutlist <- list()
for (i in 1:length(simPath2)) {
  simPathOutlist[[i]] <- do.call("rbind", simPath2[[i]])
}



#----- Write output -----
# Just have to change the number in the file name

for (i in 1:length(dominulaOutlist)) {
  write.table(dominulaOutlist[[i]],
              paste0("wasp_data_output/dominula_RPSNFS/dominula_",
                     "RSF_",
                     nrow(dominulaOutlist[[i]]),
                     "_1",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutlist)) {
  write.table(simPathOutlist[[i]],
              paste0("wasp_data_output/dominula_RPSNFS/dominula_",
                     "RSF_path",
                     nrow(dominulaOutlist[[i]]),
                     "_1",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


write.table(fieldArena,
            "wasp_data_output/dominula_RPSNFS/RSF_arena1.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/dominula_RPSNFS/patchCoords_RSF_1.txt",
            row.names = FALSE)



rm(list = setdiff(ls(), lsf.str()))