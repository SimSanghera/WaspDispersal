#-----  Dominula  -----

#- Random Patch Size, Multiple Nest, Random Start
#   
#   In this set of simulations, there will be 5-20 nests with 100
#     wasps, incresing by 5 nests each time.
#   Starting coordinates will be randomly generated for each individual nest.
#   A habitat will be generated for each simulation.
#   Each simulation will be repeated 10 times.
#   The fieldArena will need to be reset before each nest loop
#   Move resetting of fieldArena into loop initiation

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


#----- Set up the Parameters   -----
#
# At the start of the simulation, all wasps assumed to be alive.
#   This needs to be set before simulation starts, as there is an initial
#   check to see if wasps are alive before moving on.
# The same needs to be done for "nested", assuming wasps have not nested 
#   at the start of the simulation to initiate movement.
# Probabilities for certain actions vary within dominula dependent upon
#   ecological assumptions - so multiple models should be run
# Run different number of nests on same habitats - 
#   i.e. hab 1 for 10, 15, 20, 50; etc


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
#domN <- 100

# Probability of mortality
pdMort <- 0.002

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

# Create habitat
a <- createHabitat(100, 100, 10, 0.4) # only run once. then load file below

# Explore habitat
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)

plot(which(fieldArena == 0, arr.ind = T),
     ylim = c(0, 100),
     xlim = c(0, 100))

# store habitat when satisifed
# then, load habitat before each simulation to ensure it is reset
write.table(fieldArena,
            "wasp_data_output/dominula_RPMNRS/RMR_arena_n10_10.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/dominula_RPMNRS/patchCoords_RMR_n10_10.txt",
            row.names = FALSE)

# load field arena
fieldArena <- 
  as.matrix(read.delim2("wasp_data_output/dominula_RPMNRS/RMR_arena_n10_10.txt"),
            sep = "\t")



a <- fieldArena   # cheap hack to store fieldArena for repeated use


# Get field dimension
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])



#-----  Simulation  -----

for (n in 1:15) {
  
  domN <- 100
  
  # starting coordinates
  startX <- sample(c(1:fieldX), 1, replace = T)
  startY <- sample(c(1:fieldY), 1, replace = T)
  
  for (w in 1:domN) {
    
    # initiate path history for each wasp
    pathHist      <- data.frame(nest = 0, w = 0, 
                                step = 0, X = startX, Y = startY)
    domPath[[w]]  <- data.frame(pathHist)
    
    # set current X & Y
    currentX <- startX
    currentY <- startY
    
    # set starting conditions for each wasp
    alive     <- 1
    nested    <- 0
    usurp     <- 0
    s         <- 1
    coNested  <- 0
    
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
        newPath               <- c(n, w, s, newCells[[1]], newCells[[2]])
        domPath[[w]][s + 1, ] <- newPath
        
        # move wasp to new cell
        currentX <- newCells[[1]]
        currentY <- newCells[[2]]
        
        # check if cell is occupied
        if (fieldArena[currentX, currentY] == 0) {
          
          # 1. if cell is empty, wasp nests
          nested <- 1
          fieldArena[currentX, currentY] <- 1
          
        } else {
          
          # 2. check if occupied by a single wasp
          #   options = usurp, join, move on
          if (fieldArena[currentX, currentY] == 1) {
            
            # check if wasp usurps
            usurp <- sample(c(0, 1), prob = c((1 - pdUsurp), pdUsurp),
                            1, replace = T)
            
            if (usurp == 1) {
              
              nested <- 1
              fieldArena[currentX, currentY] <- 1
              
            } else {
              
              # if wasp doesn't usurp, check if it joins
              if (usurp == 0) {
                
                pdJoin <- sample(c(0, 1), prob = c(0.475, 0.475),
                                 1, replace = T)
                
                if (pdJoin == 1) {
                  
                  nested      <- 1
                  coNested    <- 1
                  fieldArena[currentX, currentY] <- fieldArena[currentX, currentY] + 1
                  
                } # end single join
                
              } # end not usurp
              
            }
            
          } else {
            
            # 3. more than 1 wasp
            if (fieldArena[currentX, currentY] >= 2 &
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
                
                nested      <- 1
                coNested    <- 1
                fieldArena[currentX, currentY] <- fieldArena[currentX, currentY] + 1
                
              }
              
            }
            
          }
          
        }
        
      } # end if alive
      
      # store results
      dom <- cbind(w, s, startX, startY,
                   alive, nested, coNested, usurp,
                   currentX, currentY,
                   fieldArena[currentX, currentY])
      
      # print(cbind(d, w, s, alive, nested))
      
      # move wasp to next step
      s <- s + 1
      
    } # end while
    
    dominula[[w]] <- dom
    
  }
  
  simPath[[n]]        <- domPath
  totalDominula[[n]]  <- dominula
  
}



#-----  Convert output  -----

nests <- list()

for (i in 1:length(totalDominula)) {
  nests[[i]] <- as.data.frame(do.call("rbind", totalDominula[[i]]))
}


simPath2 <- simPath[!sapply(simPath, is.null)]

simPathOutList <- list()
for (i in 1:length(simPath2)) {
  simPathOutList[[i]] <- as.data.frame(do.call("rbind", simPath2[[i]]))
}


#-----  Write Output  -----
# Just have to change the number in the file name

for (i in 1:length(nests)) {
  write.table(nests[[i]],
              paste0("wasp_data_output/dominula_RPMNRS/dominula_",
                     "RMR_",
                     "n15",
                     "_",
                     "w",
                     nrow(nests[[i]]),
                     "_nest",
                     i,
                     "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/dominula_RPMNRS/dominula_",
                     "RMR_path",
                     "_n15",
                     "_",
                     "_w",
                     nrow(nests[[i]]),
                     "_nest",
                     i,
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}



rm(list = setdiff(ls(), lsf.str()))

