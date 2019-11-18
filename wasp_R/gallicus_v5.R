#-----  Gallicus Model  -----


#----- Notes -----

# Wasps:
# 2 Species - Polistes gallicus & dominula
# 
# P gallicus has 2 possible choices when encountering a nest:
#   1. Disperse further to find a new nest
#   2. Attempt to usurp the nest if it is already occupied - this is rare 
#       and potentialy highly costly, thus has a low probability of occurrence
# If the cell is empty, the indivdiual always stops moving and occupies the nest
# If the cell is already occupied, there is a chance (e.g. 5%) it will compete 
#   to take over the nest
#   ASSUMPTION - once competition starts, the new individual always wins - 
#     obviously this is not biologically accurate
#   Individuals appear in numbers up to 100 - at the start of the simulation


#----- Environment:  -----
#
# A standard matrix x, y: 1000 x 1000
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


#-----  Model Assumptions -----

# At the start of the model, all wasps assumed to be alive.
#   This needs to be set before the simulation starts, as 
#   there is an initial check to see if wasps are alive 
#   before moving on.
# Wasps are also assumed to have not nested at the start 
#   of the model - to initiate movement.
# All wasps assumed to have same probability of behavioural
#   traits - movement, nesting, usurping, mortality.



#-----  Setup -----
library(tictoc)

# import scripts containing necessary functions for the model
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

#----- 1. Random Patch Size, Single Nest, Fixed Starting Coord -----
#
# In this set of simulations, the number of wasps will increase
#   by 10, from 50-200.
# All wasps will originate from a single nest.
# The starting coordinates generated in the first simulation
#   (i.e. 50 wasps) will be used for every subsequent simulation.
# A habitat will be generated for each simulation.
# Each simulation will be repeated 10 times.

a <- createHabitat(100, 100, 10, 0.4)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)


for (g in seq(50, 200, 10)) {
  gallN <- g
  
  for (w in 1:gallN) { # for each wasp
    
    # Initiate a path history for each wasp
    pathHist <- data.frame(step = 0, X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    # Set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # Set starting conditions of nested and number of steps (s)
    nested <- 0
    s <- 1
    alive <- 1
    
    while (alive == 1 & nested == 0) { # until the wasp nests
      
      # check if wasp is alive 
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      # If the wasp is alive, the loop will run
      
      # Movement Stage
      newCells <- nextStep()
      
      # # check if new cells are in path history
      # while(any(newCells[[1]] == waspPath[[w]][1] &
      #           newCells[[2]] == waspPath[[w]][2])) {
      #   newCells <- nextStep()
      # }
      
      # Once new cells selected, wasp "moves"
      # New cells added to path history
      newPath <- c(s, newCells[[1]], newCells[[2]])
      gallPath[[w]][s+1, ] <- newPath
      
      # Move wasp to new cell - newCells become current cells
      currentX <- newCells[[1]]
      currentY <- newCells[[2]]
      
      
      # Check if the cell is occupied
      if (fieldArena[currentX, currentY] == 0) {
        nested <- 1
        usurp <- 0
        fieldArena[currentX, currentY] <- 1
      }
      
      
      # if the cell is occupied
      if (fieldArena[currentX, currentY] == 1) {
        # check if usurps
        usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                        1, replace = T)
        
        # if usurps, then nest
        if (usurp == 1) {
          nested <- 1
          fieldArena[currentX, currentY] <- 1
        }
        
      }
      
      # if non-viable cell
      if(fieldArena[currentX, currentY] == -1) {
        nested <- 0
        usurp <- 0
      }
      
      s <- s + 1
      # Store results
      #print(rbind(usurp, alive, s, nested))
      
      # remove t
      gall <- cbind(w, newPath[1], startX, startY, alive, nested,
                    usurp, currentX, currentY, newCells[[1]], newCells[[2]])
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
}


# #
# 
# gallN <- 50
# 
# waspTotal <- seq(50, 200, by = 10)
# totalList <- list()
# gall <- list()
# 
# for (w in seq(50, 200, 10)) {
#   
#   gallN <- w
#   
#   for (g in 1:gallN) {
#     gall[[g]] <- seq(1:gallN)
#   }
# totalList[[w]] <- gall[[g]]
# }
# 
# for (t in seq(50, 200, 10)) {
#   gallN <- t
#   for (i in 1:gallN) {
#     
#     gall[[i]] <- cbind(1, 0, 1, 0, 1, 0, 5)
#     
#   }
#   
#   totalList[[t]] <- gall
#   
# }



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
              paste0("wasp_data_output/100x100/gallicus_",
                    "RSF_",
                    nrow(gallicusOutList[[i]]),
                    "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/100x100/gallicus_",
                     "RSF_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}

write.table(fieldArena,
            "wasp_data_output/100x100/RSF_arena10.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/100x100/patchCoords_RSF_10.txt",
            row.names = FALSE)



rm(list = setdiff(ls(), lsf.str()))


#-----





#----- 2. Random Patch Size, Single Nest, Random Starting Coord -----
#
# In this set of simulations, the number of wasps will increase
#   by 10, from 50-200.
# All wasps will originate from a single nest.
# Different starting coordinates will be randomly generated for
#   every wasp nest.
# A habitat will be generated for each simulation.
# Each simulation will be repeated 10 times.

a <- createHabitat(100, 100, 10, 0.4)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])



for (g in seq(50, 200, 10)) {
  gallN <- g
  
  startX <- sample(c(1:fieldX), 1, replace = T)
  startY <- sample(c(1:fieldY), 1, replace = T)
  
  for (w in 1:gallN) { # for each wasp
    
    # Initiate a path history for each wasp
    pathHist <- data.frame(step = 0, X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    # Set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # Set starting conditions of nested and number of steps (s)
    nested <- 0
    s <- 1
    alive <- 1
    
    while (alive == 1 & nested == 0) { # until the wasp nests
      
      # check if wasp is alive 
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      # If the wasp is alive, the loop will run
      
      # Movement Stage
      newCells <- nextStep()
      
      # # check if new cells are in path history
      # while(any(newCells[[1]] == waspPath[[w]][1] &
      #           newCells[[2]] == waspPath[[w]][2])) {
      #   newCells <- nextStep()
      # }
      
      # Once new cells selected, wasp "moves"
      # New cells added to path history
      newPath <- c(s, newCells[[1]], newCells[[2]])
      gallPath[[w]][s+1, ] <- newPath
      
      # Move wasp to new cell - newCells become current cells
      currentX <- newCells[[1]]
      currentY <- newCells[[2]]
      
      
      # Check if the cell is occupied
      if (fieldArena[currentX, currentY] == 0) {
        nested <- 1
        usurp <- 0
        fieldArena[currentX, currentY] <- 1
      }
      
      
      # if the cell is occupied
      if (fieldArena[currentX, currentY] == 1) {
        # check if usurps
        usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                        1, replace = T)
        
        # if usurps, then nest
        if (usurp == 1) {
          nested <- 1
          fieldArena[currentX, currentY] <- 1
        }
        
      }
      
      # if non-viable cell
      if(fieldArena[currentX, currentY] == -1) {
        nested <- 0
        usurp <- 0
      }
      
      s <- s + 1
      # Store results
      #print(rbind(usurp, alive, s, nested))
      
      # remove t
      gall <- cbind(w, newPath[1], startX, startY, alive, nested,
                    usurp, currentX, currentY, newCells[[1]], newCells[[2]])
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
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

for (i in 1:length(gallicusOutList)) {
  write.table(gallicusOutList[[i]],
              paste0("wasp_data_output/100x100/gallicus_",
                     "RSR_",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/100x100/gallicus_",
                     "RSR_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


write.table(fieldArena,
            "wasp_data_output/100x100/RSR_arena10.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/100x100/gallicus_patchCoords_RSR_10.txt",
            row.names = FALSE)



rm(list = setdiff(ls(), lsf.str()))




#












#-----


#----- 3. Random Patch Size, Multiple Nest, Fixed Starting Coord -----
#
# In this set of simulations, the number of wasps will increase
#   by 500, from 100-2000.
# All wasps will originate from a single nest.
# The starting coordinates generated in the first simulation
#   (i.e. 100 wasp nests) will be used for every subsequent simulation.
# A habitat will be generated for each simulation.
# Each simulation will be repeated 10 times.

nestList <- list()
nestPath <- list()
trackloop <- data.frame(g = NA, n = NA, w = NA)
tracking <- list()
trackingAll <- list()

a <- createHabitat(1000, 1000, 10, 0.4)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

write.table(fieldArena,
            "wasp_data_output/RMF_arena1.txt",
            row.names = FALSE,
            sep = "\t")

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)


tic("total")
for (g in seq(100, 200, 50)) {
  
  # number of nests 
  numNests <- g/10
  
  for (n in 1:numNests) {
    
    # set number of wasps
    gallN <- 100
    
    startX <- sample(c(1:fieldX), 1, replace = T)
    startY <- sample(c(1:fieldY), 1, replace = T)
    
    
    for (w in 1:gallN) { # for each wasp
      
      # Initiate a path history for each wasp
      pathHist <- data.frame(step = 0, X = startX, Y = startY)
      gallPath[[w]] <- data.frame(pathHist)
      
      # Set current X & Y cells
      currentX <- startX
      currentY <- startY
      
      # Set starting conditions of nested and number of steps (s)
      nested <- 0
      s <- 1
      alive <- 1
      
      while (alive == 1 & nested == 0) { # until the wasp nests
        
        # check if wasp is alive 
        alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                        1, replace = T)
        
        # If the wasp is alive, the loop will run
        
        # Movement Stage
        newCells <- nextStep()
        
        # # check if new cells are in path history
        # while(any(newCells[[1]] == waspPath[[w]][1] &
        #           newCells[[2]] == waspPath[[w]][2])) {
        #   newCells <- nextStep()
        # }
        
        # Once new cells selected, wasp "moves"
        # New cells added to path history
        newPath <- c(s, newCells[[1]], newCells[[2]])
        gallPath[[w]][s+1, ] <- newPath
        
        # Move wasp to new cell - newCells become current cells
        currentX <- newCells[[1]]
        currentY <- newCells[[2]]
        
        
        # Check if the cell is occupied
        if (fieldArena[currentX, currentY] == 0) {
          nested <- 1
          usurp <- 0
          fieldArena[currentX, currentY] <- 1
        }
        
        
        # if the cell is occupied
        if (fieldArena[currentX, currentY] == 1) {
          # check if usurps
          usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                          1, replace = T)
          
          # if usurps, then nest
          if (usurp == 1) {
            nested <- 1
            fieldArena[currentX, currentY] <- 1
          }
          
        }
        
        # if non-viable cell
        if(fieldArena[currentX, currentY] == -1) {
          nested <- 0
          usurp <- 0
        }
        
        s <- s + 1
        # Store results
        #print(rbind(usurp, alive, s, nested))
        
        # remove t
        gall <- cbind(w, newPath[1], startX, startY, alive, nested,
                      usurp, currentX, currentY, newCells[[1]], newCells[[2]])
        trackloop[w, ] <- cbind(g, n, w)
      } # end of step loop
      
      tracking[[n]] <- trackloop
      gallicus[[w]] <- gall
      
      
    }
    
    simPath[[n]] <- gallPath
    totalGallicus[[n]] <- gallicus # single nest into totalGallicus
    
    
  }
  
  trackingAll[[g]] <- tracking
  nestPath[[g]] <- simPath
  nestList[[g]] <- totalGallicus
  
}
toc()



#----- Convert output -----

totalGallicus2 <- totalGallicus[!sapply(totalGallicus, is.null)]

gallicusOutList <- list()


for(i in 1:length(totalGallicus2)) {
  gallicusOutList[[i]] <- do.call("rbind", totalGallicus2[[i]])
}


nestList2 <- nestList[!sapply(nestList, is.null)]
nestList2 <- nestList2[!sapply(nestList2, is.null)]


simPath2 <- simPath[!sapply(simPath, is.null)]

simPathOutList <- list()
for (i in 1:length(simPath2)) {
  simPathOutList[[i]] <- do.call("rbind", simPath2[[i]])
}


#----- Write output -----

for (i in 1:length(gallicusOutList)) {
  write.table(gallicusOutList[[i]],
              paste0("wasp_data_output/gallicus_",
                     "RSF_",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              sep = "\t")
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/gallicus_",
                     "RSF_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


write.table(patchCoords, 
            "wasp_data_output/patchCoords_RSF_10.txt",
            row.names = FALSE)



rm(list = setdiff(ls(), lsf.str()))



#-----

#----- 6. Fixed Patch Size, Single Nest, Fixed Starting Coord -----
#
# In this set of simulations, the number of wasps will increase
#   by 10, from 50-200.
# All wasps will originate from a single nest.
# The starting coordinates generated in the first simulation
#   (i.e. 50 wasps) will be used for every subsequent simulation.
# A habitat will be generated only once and used for all simulations.
# Each simulation will be repeated 10 times.

a <- createHabitatFixed(100, 100, 10, 0.3)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

write.table(fieldArena,
            "wasp_data_output/FSF_arena1.txt",
            row.names = FALSE,
            sep = "\t")


fieldArena <- as.matrix(read.delim2("wasp_data_output/FSF_arena1.txt",
                           sep = "\t"))


# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)

points(startX, startY, col = "red", pch = 4)

for (g in seq(50, 200, 10)) {
  gallN <- g
  
  for (w in 1:gallN) { # for each wasp
    
    # Initiate a path history for each wasp
    pathHist <- data.frame(step = 0, X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    # Set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # Set starting conditions of nested and number of steps (s)
    nested <- 0
    s <- 1
    alive <- 1
    
    while (alive == 1 & nested == 0) { # until the wasp nests
      
      # check if wasp is alive 
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      # If the wasp is alive, the loop will run
      
      # Movement Stage
      newCells <- nextStep()
      
      # # check if new cells are in path history
      # while(any(newCells[[1]] == waspPath[[w]][1] &
      #           newCells[[2]] == waspPath[[w]][2])) {
      #   newCells <- nextStep()
      # }
      
      # Once new cells selected, wasp "moves"
      # New cells added to path history
      newPath <- c(s, newCells[[1]], newCells[[2]])
      gallPath[[w]][s+1, ] <- newPath
      
      # Move wasp to new cell - newCells become current cells
      currentX <- newCells[[1]]
      currentY <- newCells[[2]]
      
      
      # Check if the cell is occupied
      if (fieldArena[currentX, currentY] == 0) {
        nested <- 1
        usurp <- 0
        fieldArena[currentX, currentY] <- 1
      }
      
      
      # if the cell is occupied
      if (fieldArena[currentX, currentY] == 1) {
        # check if usurps
        usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                        1, replace = T)
        
        # if usurps, then nest
        if (usurp == 1) {
          nested <- 1
          fieldArena[currentX, currentY] <- 1
        }
        
      }
      
      # if non-viable cell
      if(fieldArena[currentX, currentY] == -1) {
        nested <- 0
        usurp <- 0
      }
      
      s <- s + 1
      # Store results
      #print(rbind(usurp, alive, s, nested))
      
      # remove t
      gall <- cbind(w, newPath[1], startX, startY, alive, nested,
                    usurp, currentX, currentY, newCells[[1]], newCells[[2]])
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
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
              paste0("wasp_data_output/gallicus_",
                     "FSF_",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/gallicus_",
                     "FSF_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


rm(list = setdiff(ls(), lsf.str()))


#-----



#----- 7. Fixed Patch Size, Single Nest, Random Starting Coord -----
#
# In this set of simulations, the number of wasps will increase
#   by 10, from 50-200.
# All wasps will originate from a single nest.
# The starting coordinates generated in the first simulation
#   (i.e. 50 wasps) will be used for every subsequent simulation.
# A habitat will be generated only once and used for all simulations.
# Each simulation will be repeated 10 times.

a <- createHabitatFixed(100, 100, 10, 0.3)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

write.table(fieldArena,
            "wasp_data_output/FSR_arena1.txt",
            row.names = FALSE,
            sep = "\t")


fieldArena <- as.matrix(read.delim2("wasp_data_output/FSR_arena1.txt",
                                    sep = "\t"))


# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])



for (g in seq(50, 200, 10)) {
  gallN <- g
  
  startX <- sample(c(1:fieldX), 1, replace = T)
  startY <- sample(c(1:fieldY), 1, replace = T)
  
  for (w in 1:gallN) { # for each wasp
    
    # Initiate a path history for each wasp
    pathHist <- data.frame(step = 0, X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    # Set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # Set starting conditions of nested and number of steps (s)
    nested <- 0
    s <- 1
    alive <- 1
    
    while (alive == 1 & nested == 0) { # until the wasp nests
      
      # check if wasp is alive 
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      # If the wasp is alive, the loop will run
      
      # Movement Stage
      newCells <- nextStep()
      
      # # check if new cells are in path history
      # while(any(newCells[[1]] == waspPath[[w]][1] &
      #           newCells[[2]] == waspPath[[w]][2])) {
      #   newCells <- nextStep()
      # }
      
      # Once new cells selected, wasp "moves"
      # New cells added to path history
      newPath <- c(s, newCells[[1]], newCells[[2]])
      gallPath[[w]][s+1, ] <- newPath
      
      # Move wasp to new cell - newCells become current cells
      currentX <- newCells[[1]]
      currentY <- newCells[[2]]
      
      
      # Check if the cell is occupied
      if (fieldArena[currentX, currentY] == 0) {
        nested <- 1
        usurp <- 0
        fieldArena[currentX, currentY] <- 1
      }
      
      
      # if the cell is occupied
      if (fieldArena[currentX, currentY] == 1) {
        # check if usurps
        usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                        1, replace = T)
        
        # if usurps, then nest
        if (usurp == 1) {
          nested <- 1
          fieldArena[currentX, currentY] <- 1
        }
        
      }
      
      # if non-viable cell
      if(fieldArena[currentX, currentY] == -1) {
        nested <- 0
        usurp <- 0
      }
      
      s <- s + 1
      # Store results
      #print(rbind(usurp, alive, s, nested))
      
      # remove t
      gall <- cbind(w, newPath[1], startX, startY, alive, nested,
                    usurp, currentX, currentY, newCells[[1]], newCells[[2]])
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
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
              paste0("wasp_data_output/gallicus_",
                     "FSR_",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList)) {
  write.table(simPathOutList[[i]],
              paste0("wasp_data_output/gallicus_",
                     "FSR_path",
                     nrow(gallicusOutList[[i]]),
                     "_10",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}


rm(list = setdiff(ls(), lsf.str()))







#-----

