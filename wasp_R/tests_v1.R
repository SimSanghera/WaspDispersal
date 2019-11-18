# tests

# Multiple starting nests for lots of wasps


nestList <- list()      # list for everything - all nests with all wasps
gallicus <- list()      # list for wasps from one nest
totalGallicus <- list() # list for each wasp from all sims


for (g in seq(1, 10, by = 1)) {
  print(g)
  numNests <- g
  
  for (n in 1:numNests) {
    print(n)
    gallN <- 10
    
    for (w in 1:gallN) {
      print(w)
      
      for (x in 1:10) {
        
        gall <- x*2
        print(gall)
        gallicus[[w]] <- gall
      }    
      
    }
    
    totalGallicus[[n]] <- gallicus
    
  }
  
  
  nestList[[g]] <- totalGallicus
  
}


# 
library(tidyverse)



##############################

a <- createHabitat(1000, 1000, 10, 0.4)
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T))

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)

gallicusMod <- function() {}

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




#######
# testing while loops
alive <- 1
nested <- 1
test <- list()

for (i in 1:10) {
  alive <- 1
  nested <- 0
  
  while(alive == 1 & nested == 0) {
  
  alive <- sample(c(0, 1), prob = c(0.3, 0.7),
                  1, replace = T)
  
  if(alive == 1){
    nested <- sample(c(0, 1), prob = c(0.5, 0.5),
                     1, replace = T)
    }
  
  }
  
  testy <- cbind(alive, nested)
  
  test[[i]] <- testy
  
}

test_output <- do.call("rbind", test)



for (i in 1:100) {
  nested <- 0
  alive <- 1
  s <- 0
  while (alive == 1) {
    
    alive <- sample(c(0, 1), prob = c(0.5, 0.5),
                     1, replace = T)
    
    while (nested == 0) {

      nested <- sample(c(0, 1), prob = c(0.4, 0.6),
                      1, replace = T)
      
    }
    
    s <- s+1
    
  }
  testy <- cbind(i, alive, nested, s)
  
  test[[i]] <- testy
  
}

test_output <- do.call("rbind", test)


#----
test <- list()

for (w in 1:10) {
  alive <- 1
  nested <- 0
  
  while(alive == 1 & nested == 0) {
  
    print(ifelse(alive == 1, "alive", "dead"))
    print(ifelse(nested == 1, "nested", "not"))
    
    alive <- sample(c(0, 1), 1)
    nested <- sample(c(0, 1), 1)
    
  }
  
  
}



for (w in 1:10) {
  alive <- 1
  nested <- 0

  while (alive == 1 & nested == 0) {
    
    alive <- sample(c(0, 1), prob = c(0.1, 0.9), 1)
    if (alive == 1) {
      
      a <- sample(1:10, 1)
      if (a > 5){
        nested <- 1
      }
      
    }
    
    print(rbind(w, alive,a, nested))
    
  }
  
}




#---------------------- test ------------------------
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
source("wasp_R/build_Landscape_v5.R")
source("wasp_R/waspFunctions.R")


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
a <- fieldArena
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T), ylim = c(0, 100), xlim = c(0, 100))
plot(which(a == 0, arr.ind = T), ylim = c(0, 100), xlim = c(0, 100))
fieldArena <- a

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)


for (g in seq(50, 100, 10)) {
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
      if (alive == 1) {
        
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
        
      }
    
      # Store results
      
      # remove t
      gall <- cbind(w, s, startX, startY, alive, nested,
                    usurp, currentX, currentY)
      
      print(cbind(s,w, alive, nested))
      s <- s + 1
      
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
}


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

gall50 <- gallicusOutList[[1]]

# possible solutions to usurping itself 
#  add another check variable after nesting in empty cell
#  or use nested if statement
#  or use break after each nested == 1


#- nested if ----
for (g in seq(50, 100, 10)) {
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
      if (alive == 1) {
        
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
          
        } else {
          
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
          
        }
        
        
        # if non-viable cell
        if(fieldArena[currentX, currentY] == -1) {
          nested <- 0
          usurp <- 0
        }
        
      }
      
      # Store results
      
      # remove t
      gall <- cbind(w, s, startX, startY, alive, nested,
                    usurp, currentX, currentY)
      
      print(cbind(s,w, alive, nested))
      s <- s + 1
      
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
}


wasp50_3 <- as.data.frame(simPath2[[1]][3])

lines(wasp50_3[, "X"], wasp50_3[, "Y"], col = "blue")

points(gallOutput[, "currentX"], gallOutput[, "currentY"], col = "red")

points(gallPath[[99]][, 2], gallPath[[99]][, 3], col = "blue")


## current problem - wasp not stopping at first empty cell, and usurping itself






#--- Working -----

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
source("wasp_R/build_Landscape_v5.R")
source("wasp_R/waspFunctions.R")

a <- createHabitat(100, 100, 10, 0.4)
a <- fieldArena
habitatV_NV <- table(fieldArena)
patchCoords <- which(fieldArena == 0, arr.ind = T)
plot(which(fieldArena == 0, arr.ind = T), ylim = c(0, 100), xlim = c(0, 100))
plot(which(a == 0, arr.ind = T), ylim = c(0, 100), xlim = c(0, 100))
fieldArena <- a

# Get field dimensions
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])

startX <- sample(c(1:fieldX), 1, replace = T)
startY <- sample(c(1:fieldY), 1, replace = T)




#---- working ---- 
# need to add fieldarena reset before each nest loop
for (g in seq(50, 100, 10)) {
  gallN <- g
  fieldArena <- a # need to reset fieldarena to start condition
  
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
    usurp <- 0
    
    while (alive == 1 & nested == 0) { # until the wasp nests
      
      # check if wasp is alive 
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      # If the wasp is alive, the loop will run
      if (alive == 1) {
        
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
          
        } else {
          
          # if the cell is occupied
          if (fieldArena[currentX, currentY] == 1) {
            # check if usurps
            usurp <- sample(c(0, 1), prob = c((1 - pgUsurp), pgUsurp),
                            1, replace = T)
            #usurp <- sample(c(0, 1), prob = c(0.4, 0.6), 1, replace = T)
            
            # if usurps, then nest
            if (usurp == 1) {
              nested <- 1
              fieldArena[currentX, currentY] <- 1
            }
            
          }
          
        }
        
        
        # if non-viable cell
        if(fieldArena[currentX, currentY] == -1) {
          nested <- 0
          usurp <- 0
        }
        
      }
      
      # Store results
      
      # remove t
      gall <- cbind(w, s, startX, startY, alive, nested,
                    usurp, currentX, currentY)
      
      print(cbind(s,w, alive, nested))
      s <- s + 1
      
      
    } # end of step loop
    
    gallicus[[w]] <- gall
    
    
  }
  
  simPath[[g]] <- gallPath
  totalGallicus[[g]] <- gallicus
  
}

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

gall50 <- gallicusOutList[[1]]
gall100 <- gallicusOutList[[6]]

wasp50_40 <- as.data.frame(simPath2[[1]][40])
wasp100_88 <- as.data.frame(simPath2[[6]][88])

lines(wasp50_40[, "X"], wasp50_40[, "Y"], col = "blue")
lines(wasp100_88[, "X"], wasp100_88[, "Y"], col = "green")

points(gallOutput[, "currentX"], gallOutput[, "currentY"], col = "red")

points(gallPath[[99]][, 2], gallPath[[99]][, 3], col = "blue")


wasp100_1 <- as.data.frame(simPath2[[6]][1])
lines(wasp100_1[, "X"], wasp100_1[, "Y"], col = "red")
