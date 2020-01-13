#-----  Gallicus & Dominula -----

# Add a line that converts all nested dominula to value -2, then run gallicus


# Protocols:
# 1. run dominula nests first - minimum 10
# 2. convert nested single dominula (cell value = 1) to -2 value
# 3. run gallicus nests
# 4. first, run ratio 1:1
# 5. second, run ratios in favour of dominula
# 6. third, once gallicus complete, convert -2 back to values 1, run more dominula.

# Experiments: 10 of each
#   1. 10-10, 15-15, 20-20, 30-30, 40-40, 50-50
#   2. More dominula than gallicus
#     a. 10 dominula, 10 gallicus
#     b. 20 dominula, 10 gallicus
#     c. 30 dominula, 10 gallicus
#     d. 40 dominula, 10 gallicus
#     e. 50 dominula, 10 gallicus
#   3. opposite - more gallicus than dominula
#   4. dominula, gallicus, dominula
#     a. 10, 10, 10; b. 20, 20, 20; c. 30, 30, 30; d. 40, 40, 40
#     e. 50, 50, 50
#     f. 20, 10, 10; g. 20, 10, 20
#     h. 30, 10, 10; i. 30, 10, 20; j. 30, 10, 30


#-----  Setup -----
library(tictoc)

# import scripts containing necessary functions for the model
#   - createHabitat
#   - nextStep
source("wasp_R/build_Landscape_v5.R")
source("wasp_R/waspFunctions.R")


# 1. Create a list to store all wasp information from the model
dominula <- list()
totalDominula <- list()
gallicus <- list()
totalGallicus <- list()

# 2. Create a list to store all individual path histories
domPath <- list()
simPath_D <- list()
gallPath <- list()
simPath_G <- list()

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


# Probability of mortality
pdMort <- 0.002
pgMort <- 0.002

# Prob of usurping
pdUsurp <- 0.05
pgUsurp <- 0.05

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

# Save & Load habitats
write.table(fieldArena,
            "wasp_data_output/mixed_spp/mixSpp_arena_10.txt",
            row.names = FALSE,
            sep = "\t")

write.table(patchCoords, 
            "wasp_data_output/mixed_spp/patchCoords_mixSpp_10.txt",
            row.names = FALSE)

# load field arena
fieldArena <- 
  as.matrix(read.delim2("wasp_data_output/mixed_spp/mixSpp_arena_2.txt"),
            sep = "\t")



a <- fieldArena   # cheap hack to store fieldArena for repeated use


# Get field dimension
fieldX <- length(fieldArena[1, ])
fieldY <- length(fieldArena[, 1])



#-----  Simulation  -----

# dominula
for (n in 1:10) {
  
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
  
  simPath_D[[n]]        <- domPath
  totalDominula[[n]]  <- dominula
  
}


# CONVERT ALL DOM SINGLE NESTS TO -2 VALUE
fieldArena[which(fieldArena == 1, arr.ind = T)] <- -2
table(fieldArena)

# gallicus
for (n in 1:50) { # for each nest - just change number of nests
  
  gallN <- 100 # number of wasps
  
  startX <- sample(c(1:fieldX), 1, replace = T)
  startY <- sample(c(1:fieldY), 1, replace = T)
  
  for (w in 1:gallN) { # for each wasp
    
    # initiate a path history for each wasp
    pathHist <- data.frame(nest = 0, wasp = 0, step = 0,
                           X = startX, Y = startY)
    gallPath[[w]] <- data.frame(pathHist)
    
    # set current X & Y cells
    currentX <- startX
    currentY <- startY
    
    # set starting conditions
    alive       <- 1
    nested      <- 0
    usurp       <- 0
    s           <- 1
    
    # start search & move loop
    while (alive == 1 & nested == 0) {
      
      # check if wasp is alive
      alive <- sample(c(0, 1), prob = c(pgMort, (1 - pgMort)),
                      1, replace = T)
      
      if (alive == 1) {
        
        # movement
        newCells <- nextStep()
        
        # add new cells
        newPath <- c(n, w, s, newCells[[1]], newCells[[2]])
        gallPath[[w]][s+1, ] <- newPath
        
        # move wasp
        currentX <- newCells[[1]]
        currentY <- newCells[[2]]
        
        # check occupancy status
        # 1. cell is empty
        if (fieldArena[currentX, currentY] == 0) {
          
          # wasp nests
          nested <- 1
          fieldArena[currentX, currentY] <- 1
          
        } else {
          
          # 2. cell has a wasp in it
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
          
        } # end of cell occupied
        
      } # end if alive
      
      # store results
      gall <- cbind(n, w, s, startX, startY,
                    alive, nested, usurp,
                    currentX, currentY)
      
      # move to next step
      s <- s + 1
      
    } # end while
    
    gallicus[[w]] <- gall
    
  }
  
  simPath_G[[n]]      <- gallPath
  totalGallicus[[n]]  <- gallicus
  
}



# Do we want to convert back to dominula value 1 - or will this hide
#  where the dominula and gallicus nests are separate? 
# No, keep as -2. Then when get frequency of nest numbers, we will 
#  know how many are for each spp.
# CONVERT ALL DOM SINGLE NESTS BACK TO 1 VALUE
#fieldArena[which(fieldArena == -2, arr.ind = T)] <- 1


#-----  Convert output  -----

nestsDom <- list()
for (i in 1:length(totalDominula)) {
  nestsDom[[i]] <- as.data.frame(do.call("rbind", totalDominula[[i]]))
}


simPath_D2 <- simPath_D[!sapply(simPath_D, is.null)]
simPathOutList_D <- list()
for (i in 1:length(simPath_D2)) {
  simPathOutList_D[[i]] <- as.data.frame(do.call("rbind", simPath_D2[[i]]))
}


nestsGal <- list()
for (i in 1:length(totalGallicus)) {
  nestsGal[[i]] <- as.data.frame(do.call("rbind", totalGallicus[[i]]))
}

simPath_G2 <- simPath_G[!sapply(simPath_G, is.null)]
simPathOutList_G <- list()
for (i in 1:length(simPath_G2)) {
  simPathOutList_G[[i]] <- as.data.frame(do.call("rbind", simPath_G2[[i]]))
}


#-----  Write Output  -----
# Just have to change the number in the file name

for (i in 1:length(nestsDom)) {
  write.table(nestsDom[[i]],
              paste0("wasp_data_output/mixed_spp/dominula_",
                     "mix_",
                     "d10",
                     "g50",
                     "_",
                     "w",
                     nrow(nestsDom[[i]]),
                     "_nest",
                     i,
                     "_2",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList_D)) {
  write.table(simPathOutList_D[[i]],
              paste0("wasp_data_output/mixed_spp/dominula_",
                     "mix_path",
                     "_d10",
                     "g50",
                     "_w",
                     nrow(nestsDom[[i]]),
                     "_nest",
                     i,
                     "_2",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}



for (i in 1:length(nestsGal)) {
  write.table(nestsGal[[i]],
              paste0("wasp_data_output/mixed_spp/gallicus_",
                     "mix_",
                     "d10",
                     "g50",
                     "_",
                     "w",
                     nrow(nestsGal[[i]]),
                     "_nest",
                     i,
                     "_2",
                     ".txt"),
              sep = "\t",
              row.names = FALSE)
}


for (i in 1:length(simPathOutList_G)) {
  write.table(simPathOutList_G[[i]],
              paste0("wasp_data_output/mixed_spp/gallicus_",
                     "mix_path",
                     "_d10",
                     "g50",
                     "_w",
                     nrow(nestsGal[[i]]),
                     "_nest",
                     i,
                     "_2",
                     ".txt"),
              row.names = FALSE,
              sep = "\t")
}



rm(list = setdiff(ls(), lsf.str()))


