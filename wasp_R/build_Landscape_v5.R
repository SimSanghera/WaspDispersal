#-----  buildLandscape <- function()  -----

# Creating an arena

# This script will hopefully enable us to create an ecological arena
#   with patches of viable nesting areas - to replicate the favoured
#   cactii utilised by wasps in the wild.
# This is similar to many species nesting preferences, where they are
#   selective in choosing areas to nest within a habitat.

# Habitat itself: should have adjustable parameters to alter total size
#
# Patches:
#   - can be placed randomly throughout the habitat
#   - can specify number of patches to include
#   - can vary size of patches
#   - patches are random distances from other patches

# Could this also be replicated if high-quality real world habitat
#   data was available? Probably. Cool



#----- Habitat -----

# Number of patches
# Size of patches - but these vary
# Selecting random patch location
# Viable nesting habitat denoted by 0
# Non-viable nesting habitat denoted by -1

# As a function:
#   - input parameters: x&y, number of patches, size of patches
#   - can maybe make sizes of patches random - picking numbers from
#       between 10 - 100
#   - make field arena of size x.y
#   - for each patch:
#     - select a random starting point
#     - select a random size - number of cells to belong to this patch
#       in both planes (x, y) - but not extending past boundaries
#     - fill these cells with 0


# Function Inputs:
#   - x = number of rows (height of matrix)
#   - y = number of columns (width of matrix)
#   - nPatches = number of patches desired in habitat
#   - size = percentage value - so size of patch is % of x or y
#             must be input as a decimal - 10% = 0.1, 20% = 0.2


#-----  NOTES FOR SIMULATION  -----
# For initial simulations, habitat was created under 2 conditions
#   to test validity of random patch size:
#   1. Condition where patches were a fixed size
#   2. Condition where patches were of random sizes
# 
# For this purpose, to meet condition 1, use function named 
#   "createHabitatFixed"

# Also changed in sampling start cell for patches. SHould not
#   be replaced as no 2 patches would start in the same place, 
#   otherwise these would be classed as the same patch


createHabitat <- function(x, y, nPatches, size) {
  
  # Make a matrix filled with -1's: currently all cells non-viable
  fieldArena <<- matrix(-1, nrow = x, ncol = y)
  
  # create patches
  for (patch in 1:nPatches) {
    
    # select random starting point
    patchX <<- sample(c(1:x), 1, replace = F)
    patchY <<- sample(c(1:y), 1, replace = F)
    
    # select size of patch - in this function, 5-10% of habitat area
    patchWidth  <<- sample(c(5:(y*size)), 1, replace = T) # cols
    patchHeight <<- sample(c(5:(x*size)), 1, replace = T) # rows
    
    # set range limits of the patch area
    if (((patchX + patchHeight) <= x) && ((patchY + patchWidth) <= y)) {
      fieldArena[patchX:(patchX + patchHeight),
                 patchY:(patchY + patchWidth)] <<- 0
    }
    
    else {
      
      # if patches extend beyond matrix limits
      if (((patchX + patchHeight) > x) && ((patchY + patchWidth) > y)) {
        patchWidth <<- (patchWidth - ((patchY + patchWidth) - y))
        patchHeight <<- (patchHeight - ((patchX + patchHeight) - x))
        fieldArena[patchX:(patchX + patchHeight),
                   patchY:(patchY + patchWidth)] <<- 0
      }
      
      else {
        
        # if patchWidth extends beyond number of columns
        if (((patchX + patchHeight) <= x) && ((patchY + patchWidth) > y)) {
          patchWidth <<- (patchWidth - ((patchY + patchWidth) - y))
          fieldArena[patchX:(patchX + patchHeight), 
                     patchY:(patchY + patchWidth)] <- 0
        }
        
        else {
          
          # if patchHeight extends beyond number of rows
          if (((patchX + patchHeight) > x) && ((patchY + patchWidth) <= y)) {
            patchHeight <<- (patchHeight - ((patchX + patchHeight) - x))
            fieldArena[patchX:(patchX + patchHeight),
                       patchY:(patchY + patchWidth)] <<- 0
          }
          
        }

      }
      
    }
    
  }
  
  return(fieldArena)
  
}


# Test
#createHabitat(100, 100, 10, 0.2)

# Get indices of all viable sites - where cell value = 0
#patchCoords <- which(fieldArena == 0, arr.ind = T)

# Get plot of patches - but remember will look different to matrix due 
#  to order of rows, cols
#plot(which(fieldArena == 0, arr.ind = T))



#----- Create Habitat with fixed patch sizes
# to implement this aspect of the function, simply remove the 
#   patchWidth/height <- sample(c(5:(x/y*size)), 1, replace = T)
# and replace with:
#   patchW/H <- x*size
# Out of restriction of the "size" argument, the patches are forced
#   into square shaped areas

createHabitatFixed <- function(x, y, nPatches, size) {
  
  # Make a matrix filled with -1's: currently all cells non-viable
  fieldArena <<- matrix(-1, nrow = x, ncol = y)
  
  # create patches
  for (patch in 1:nPatches) {
    
    # select random starting point
    patchX <<- sample(c(1:x), 1, replace = F)
    patchY <<- sample(c(1:y), 1, replace = F)
    
    # select size of patch - in this function, 5-10% of habitat area
    patchWidth  <<- y*size # cols
    patchHeight <<- x*size # rows
    
    # set range limits of the patch area
    if (((patchX + patchHeight) <= x) && ((patchY + patchWidth) <= y)) {
      fieldArena[patchX:(patchX + patchHeight),
                 patchY:(patchY + patchWidth)] <<- 0
    }
    
    else {
      
      # if patches extend beyond matrix limits
      if (((patchX + patchHeight) > x) && ((patchY + patchWidth) > y)) {
        patchWidth <<- (patchWidth - ((patchY + patchWidth) - y))
        patchHeight <<- (patchHeight - ((patchX + patchHeight) - x))
        fieldArena[patchX:(patchX + patchHeight),
                   patchY:(patchY + patchWidth)] <<- 0
      }
      
      else {
        
        # if patchWidth extends beyond number of columns
        if (((patchX + patchHeight) <= x) && ((patchY + patchWidth) > y)) {
          patchWidth <<- (patchWidth - ((patchY + patchWidth) - y))
          fieldArena[patchX:(patchX + patchHeight), 
                     patchY:(patchY + patchWidth)] <- 0
        }
        
        else {
          
          # if patchHeight extends beyond number of rows
          if (((patchX + patchHeight) > x) && ((patchY + patchWidth) <= y)) {
            patchHeight <<- (patchHeight - ((patchX + patchHeight) - x))
            fieldArena[patchX:(patchX + patchHeight),
                       patchY:(patchY + patchWidth)] <<- 0
          }
          
        }
        
      }
      
    }
    
  }
  
  return(fieldArena)
  
}


# Get indices of all viable sites - where cell value = 0
#patchCoords <- which(fieldArena == 0, arr.ind = T)

# Get plot of patches - but remember will look different to matrix due 
#  to order of rows, cols
#plot(which(fieldArena == 0, arr.ind = T))


#table(fieldArena)