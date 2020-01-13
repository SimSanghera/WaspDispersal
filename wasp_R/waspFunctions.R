#----- Functions required for the wasp dispersal model  -----



# nextStep: select a new direction and cell
#   Within this function, will select new X & Y coordinates by
#   sampling from -1:1 in both planes, then adding the resulting 
#   values to the current cell.
#   Checks for illegal movements to match the grid design - as 
#   described above.
#   Also checks to see if new cell coordinates are still within the
#   boundary limits of the grid.
#   Returns the new cell as a list containing newX & newY

nextStep <- function() {
  # select direction
  dx <- sample(c(-1, 0, 1), 1, replace = T)
  dy <- sample(c(-1, 0, 1), 1, replace = T)
  
  # check for illegal movement
  while(dx == -1 & dy == -1 ||
        dx ==  1 & dy == -1 ||
        dx ==  0 & dy ==  0) {
    dx <- sample(c(-1, 0, 1), 1, replace = T)
    dy <- sample(c(-1, 0, 1), 1, replace = T)
  }
  
  # add dx & dy to current X & Y to get new cell
  newX <- currentX + dx
  newY <- currentY + dy
  
  # check new cells within arena
  while (newX <= 1 | newX > fieldX |
         newY <= 1 | newY > fieldY) {
    dx <- sample(c(-1, 0, 1), 1, replace = T)
    dy <- sample(c(-1, 0, 1), 1, replace = T)
    
    newX <- currentX + dx
    newY <- currentY + dy
    
  }
  
  # return as list
  return(list(newX, newY))
  
}

