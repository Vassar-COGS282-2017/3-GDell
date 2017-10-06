# Author: Gabriel Dell'Accio

# This model alters the original schelling model of segregation by 
# adding a property cost and wealth parameter to the function that determines
# whether an individual is unhappy or not. In this implementation, blue individuals
# are given more money while red indivuals are given less. Property cost is greater the
# closer you are to the center of the grid. Individuals take a similarity cost when 
# are in placed in a position with a higher property cost than their wealth value.

# model parameters ####
rows <- 50 
cols <- 50
proportion.group.1 <- .5 # proportion of red agents
empty <- .6# proportion of grid that will be empty space
min.similarity <- 4/8 # minimum proportion of neighbors that are the same type to not move
center.grid <- c(25,25)

# create.grid ####
# generates a rows x column matrix and randomly places the initial population
# values in the matrix are either 0, 1, or 2
# if 0, the space is empty
# 1 and 2 represent the two different groups
create.grid <- function(rows, cols, proportion.group.1, empty){
  pop.size.group.1 <- (rows*cols)*(1-empty)*proportion.group.1
  pop.size.group.2 <- (rows*cols)*(1-empty)*(1-proportion.group.1)
  
  initial.population <- sample(c(
    rep(1, pop.size.group.1), 
    rep(2, pop.size.group.2), 
    rep(0, (rows*cols)-pop.size.group.1-pop.size.group.2)
  ))
  grid <- matrix(initial.population, nrow=rows, ncol=cols)
}

# visualize.grid ####
# outputs a visualization of the grid, with red squares representing group 1,
# blue squares group 2, and black squares empty locations.
visualize.grid <- function(grid){
  # Produces visualization of a matrix.
  image(grid, col=c('black','red','blue'), xaxs=NULL, yaxs=NULL, xaxt='n', yaxt='n')
}

# empty.locations ####
# returns all the locations in the grid that are empty
# output is an N x 2 array, with N equal to the number of empty locations
# the 2 columns contain the row and column of the empty location.
empty.locations <- function(grid){
  return(which(grid==0, arr.ind=T))
}

# similarity.to.center ####
# takes a grid and the center.val of that grid and returns
# the proportion of cells that are the same as the center,
# ignoring empty cells. the center.val must be specified
# manually in case the grid has an even number of rows or 
# columns
# Used to calculate neighborhood similarity. What proportion of neighbors match the center value.
similarity.to.center <- function(grid.subset, center.val){
  if(center.val == 0){ return(NA) }
  same <- sum(grid.subset==center.val) - 1
  not.same <- sum(grid.subset!=center.val) - sum(grid.subset==0)
  return(same/(same+not.same))
}

# segregation ####
# computes the proportion of neighbors who are from the same group
segregation <- function(grid){
  same.count <- 0
  diff.count <- 0
  for(row in 1:(nrow(grid)-1)){
    for(col in 1:(ncol(grid)-1)){
      if(grid[row,col] != 0 && grid[row+1,col+1] != 0){
        if(grid[row,col] != grid[row+1,col+1]){
          diff.count <- diff.count + 1
        } else {
          same.count <- same.count + 1
        }
      }
    }
  }
  return(same.count / (same.count + diff.count))
}

match.means <- function(xval, yval) {
  if(wealth.grid[xval,yval] < property.grid[xval,yval]) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# unhappy.agents ####
# takes a grid and a minimum similarity threshold and computes
# a list of all of the agents that are unhappy with their 
# current location. the output is N x 2, with N equal to the
# number of unhappy agents and the columns representing the 
# location (row, col) of the unhappy agent in the grid
unhappy.agents <- function(grid, min.similarity){
  grid.copy <- grid
  for(row in 1:rows){
    for(col in 1:cols){
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      
      if(match.means(row,col) == FALSE) {
        similarity.score <- similarity.score -.3
      }
      
      if(is.na(similarity.score)){
        grid.copy[row,col] <- NA
      } else {
        grid.copy[row,col] <- similarity.score >= min.similarity
      }
    }
  }
  return(which(grid.copy==FALSE, arr.ind = T))
}


property.distribution <- function(x) {
  return(x^2)
}

# assign.property.cost


assign.property.cost <- function(center, currentIndex, dist) {
  rowDiff <- abs(center[1] - currentIndex[1])
  columnDiff <- abs(center[2] - currentIndex[2])
  totalDifference <- rowDiff + columnDiff
  print(paste("THE TOTLA DIFF IS :",totalDifference))
  scaledDifference <- 1 - (dist(totalDifference)/dist(cols))
  return(scaledDifference)
}

create.property.matrix <- function(rowsVec, colVec) {
  center <- c(rowsVec/2, colVec/2)
  
  property.grid <<- matrix(nrow=rowsVec, ncol=colVec)
  temp <<- c()
  
  for(i in 1:rowsVec) {
    for(j in 1:colVec){
      currentIndex <- c(i,j)
      print(currentIndex)

      property.grid[i,j] <<- assign.property.cost(center, currentIndex, property.distribution)
    }
  }
  
}

wealthValue <- 0

assign.wealth <- function(theType, distOne, distTwo) {
  
  if(theType == 0) {
    return(0)
   
  }
  if(theType == 1) {
    return(distOne)
  
  }
  if(theType == 2) {
    return(distTwo)
  }
  
}

# rnorm

create.wealth.matrix <- function(rows, col) {
  center <- c(rows/2, col/2)
  
  wealth.grid <<- grid
  
  for(i in 1:rows) {
    for(j in 1:col){
      currentIndex <- c(i,j)
      
      wealth.grid[i,j] <<- assign.wealth(wealth.grid[i,j], runif(1,0,.5), runif(1,.5,1))
    }
  }
}



# one.round ####
# runs a single round of the simulation. the round starts by finding
# all of the unhappy agents and empty spaces. then unhappy agents are randomly
# assigned to a new empty location. a new grid is generated to reflect all of
# the moves that took place.
one.round <- function(grid, min.similarity){
  empty.spaces <- empty.locations(grid)
  unhappy <- unhappy.agents(grid, min.similarity)
  empty.spaces <- empty.spaces[sample(1:nrow(empty.spaces)), ]
  for(i in 1:nrow(empty.spaces)) {
    
    if(i>nrow(unhappy)){ 
      break;
    }
    # Move unhappy to empty spaces.
    grid[empty.spaces[i,1], empty.spaces[i,2]] <- grid[unhappy[i,1],unhappy[i,2]]
    wealth.grid[empty.spaces[i,1], empty.spaces[i,2]] <<- wealth.grid[unhappy[i,1],unhappy[i,2]]
    # A blank space.
    grid[unhappy[i,1], unhappy[i,2]] <- 0
    wealth.grid[unhappy[i,1], unhappy[i,2]] <<- 0
  }
  return(grid)
}

# running the simulation ####
done <- FALSE # a variable to keep track of whether the simulation is complete
grid <- create.grid(rows, cols, proportion.group.1, empty)
create.property.matrix(rows, cols)
create.wealth.matrix(rows, cols)
seg.tracker <- c(segregation(grid)) # keeping a running tally of the segregation scores for each round
while(!done){
  new.grid <- one.round(grid, min.similarity) # run one round of the simulation, and store output in new.grid
  seg.tracker <- c(seg.tracker, segregation(grid)) # calculate segregation score and add to running list
  if(all(new.grid == grid)){ # check if the new.grid is identical to the last grid
    done <- TRUE # if it is, simulation is over -- no agents want to move
  } else {
    grid <- new.grid # otherwise, replace grid with new.grid, and loop again.
  }
}
layout(1) # change graphics device to have two plots
visualize.grid(grid) # show resulting grid
plot(seg.tracker) # plot segregation over time

unhappy.agents(grid, min.similarity)


