
# This function creates a special "matrix" object that can cache its inverse.

# The first function, makeVector creates a matrix, which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #init empty holder
  inverse <- NULL
  
  set<- function(y){
    #set global x
    x<<-y
    #after setting new values, new inverse matrix must be recalculated
    inverse<<-NULL
  }
  
  #return matrix
  get <- function() x
  
  #set inverse of a square matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  
  #get inverse of a square matrix
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse=setInverse,
       getInverse=getInverse
       )
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #extract cached inverse matrix if it exists, otherwise it returns NULL
      inverse<- x$getInverse()
      if(!is.null(inverse)){
        message("Inverse matrix was received from cache")
        return(inverse)
      }
      message("Inverse matrix will be  recalculated")
      #extract matrix data
      data <- x$get()
      
      #create inverse of the matrix
      inverse <- solve(data)
      
      #save inverse matrix to cache
      x$setInverse(inverse)
      inverse
}
