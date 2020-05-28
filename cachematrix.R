## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {         #Here is defined the x variable (empty)
  inverted <- NULL
  set <- function(y) {
    x <<- y                                         #This is assigned a value from some parental env to x variable
    inverted <<- NULL
  }
  get <- function() x                               #This function only retrieves the x value
  setinverse <- function(mean) inverted <<- mean    #This function assigns the mean value to the "inverted" variable somewhere in parent env
  getinverse <- function() inverted                 #This line retrieves the the inverted matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                    ## Return a matrix that is the inverse of 'x'
        
  inverted <- x$getinverse()                        
  if(!is.null(inverted)) {                          #Is a inverted matrix already exist (cached), the message and matrix will be retrieved
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)                      #Here the matrix is inverted
  x$setinverse(inverted)                            #Then, stored.
  inverted
  
}

# IGNORE, just for test purpose

#G <- rbind(c(1, -1/4), c(-1/4, 1))
#as.matrix(G)
#class(G)
#aMatrix <-makeCacheMatrix(G)
#aMatrix$get()                               # retrieve the value of x
#aMatrix$getinverse()                        # retrieve the value of m, which should be NULL
#aMatrix$set(rbind(c(2, -1/4), c(-1/4, 2)))  # reset value with a new vector
#cacheSolve(aMatrix)                         # notice that f() shows solved matrix of the previous line, not for G
#aMatrix$getinverse()                        # retrieve it directly
