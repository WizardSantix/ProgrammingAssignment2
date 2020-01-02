## Cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.
##  The set, get, setinverse, getinverse functions will be put in a list which,
##  together with the data of the matrix (x) and its inverse (i), are the special object.
##  See https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

makeCacheMatrix <- function(x = matrix()) { ## initialize x
## as makeCacheMatrix returns functions, x will be stored by the object because it is part of the
## makeCacheMatrix environment
  
  i <- NULL ## initialize the value of i (the inverse of x, starting as NULL to be used later)
  set <- function(y){ ## the behavior of the first setter object is defined,
                      ## this one retrieves the value of the input y 
    x<<-y             ## and assigns it to x in the parent environment using <<-.
    i<<-NULL          ## NULL is assigned to i because should x be changed, we wouldn't want
                      ## i to have the inverse of a previous x value stored in it
  }
  
  get <- function () {x} ## Getter for x (taking advantage of lexical scoping since x is in 
                         ## the parent environment)
  setinverse <- function(inverse){ i <<- inverse } ## Assigns the input value of inverse to i 
                                                   ## in the parent environment using <<-
    getinverse <- function () {i} ## Gets the value of i from the parent environment taking 
                                  ## advantage of the lexical scoping
  
  list(set= set, get=get, setinverse = setinverse, getinverse = getinverse) ## Names the elements of the list object
}
  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse() ## Calls the value of i from the x object
  if(!is.null(i)){    ## Checks if there is a value assigned to i
    message("getting cached data")
    return(i)         ## Returns the value of i (inverse matrix of x) if it already exists
  }
  data <- x$get()     ## Retrieves the value of the x matrix from the x object
  i <- solve(data, ...) ## Calculates the value of i (the inverse of the x matrix)
  x$setinverse(i)       ## Assignes the value of i in this environment to i in the x object
  i                     ## Returns the value of i
  }
