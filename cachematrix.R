## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function used to create a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    ## This is the stored inversion location
  set <- function(y){
    x <<- y
    inv <<- NULL    ## Initialization to be NULL
  }
  get <- function() x   ## return the input matrix
  setInverse <- function(inverse) inv <- inverse    ## Set the inversed matrix
  getInverse <- function() inv      ## Return the inversed matrix
  list(set =set, get = get,         ## This is to return a list that contrains all these functions
       setInverse = setInverse,
       getInverse = getInverse)	 
}


## Write a short comment describing this function
## This is a function used to compute the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheslove should retrieve the inverser 
## form the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()    	      ## Get the inversed matrix from object x
  if (!is.null(inv)){      	      ## If the inversed matrix exist
    message("getting cached data")
    return(inv)	     		      ## Return the calculated inversion
  }        
  data <- x$get()        		## if not, get the matrix object
  inv <- solve(data,...)		## We solve it
  x$setInverse(inv)	   		## Set it to the object
  inv			     		      ## Return the solved result
}

## Get tested

test <- matrix(rnorm(9),3,3)  ## Generate a random square invertible matrix
testcached <- makeCacheMatrix(test)   ## Generate a makeCacheMatrix object using the makeCacheMatrix
testInv <- cacheSolve(testcached)     ## Calculate the retrieve inversion 
test						  ## Display the input matrix

           [,1]       [,2]       [,3]
[1,] -0.2674050 -0.7338176 -0.2746517
[2,] -0.8874628 -0.7371650 -0.9602245
[3,]  0.4498568 -0.4770389  0.6658262

testInv					  ## Display the output matrix
           [,1]       [,2]       [,3]
[1,]  13.508355 -8.8208207 -7.1488266
[2,]  -2.262559  0.7757376  0.1854337
[3,] -10.747781  6.5154590  6.4647614