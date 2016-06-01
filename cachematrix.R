## DSchreck 06/01/2016
## functions below cache the inverse of a matrix rather than compute it repeatedly

library(MASS) #use MASS library for ginv() which allows inverse of non-square matrices

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      M <- NULL  #M will be populated with inverse matrix
      #y <- NULL
      set <- function(y) { #set()
            x <<- y
            M <<- NULL
      }
      get <- function() { x } #get input matrix
      setMinv <- function(inv) { M <<- inv } #set M to inverse
      getMinv <- function() { M } #get inverse matrix
      list(get=get, set=set, setMinv=setMinv, getMinv=getMinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      M <- x$getMinv()
      #if inverse is already populated get from cache
      if(!is.null(M)) { #check if cacheSolve has already been run
            #if(identical(ginv(x$set(), ...),x$get()) ) { # check if matrix has changed
                  message("getting cached data")
                  return(M)
            #}
      }
      data <- x$get() #get the input/original matrix
      x$set(data) # run the set function on the input matrix to cache it
      M <- ginv(data, ...) #get the inverse of input matrix using ginv() instead of solve() for non-square matrices
      x$setMinv(M) # run the setMInv to cache the inverse of the input matrix
      M #return the inverse of input matrix
}

#sample input:
#>mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
#>mat2 <- makeCacheMatrix(mat)
#>cacheSolve(mat2)
#should return:
#     [,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4