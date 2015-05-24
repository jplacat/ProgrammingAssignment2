##Programming Assignment 2 - Jordi Pla

## Together, the two functions below can be used to calculate the Inverse of a Matrix. This is however done
## in a more efficient way as the result will be cached, and therefore the solve() function - used to inverse
## a function - is only applied if the input matrix changes. This can make calculation potentially faster,
## specially when using bigger matrices.


## Function makeCacheMatrix(x) takes a square matrix as input "x" (invertible matrices are always square!). This first
## function creates a list of 4 functions, which are set(y), get(), setInv(Inv) and getInv(). The output of this
## function will be the input of the cacheSolve(x) function, to come up with the inverse of matrix "x".

makeCacheMatrix <- function(x = matrix()) {
      InvM <- NULL # InvM is used to store the resulting inversed matrix. It is initialised at the beginning.
      set <- function(y) {
            x <<- y
            InvM <<- NULL
      }
      get <- function() x                       # To return the input matrix
      setInv <- function(Inv) InvM <<- Inv      # To set the variable InV to the inversed matrix
      getInv <- function() InvM                 # To return the inversed matrix
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)                     # To return the list made up by the 4 functions defined above
}


## This second function takes the list created by the function before, and computes the Inversed matrix. However it first
## checks if the inversed has already been computed, and if so it simply returns the result, without having to calculate 
## again, and warns the user with a message "getting cached data". Otherwise, if the matrix has changed, it calculates the
## inverse of this matrix.


cacheSolve <- function(x, ...) {
      m <- x$getInv()         # Sets "m" to the chached inversed matrix. If it is the first time, or the input matrix has 
                              # changed, this will be NULL, otherwise it stores the result already calculated.
      
      if(!is.null(m)) {       # If "m" is NOT null, it means that the result is already cached and the matrix has not changed,
                              # therefore the result is printed without having to recalculate, and the user is warned with a 
                              # message.
            
            message("getting cached data")
            return(m)
      }
      data <- x$get()   # Otherwise, when the inverse has not been calculated yet, we store the input matrix in "data"
      m <- solve(data)  # Then we apply the default function solve() to calculate the inverse of the matrix and store in m
      x$setInv(m)       # We cache the result using the setInv function, to be used in the next time.
      m                 # Finally we print the result, that´s the inversed matrix.
}
