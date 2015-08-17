###################################################################################################
# Creates a special "matrix", which is really a list containing a function to 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse of the matrix
# 4.get the value of the inverse of the matrix
###################################################################################################
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   
   set <- function(y) 
   {
      x <<- y
      inv <<- NULL
   }
   
   get <- function() { x }
   
   setInverse <- function(inverse) { inv <<- inverse }
   getInverse <- function() { inv }
   
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

###################################################################################################
# Returns a matrix that is the inverse of 'x'
# Calculates the inverse of the special "matrix" created by makeCacheMatrix function.
# It first checks to see if the inverse of the matrix has already been calculated. If so, it gets the cached value and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
# 
# WARNING: x matrix is supposed to be always invertible (thus also a square matrix)
# No test is done to see if it is really invertible. If it is not invertible, R function solve will raise an error, either because the matrix is non-square or because it is square but still noninvertible.
###################################################################################################
cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
   
   if( !is.null(inv) )
      message("getting cached data")
   else 
   {         
      m <- x$get()
      
      #WARNING: if m is noninvertible, R function solve will raise an error
      identity_matrix <- diag( nrow(m) )
      inv <- solve(m, identity_matrix, ...)
      x$setInverse(inv)
   }

   inv
}

###################################################################################################
# Some tests on an invertible matrix
#> m1 <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
#> sm1 <- makeCacheMatrix(m1)
#> sm1$get()
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2
#> sm1$getInverse()
#NULL
#> cacheSolve(sm1)
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#> sm1$getInverse()
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#> cacheSolve(sm1)
#getting cached data
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#> sm1$getInverse()
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#> sm1$set(m1)
#> sm1$getInverse()
#NULL

###################################################################################################
# Some tests on a non-square matrix (also noninvertible), and on a square-noninvertible matrix
#> m2 <- matrix(1:6, nrow = 2, ncol = 3)
#> sm2 <- makeCacheMatrix(m2)
#> sm2$get()
#     [,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6
#> cacheSolve(sm2)
#Error in solve.default(m, identity_matrix, ...) : 
#  'a' (2 x 3) must be square
#
#> m3 <- matrix(c(2, 1, 6, 3), nrow = 2, ncol = 2)
#> sm3 <- makeCacheMatrix(m3)
#> sm3$get()
#     [,1] [,2]
#[1,]    2    6
#[2,]    1    3
#> cacheSolve(sm3)
#Error in solve.default(m, identity_matrix, ...) : 
#  Lapack routine dgesv: system is exactly singular: U[2,2] = 0


