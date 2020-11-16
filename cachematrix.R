##makeCacheMatrix creates a special matrix object which can hold its inverse.
##when the object is created first time / initialized, the inverse remains NULL.
##
## this can be tested with simple test script like following ( assuming my current 
##directory has the  cachematrix.R file with function defination saved in it)
##
## source("./cachematrix.R")
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$getInverse()
## 
## above code will print NULL as R runtime has not executed the inverse operation yet

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) x_inv <<- inverse
  getInverse <- function() x_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## function cacheSolve computes the inverse of the "matrix" created by 
## makeCacheMatrix defined above. 
## it checks if inverse has already been calculated or not
## if not calculated earlier then inverse is calculated and saved in x_inv to avoid future inversion.
## aqding 2 lines of test code after the 3 lines used to test can test out the fucntion as below
##
##
## source("./cachematrix.R")
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$getInverse()
### this line forces the inversion
## cacheSolve(my_matrix) 
### following line can confirm teh inverse is persisting in cache
## my_matrix$getInverse() 

cacheSolve <- function(x, ...) {
  
  x_inv <- x$getInverse()
  if (!is.null(x_inv)) {
    return(x_inv)
  }
  mat <- x$get()
  x_inv <- solve(mat, ...)
  x$setInverse(x_inv)
  x_inv
}
