## Put comments here that give an overall description of what your
## functions do
## By Fauzy Che Yayah

# This function will create list containing function for
# a) setter the matrix value
# b) getter the matrix value
# c) setter the inverse matrix value
# d) getter the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
# This function have the return value of the inverse matrix.
# 1st chech the if the inverse matrix is computed or not before , if yes then return the result and skip. If no then it will compute the inverse , setting the value into cache using the setinverse funtion

cacheSolve <- function(x, ...) {
		 inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Sample run
#--------------------------------------------
# > x = rbind(c(1, -1/2), c(-1/2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
     # [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0
# > cacheSolve(m)
          # [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
#