##  functions that cache the inverse of a matrix
##  Assumption: Matrix supplied is always invertible

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(a = matrix()) {
        #a: matrix ; i:inverse of a
        i <- NULL
        set <- function(y) {
                if (nrow(y)==ncol(y)){
                        a <<- y
                        i <<- NULL                        
                }
                else {
                        print("input object is not a square marix")
                        a<<-NULL
                        i<<NULL
                }
        }
        get <- function() a
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(a, ...) {
        i <- a$getinverse()
        if(!is.null(i)) {
                print("Message: getting cached data")
                return(i)
        }
        data <- a$get()
        i <- solve(data, ...)
        a$setinverse(i)
        i
}
