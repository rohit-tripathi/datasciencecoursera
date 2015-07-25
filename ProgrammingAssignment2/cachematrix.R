## The function makeCacheMatrix makes a special "matrix" object that is actually
## a list containing 4 functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse)  inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes the special "matrix" object x and the matrix y whose inverse
## needs to be calculated. It compares if both are same (i.e. whether or not 
## the matrix has changed) using the get() funtion of y. If not, it checks if the
## inverse has already been calculated by calling getinv() funtion of the special
## matrix object. If yes, it returns the inverse. Otherwise, it calculates the
## inverse using the other functions of the special "matrix" and returns it

cacheSolve <- function(x, y = NULL, ...) {
        data <- x$get()
        if ((is.null(y)) || identical(y,data)) {
                message("Matrix has not changed")
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("Inverse already cached. Getting cached inverse")
                        return(inv)
                }
                inv <- solve(data)
                
                
        }
        else {
                message("Matrix has changed. Getting inverse. Caching matrix and the inverse")
                inv <- solve(y)
                x$set(y)
                
        }
        
        x$setinv(inv)        
        inv
        
}
