## Below are 2 functions that cache the inverse of a matrix after computing it to reduce the computation effort
## Assumption - the matrix supplied is always invertible

## makeCacheMatrix creates an R object, makeCacheMatrix, that contains four functions: 
## set(), get(), setinverse(), and getinverse(). This is stored in a list with each element named.
## It also includes the two data objects, x and inv. x and inv are both matrices

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         #Setting the values of x & inv in the parent environment. 
                inv <<- NULL    #If setting a new value for x, need to ensure that the value of inv is reset too
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse #setting the value of inv in the parent environment
        getinv <- function() inv
        list(set = set, get = get, #returning a list of functions
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the R object created using makeCacheMatrix
## It first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix, sets the value of the inverse in the cache
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) { #checking if the inv has already been calculated
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #getting the matrix from the parent environment
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


## Code to test the functions
## B = matrix(c(2, 4, 3, 1, 5, 7, 8, 9, 2), nrow=3, ncol=3) 
## x1 <- makeCacheMatrix(B)
## invB <- cacheSolve(x1)
## B%*%invB

