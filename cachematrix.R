## Sherif G. Jan. 7, 2017. 
## makeCacheMatrix builds an object containing 4 functions within it. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #set values of x and m
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #just return the matrix. 
        get <- function(){
                x
        }
        setInverse <- function(inverse){
                i <<- inverse
        }
        getInverse <- function(){
                i
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Computes the inverse of a function, or retrives it if it has already been calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(ncol(x$get()) != nrow(x$get())) stop("Not an invertible matrix")
        i <- x$getInverse()
        #check to see if matrix inverse has already been computed. 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}


