## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## This function creates a special "matrix" object that caches 
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## This is a test of the two functions created above, from a matrix created
## as an example.
mat1<-c(1,2,3)
mat2<-c(6,5,6)
mat3<-c(7,8,9)
matr<-cbind(mat1, mat2, mat3)
matr

specialmatr <- makeCacheMatrix(matr)
cacheSolve(specialmatr)

