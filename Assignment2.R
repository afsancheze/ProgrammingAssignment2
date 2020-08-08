#Function 1 This function creates a special "matrix" object that can 
#cache its inverse.

makeCacheMatrix<-function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inversa <<- solve
        getinv <- function() inversa
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Function 2 This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#cacheSolve` should retrieve the inverse from the cache.#

cacheSolve <- function(x, ...) {
        inversa<- x$getinv()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        matriz <- x$get()
        inversa <- solve(matriz, ...)
        x$setinv(inversa)
        inversa
}
