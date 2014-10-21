##makeCacheMatrix creates a special matrix object, and allow to cache its 
##inverse, but it does not calculate it. The inverse can either be cached 
##manually using setinverse, or it can be calculated and cached automatically by 
##cacheSolve. If there is already an inverse cached in makeCacheMatrix, 
##cacheSolve simply returns the cached value.


## makeCacheMatrix takes a matrix as argument. It returns the "special matrix 
## object" mentioned in the assignment. 
##The 4 functions returned in this list allow the user to:
##1)obtain the matrix passed as argument (get),  
##2)pass a new matrix as argument(set), 
##3)get the inverse of the matrix, if it has been cached before (getinverse),
##4)cache the inverse of the matrix (setinverse)if it has been calculated 
##elsewhere,for example by using solve(). 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## this step removes previously cached inverse 
        set <- function (y){
                x <<- y  
                i <<- NULL ## this step removes previously cached inverse. Note 
        }                  ## the <<- operator, so i is assigned a null value in 
                           ## the makeCacheMatrix environment.
        get <- function () x
        setinverse <- function(inverse) i<<-inverse ## same use of <<- as above
        getinverse <- function () i ## returns the inverse if it 
                                    ##was cached by using setinverse
        
        list(set = set, get = get,   ## List of functions. This is necessary for
             setinverse = setinverse,## indexing the functions, f.ex x$get()
             getinverse = getinverse)
}


## cacheSolve takes makeCacheMatrix(x) as an argument. It checks if an inverse  
## has been cached for makeCacheMatrix(x). If yes, it retrieves the value of the
## cache.If not, it calculates one, and caches it in makeCacheMatrix.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting inverse from cache")
                return(i)
        }
        data <- x$get()##retrieves the matrix from makeCacheMatrix
        i <- solve (data)
        x$setinverse(i) ##caches the inverse in makeCacheMatrix with setinverse
        i     ## Return a matrix that is the inverse of 'x'  
}
