##The  function, `makeCacheMatrix` creates a special "matrix" object 
## that can cache its inverse, which is

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the inverse of the matrix
#4.  get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y) {
                x<<-y
                mat<<-NULL
        }
        get <- function() x
        setinverse<-function(solve) mat <<- solve
        getinverse<- function() mat
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'
#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache. 
#However, it first checks to see if the
#inverse has already been calculated. If so, it `gets the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setinverse`
#function.

cacheSolve <- function(x, ...) {
        mat<-x$getinverse()
        if (!is.null(mat)) {
                message ("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        mat
}


