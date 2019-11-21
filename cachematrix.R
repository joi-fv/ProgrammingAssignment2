##    We have two functions that cache the inverse of a matrix


##      Here is a function called "makeCacheMatrix" that it's going to 
##      create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrix) {
      x <<- matrix
      inv <<-NULL
    }
    
    get <- function() {
      x
    }
    
    
    setInverse <- function(inverse){
      inv <<- inverse
    }
    
    getInverse <- function(){
      inv
    }
    return(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}





## This function is going to invese the matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        
        
        return(m)
}
