# ######################################################################
# Assignment: Caching the Inverse of a Matrix
# makeCacheMatrix and cacheSolve functions create special object that 
# caches the matrix and its inverse to prevent expensive recomputations.
# ######################################################################

# ######################################################################
# makeCacheMatrix - Creates a "special" matrix that is really a list of 
# 4 functions -  get() and set() get and set the value of the matrix 
# while getInverse() and setInverse() get and set the matrix inverse.
# The matrix argument provided defaults to a 1x1 matrix of NA,
# but it is ASSUMED that the argument is invertible if provided 
# or using in calling the set function.
# ######################################################################
makeCacheMatrix <- function( x = matrix() ) {  
                m <- NULL
                set <- function( y ) {
                        x <<- y   
                        m <<- NULL
                }
                get <- function() x  
                setInverse <- function( minverse ) m <<- minverse
                getInverse <- function() m
                list( set = set, get = get, 
                      setInverse = setInverse,
                      getInverse = getInverse )
}

# ######################################################################
# cacheSolve calculates the inverse of the "special" matrix created with 
# makeCacheMatrix only if the inverse has not been computed before.
# The argument x is the "special" matrix
# ######################################################################
cacheSolve <- function( x, ... ) {
        m <- x$getInverse()
        if ( !is.null( m ) ) {
                message( "getting cached inverse of the matrix " )
                return( m )
        }
        data <- x$get()
        m <- solve( data, ... )
        x$setInverse( m )
        m
}

# ######################################################################

