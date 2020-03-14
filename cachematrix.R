##The particular function creates a specific "MATRIX" object which can cache its inverse
## makeCacheMatrix creates a list containing a function to:
## Get & Set value of MATRIX
## Get & Set value of inv MATRIX
makeCacheMatrix <- function(a = matrix()) {
inv <- NULL
    SET <- function(c)
        {  a <<- c
            inv <<- NULL }
    GET <- function() a
    SETinverse <- function(inverse) inv <<- inverse
    GETinverse <- function() inv
    list(SET=SET, GET=GET, SETinverse=SETinverse, GETinverse=GETinverse)
}
##The particular function computes inverse of a special MATRIX returned by
##makeCacheMatrix (*which is the function ABOVE*)
##Computing inverse of square MATRIX is done with the solve
##function in R .... if X = square invertible MATRIX,
##solve(X) returns inv
##For the specific assignment.... assume the MATRIX supplied is always invertible!!!

cacheSolve <- function(b, ...) {

inv <- b$GETinverse()
    if(!is.null(inv))
        { message("Cached data is:")
        return(inv) }  
    data <- b$GET()
    inv <- solve(data)
    b$SETinverse(inv)
        ## Return a matrix that is the inverse of 'b'
    inv
}
