Johns Hopkins University
JOSE BAEZA DIAZ
03-07-2020
Johns Hopkins University
Tareas calificadas por los compa�eros: Programming Assignment 2: Lexical Scoping
Introducci�n Esta segunda asignaci�n de programaci�n requerir� que escriba una funci�n R que sea capaz de almacenar en cach� los c�lculos potencialmente lentos. Por ejemplo, tomar la media de un vector num�rico suele ser una operaci�n r�pida. Sin embargo, para un vector muy largo, puede tomar demasiado tiempo calcular la media, especialmente si tiene que calcularse repetidamente (por ejemplo, en un bucle). Si el contenido de un vector no est� cambiando, puede tener sentido almacenar en cach� el valor de la media para que cuando lo necesitemos de nuevo, se pueda buscar en la memoria cach� en lugar de volver a calcular. En esta asignaci�n de programaci�n, aprovechar� las reglas de alcance del lenguaje R y c�mo se pueden manipular para preservar el estado dentro de un objeto R.

## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    
    ## Initialize the inverse property
    i <- NULL
    
    ## Method to set the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    
    
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}
