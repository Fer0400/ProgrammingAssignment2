Johns Hopkins University
JOSE BAEZA DIAZ
03-07-2020
Johns Hopkins University
Tareas calificadas por los compañeros: Programming Assignment 2: Lexical Scoping
Introducción Esta segunda asignación de programación requerirá que escriba una función R que sea capaz de almacenar en caché los cálculos potencialmente lentos. Por ejemplo, tomar la media de un vector numérico suele ser una operación rápida. Sin embargo, para un vector muy largo, puede tomar demasiado tiempo calcular la media, especialmente si tiene que calcularse repetidamente (por ejemplo, en un bucle). Si el contenido de un vector no está cambiando, puede tener sentido almacenar en caché el valor de la media para que cuando lo necesitemos de nuevo, se pueda buscar en la memoria caché en lugar de volver a calcular. En esta asignación de programación, aprovechará las reglas de alcance del lenguaje R y cómo se pueden manipular para preservar el estado dentro de un objeto R.

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
