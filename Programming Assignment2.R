## makeCacheMatrix : crée un objet "matrice" capable de mettre en cache son inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialisation du cache pour l'inverse
  
  # Définir la matrice
  set <- function(y) {
    x <<- y
    inv <<- NULL  # réinitialiser le cache si la matrice change
  }
  
  # Obtenir la matrice
  get <- function() x
  
  # Mettre en cache l'inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Obtenir l'inverse mis en cache
  getInverse <- function() inv
  
  # Retourner la liste de toutes les fonctions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve : calcule l'inverse de la matrice "spéciale"
## Si l'inverse est déjà en cache, le retourne directement
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Si pas en cache, calculer l'inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # calcul de l'inverse
  x$setInverse(inv)       # mise en cache
  inv                      # retourner l'inverse
}


