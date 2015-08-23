## The functions (written below) consist on  caching the Inverse of a Matrix
##That means to create a special object that stores a matrix and caches its inverse.
##Calculating the inverse of a matrix is usually costly computation
##The idea is when we have big matrices, we can cache the value of the inverse 
##so that when we need it again, it can be looked up in the cache rather than recomputed,
##that is because the latter will take more time.

## The first function creates an object(matrix) that caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
      m_inv<-NULL
      set<-function(y){
            x<<-y
            m_inv<<-NULL
      }
      get<-function()x
      setInv<-function(inverse)m_inv<<-inverse
      getInv<-function()m_inv
      list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## The next function computes the inverse of the created makeCacheMatrix above.
##There are two cases;  if the inverse has already been calculated (and the 
##matrix has not changed), then it should retrieve the inverse from the cache.
##if it has not been calculated, cacheSolve do that procedure.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m_inv<-x$getInv()
      if(!is.null(m_inv)){
            message("getting cached data")
            return(inv)
      }
      m<-x$get()
      #here i call the function solve for calcuting the inverse
      m_inv<-solve(m,...)
      x$setInv(m_inv)
      m_inv
}
