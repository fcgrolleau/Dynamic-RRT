defactorize <- function(x)
{
  if(class(x)[1]=="factor" & !is.na(as.numeric(as.character(x))[1])) {
      temp <- as.numeric(as.character(x))
  } else { 
      temp <- x
  }
        return(temp)
}

# Defactorize a dataset as follows
#dat <- as.data.frame(lapply(dat, defactorize))

#Rubins rule
rubinr <-  function(thetas, vars) {
  theta <- mean(thetas)
  w <- mean(vars)
  b <- var(thetas)
  return(w+(1+1/length(vars))*b)
}
