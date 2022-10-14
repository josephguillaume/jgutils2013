h.equations <- function(Hmatrix){
  ## TODO: varnames
  eq <- Hmatrix[,1,drop=F]
  b <- Hmatrix[,2,drop=F]
  A <- -Hmatrix[,-c(1,2),drop=F]
  getVar <- function(val,var) {
    if(round(val,2)==0) return(NA)
    if(round(val,2)==1) return(sprintf("V%d",var))
    if(round(val,2)==-1) return(sprintf("- V%d",var))
    return(sprintf("%.2f V%d",val,var))
  }
  getEqn <- function(vals) {
    vars <- sapply(1:length(vals),function(i) getVar(vals[i],i))
    vars <- vars[!is.na(vars)]
    paste(vars,collapse=" + ")
  }
  A <- apply(A,1,getEqn)
  sapply(1:nrow(Hmatrix),function(i) ifelse(eq[i]==1,
                                               sprintf("%s = %.2f",A[i],b[i]),
                                               sprintf("%s <= %.2f",A[i],b[i])
                                               )
         )
}

## -1, 0, or +1 if it is in the interior, boundary, or exterior
membership <- function(MV,points=MV[,-c(1,2)]){
  if (!is.matrix(points)) points <- matrix(points,nrow=1)
  
  MH <- scdd(MV)$output
  A <- -MH[,-c(1,2)]
  b <- MH[,2]

  axb <- (A %*% t(points))-b
  apply(axb,2,function(x) {
    y <- max(x)
    ifelse(abs(y)<1e-6,0,sign(y))
  })
}

##a <- factor(membership,levels=c(-1,0,1),labels=c("interior","boundary","exterior"))
## p8 of vignette
membershipQ <- function(MV,points=MV[,-c(1,2)]){
  if (!is.matrix(points)) points <- matrix(points,nrow=1)
  MH <- scdd(d2q(MV))
  b <- MH$output[, 2]
  a <- qneg(MH$output[, -c(1, 2)])
  axb <- qmatmult(a, t(points))

  axb <- sweep(axb, 1, b, FUN = qmq)
  apply(axb, 2, function(foo) max(qsign(foo)))
}

##p14 of vignette
## hull.points does not have to be V representation
is.exteriorQ <- function(hull.points,test.points){
  if (!is.matrix(test.points)) test.points <- matrix(test.points,nrow=1)
  apply(test.points,1,function(test.point){
    hrep <- cbind(0, 0, 1, -hull.points)
    hrep <- rbind(hrep, c(0, 1, 1, -test.point))
    out <- lpcdd(d2q(hrep), d2q(c(-1, test.point)), minimize = FALSE)
    out$optimal.value=="1"
  })
}
