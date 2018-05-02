###Linear model
fit_lm<-function(x,y,intercept=TRUE,lambda=0)
{
  ##Conversion to matrix if required
  if (!is.matrix(x))
  {
    x=as.matrix(x)
  }
  if (!is.matrix(y))
  {
    y=as.matrix(y)
  }
  #Add the intercept coefficient
  if (intercept)
  {
    x=cbind(x,1)
  }
  my_lm=list(intercept=intercept)
  ##Compute coefficients estimates
  my_lm[['coeffs']]=solve(t(x) %*% x) %*% t(x) %*% y
  ##Compute estimates for the train dataset
  my_lm[['preds']]=x %*% my_lm[['coeffs']]
  my_lm[['residuals']]=my_lm[['preds']]-y
  my_lm[['mse']]=mean(my_lm[['residuals']]^2)
  attr(my_lm, "class") <- "my_lm"
  return(my_lm)
}