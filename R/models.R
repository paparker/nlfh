FH_Fit <- function(Y, X, S2, sig2b=1000, iter=1000, burn=500){
  ### This is the basic Bayesian FH model.
  # Y is a vector of direct estimates (length is the number of observations)
  # X is a matrix (number of rows equals number of observations) of covariates
  # S2 is a vector of sampling variances associated with the direct estimates (length is the number of observations)
  # sig2b is the prior variance for the regression coefficients
  # iter is the number of MCMC iterations
  # burn is the number of MCMC iterations to be discarded as burn-in
  n <- length(Y)
  p <- ncol(X)
  r <- n
  tau2 <- 1
  eta1 <- rnorm(n)
  tau2Out <- rep(NA, iter)
  beta1Out <- matrix(NA, nrow=iter, ncol=p)
  eta1Out <- matrix(NA, nrow=iter, ncol=r)
  XP <- cbind(X, diag(n))
  thetaOut <- xbOut <- matrix(NA, nrow=n, ncol=iter)
  ll <- rep(NA, iter)
  XtX <- t(X)%*%Diagonal(x=1/S2)%*%X
  pb <- txtProgressBar(min=0, max=iter, style=3)
  for(i in 1:iter){
    ## Fixed effects
    meanBeta <- t(X)%*%Diagonal(x=1/S2)%*%(Y-eta1)
    Ub <- chol(forceSymmetric(XtX + 1/sig2b*Diagonal(p)))
    b <- rnorm(p)
    beta1 <- beta1Out[i,] <- backsolve(Ub, backsolve(Ub, meanBeta, transpose=T) + b)


    ## Random effects
    var <- solve(Diagonal(x=1/S2+(1/tau2)))
    meanEta <- var%*%Diagonal(x=1/S2)%*%(Y-X%*%beta1)
    eta1 <- eta1Out[i,] <- rnorm(n, mean=as.numeric(meanEta), sd=sqrt(diag(var)))


    theta <- thetaOut[,i] <- X%*%beta1 + eta1
    xbOut[,i] <- theta - eta1

    tau2 <- tau2Out[i] <- 1/rgamma(1,
                                   0.1 + n/2,
                                   0.1 + t(theta-X%*%beta1)%*%(theta-X%*%beta1)/2)
    ll[i] <- -2*sum(dnorm(Y, mean = theta, sd=sqrt(S2), log=T))

    setTxtProgressBar(pb, i)
  }
  DIC <- 2*mean(ll[-c(1:burn)]) + 2*sum(dnorm(Y, mean=rowMeans(thetaOut[,-c(1:burn)]), sd=sqrt(S2), log=T))
  return(list(Preds=thetaOut[,-c(1:burn)], Tau2=tau2Out[-c(1:burn)], Beta=beta1Out[-c(1:burn),], XB=xbOut[,-c(1:burn)], DIC=DIC))
}



FH_RNN_Fit <- function(Y, X, S2, nh=200, iter=1000, burn=500){
  ### This is the random weight neural network Bayesian FH model (nonlinear method 1).
  # Y is a vector of direct estimates (length is the number of observations)
  # X is a matrix (number of rows equals number of observations) of covariates
  # S2 is a vector of sampling variances associated with the direct estimates (length is the number of observations)
  # nh is the number of hidden nodes in the random weight neural network
  # sig2b is the prior variance for the regression coefficients
  # iter is the number of MCMC iterations
  # burn is the number of MCMC iterations to be discarded as burn-in
  n <- length(Y)
  p <- ncol(X)
  r <- n
  tau2 <- sig2b <- 1
  eta1 <- rnorm(n)
  tau2Out <- sig2bOut <- rep(NA, iter)
  beta1Out <- matrix(NA, nrow=iter, ncol=nh)
  eta1Out <- matrix(NA, nrow=iter, ncol=r)
  HL <- plogis(X%*%matrix(rnorm(p*nh, sd=1), nrow=p))
  XtX <- t(HL)%*%Diagonal(x=1/S2)%*%HL
  thetaOut <- xbOut <- matrix(NA, nrow=n, ncol=iter)
  ll <- rep(NA, iter)
  pb <- txtProgressBar(min=0, max=iter, style=3)
  for(i in 1:iter){
    ## Fixed effects
    meanBeta <- t(HL)%*%Diagonal(x=1/S2)%*%(Y-eta1)
    Ub <- chol(forceSymmetric(XtX + 1/sig2b*Diagonal(nh)))
    b <- rnorm(nh)
    beta1 <- beta1Out[i,] <- backsolve(Ub, backsolve(Ub, meanBeta, transpose=T) + b)


    ## Random effects
    var <- solve(Diagonal(x=1/S2+(1/tau2)))
    meanEta <- var%*%Diagonal(x=1/S2)%*%(Y-HL%*%beta1)
    eta1 <- eta1Out[i,] <- rnorm(n, mean=as.numeric(meanEta), sd=sqrt(diag(var)))


    ## sample mean function coefs
    theta <- thetaOut[,i] <- HL%*%beta1 + eta1
    xbOut[,i] <- theta - eta1

    tau2 <- tau2Out[i] <- 1/rgamma(1,
                                   0.1 + n/2,
                                   0.1 + t(theta-HL%*%beta1)%*%(theta-HL%*%beta1)/2)

    sig2b <- sig2bOut[i] <- 1/rgamma(1,
                                     20 + nh/2,
                                     8 + sum(beta1^2)/2)

    ll[i] <- -2*sum(dnorm(Y, mean = theta, sd=sqrt(S2), log=T))

    setTxtProgressBar(pb, i)
  }
  DIC <- 2*mean(ll[-c(1:burn)]) + 2*sum(dnorm(Y, mean=rowMeans(thetaOut[,-c(1:burn)]), sd=sqrt(S2), log=T))
  return(list(Preds=thetaOut[,-c(1:burn)], Tau2=tau2Out[-c(1:burn)], sig2b=sig2bOut[-c(1:burn)], Beta=beta1Out[-c(1:burn),], XB=xbOut[,-c(1:burn)], DIC=DIC))
}





BARTFH <- function(y, x, D, a=0.01, b=0.01, n.iter=1000, n.burn=100, n.bart.samples=10, n.tree=50){
  ### This is the BART Bayesian FH model (nonlinear method 2).
  # y is a vector of direct estimates (length is the number of observations)
  # x is a matrix (number of rows equals number of observations) of covariates
  # D is a vector of sampling variances associated with the direct estimates (length is the number of observations)
  # a is the shape parameter for the inverse gamma prior on the random effects variance
  # b is the rate parameter for the inverse gamma prior on the random effects variance
  # sig2b is the prior variance for the regression coefficients
  # n.iter is the number of MCMC iterations
  # n.burn is the number of MCMC iterations to be discarded as burn-in
  # n.bart.samples is a control parameter for dbarts -- A non-negative integer giving the default number of samples to return each time the sampler is run
  # n.tree is a control parameter for dbarts -- A positive integer giving the number of trees used in the sum-of-trees formulation.

  n <- length(y)
  p <- ncol(x)
  u <- rnorm( n, sd=0.01)
  sigma_u2 <- 1

  varcount_iter <- matrix(NA, nrow = n.iter, ncol = p-1,
                          dimnames = list(NULL, colnames(x[,-1])))
  varprob_iter  <- varcount_iter
  # Create storage for posterior samples
  u_samples <- matrix(NA, nrow = n.iter, ncol = n)
  sigma_u2_samples <- numeric(n.iter)
  f_samples <- array(NA, dim = c(n.iter, n))  # 3D array to store f(x_i) samples


  # Initialize BART model control parameters
  bart_control <- dbartsControl(updateState = FALSE, n.samples = n.bart.samples,
                                n.burn = 0, n.trees = 50, n.chains = 1L)


  # Initialize the BART model with a placeholder response
  bart_sampler <- dbarts(
    formula = y ~ .,
    data = data.frame(y = y, x),
    weights = 1 / D,
    control = bart_control,
    verbose = FALSE
  )
  pb <- txtProgressBar(min=0, max=n.iter, style=3)
  for (iter in 1:n.iter) {
    # Update y_star
    y_star <- y - u

    # Update BART model data
    bart_sampler$setResponse(y_star)

    # Run BART sampler for n.bart.samples iterations
    bart_fit <- bart_sampler$run()
    vc <- bart_fit$varcount
    vc_sum <- rowSums(vc)
    varcount_iter[iter, ] <- vc_sum
    varprob_iter[iter, ]  <- if(sum(vc_sum) > 0){vc_sum / sum(vc_sum)}else{rep(0, p)}


    # Extract f(x_i) samples
    f_xi_samples <- bart_fit$train

    # Store f(x_i) samples
    f_samples[iter, ] <- rowMeans(f_xi_samples)

    # Average over BART samples to estimate f(x_i)
    f_xi <- rowMeans(f_xi_samples)

    # Update u_i
    for (i in 1:n) {
      mu_i <- ((y[i] - f_xi[i]) * sigma_u2) / (D[i] + sigma_u2)
      var_u_i <- (D[i] * sigma_u2) / (D[i] + sigma_u2)
      u[i] <- rnorm(1, mean = mu_i, sd = sqrt(var_u_i))
    }
    #u <- theta - X%*%c(3,-1)

    # Store u_i
    u_samples[iter, ] <- u

    # Update sigma_u2
    shape <- a + n / 2
    rate <- b + sum(u^2) / 2
    sigma_u2 <- 1 / rgamma(1, shape, rate)

    # Store sigma_u2
    sigma_u2_samples[iter] <- sigma_u2
    setTxtProgressBar(pb, iter)
  }
  return(list(f_x=f_samples[-c(1:n.burn),], u=u_samples[-c(1:n.burn),], sig2=sigma_u2_samples[-c(1:n.burn)],
              vi_props=colMeans(varprob_iter[-c(1:n.burn), , drop=F])))
}
