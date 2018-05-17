#-----------------------------------------------------------------------------
# Purpose:  Utility functions for statistical testing/modelling
# Author:   Feiyang Niu
# Date:     August 9, 2016
#-----------------------------------------------------------------------------


# append p value to the result returned by `aov` function
anova.test <- function(...) {
    res <- aov(...)
    res$p.value <- summary(res)[[1]][["Pr(>F)"]][[1]]
    return(res)
}


CIr <- function (r, n, level = 0.95) 
{
    r2z <- function (x) 
    {
        0.5 * log((1 + x)/(1 - x))
    }
    
    SEz <- function (n) 
    {
        1/sqrt(n - 3)
    }
    
    z2r <- function (x) 
    {
        (exp(2 * x) - 1)/(exp(2 * x) + 1)
    }
    
    CIz <- function (z, n, level = 0.95) 
    {
        noma <- 1 - level
        sez <- SEz(n)
        zs <- -qnorm(noma/2)
        mez <- zs * sez
        lcl <- z - mez
        ucl <- z + mez
        mat <- list(lcl, ucl)
        return(as.numeric(mat))
    }
    
    z <- r2z(r)
    uciz <- CIz(z, n, level)[2]
    lciz <- CIz(z, n, level)[1]
    ur <- z2r(uciz)
    lr <- z2r(lciz)
    mat <- list(lr, ur)
    return(as.numeric(mat))
}



# Using Fisher's z-transformation (atanh) and the classic normal approximation
# confidence intervals for a vector of correlations is computed.
# URL: http://stats.stackexchange.com/questions/18887/how-to-calculate-a-confidence-interval-for-spearmans-rank-correlation
# URL: https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
CIrho <- function (rho, N, level = 0.95) {
    stopifnot(rho < 1, rho > -1, N > 3, round(N) == N)
    z <- atanh(rho)
    kappa <- qnorm(1 - (1 - level)/2)
    output <- matrix(cbind(rho, tanh(z - kappa * sqrt(1/(N - 3))),
                           tanh(z + kappa * sqrt(1/(N - 3)))), ncol = 3)
    colnames(output) <- c("rho", paste(100 * (1 - level)/2, "%", collapse = ""),
                          paste(100 * (1 - (1 - level)/2), "%", collapse = ""))
    return(output)
}
