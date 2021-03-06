% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MAR_MethodC.R
\name{MAR_MethodC}
\alias{MAR_MethodC}
\title{MAR_MethodC}
\usage{
MAR_MethodC(n, NSIM, missRate, trueValue, cores = 1)
}
\arguments{
\item{n}{sample size in each simulated dataset}

\item{NSIM}{the number of simulation runs}

\item{missRate}{the missing rate}

\item{trueValue}{the true value of the parameter}

\item{cores}{the number of cores for parallelization, defalut = 1}
}
\value{
the beta, betaSE, meanBias, meanError, cover for methodC
}
\description{
This function will call the \code{generateData} and \code{MAR} function to generate the dataset under MAR with certain missing rate,use method C for analysis, repeate for 1000 iterations and return the beta, betaSE, meanBias, meanError, cover for each iteration
}
