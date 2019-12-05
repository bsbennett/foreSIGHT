\name{devScenarioClean}
\alias{devScenarioClean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
devScenarioClean
}
\description{
Takes the IO="dev" output of scenariogenerator and repacks it into the IO="verbose" style.
}
\usage{
devScenarioClean(obs=NULL,
                 optimArgs=NULL,
                 attPerturb=NULL,
                 attHold=NULL,
                 attPenalty=NULL,
                 modelTag=NULL,
                 modelInfoMod=list(),
                 exSpArgs=NULL, 
                 simDirectory=NULL,
                 nSeed=NULL,
                 nRep=NULL,
                 repack=FALSE
                 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A dataframe of observed climate data in the form \emph{Year Month Day P Temp}.}
  \item{optimArgs}{A list controlling the search algorithm with the following components:
    \describe{
      \item{\code{pcrossover}}{a value of probability of crossover. Defaults to 0.8.}
      \item{\code{pmutation}}{a value of probability of mutation. Defaults to 0.1}
      \item{\code{maxiter}}{a value of the maximum number of generations. Defaults to 50}
      \item{\code{maxFitness}}{a value of the stopping criteria. Defaults to 0.001}
      \item{\code{popSize}}{a value of the population size. Defaults to 100}
      \item{\code{run}}{a value of an alternative stopping criteria - consecutive runs without improvement in fitness. Defaults to 0.001}
      \item{\code{seed}}{a value of the random seed. Defaults to 1234.}
      \item{\code{parallel}}{specifies if parallel computing should be used. Defaults to False. Can be set to the number of desired cores, or \code{TRUE}, where it will detect the number of available cores and run. }
      \item{\code{keepBest}}{specifies if the optimisation should keep the best solution in each generation. Defaults to TRUE.}
      \item{\code{lambda.mult}}{A multiplier used during the optimisation for primary attributes. Defaults to zero.}
    }
  }  
  \item{attPerturb}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under shown under details below.}
  \item{attHold}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under shown under details below.}
  \item{attPenalty}{A character vector of climate attributes to place specific focus on during targeting via the use of a penalty function during the optimisation process.} 
  \item{modelTag}{A character vector of which stochastic models to use to create each climate variable.Supported tags are shown under details below.}
  \item{modelInfoMod}{A list containing information for modifying stochastic model bounds and defining fixed parameters.} 
  \item{exSpArgs}{a list to control the exposure space creation with the following components:
    \describe{
      \item{\code{type}}{a string that specifies the type of sampling. Defaults to regular spacing.}
      \item{\code{samp}}{a vector indicating the number of targets for each attribute in attSel.}
      \item{\code{bounds}}{a list containing elements for attributes listed in attSel, where each attribute has bounds specified. This should be a single value for a stationary target, and a vector of min and max change for primary attributes. Works with samp to create number of step sizes. Defaults with samp to only reproduce historical weather.}
    }
  }
  \item{simDirectory}{A string used to label the output directory.}
  \item{nSeed}{A scalar used to specify the number of seeds (in this case replicates) for the stochastic generation of time series.}
  \item{nRep}{A scalar that indicates the total number of files (i.e. no. targets x n seeds)}
  \item{repack}{If TRUE writes .csv files of all collates scenarios.}
}
\details{
Repackages output from IOmode="dev" runs of sceanariogenerator function.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
