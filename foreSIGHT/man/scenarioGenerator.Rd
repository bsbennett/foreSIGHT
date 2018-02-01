\name{scenarioGenerator}
\alias{scenarioGenerator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scenario Generator
}
\description{
Produces the data needed for an exposure space of requested climate variables, attributes and bounds.
}
\usage{
scenarioGenerator(obs = NULL,
                  modelTag = NULL,
                  attPerturb = NULL,
                  attHold=NULL,
                  attPenalty = NULL,
                  optimArgs = list(pcrossover= 0.8,
                                pmutation=0.1,
                                maxiter=10,
                                maxFitness=-0.001,
                                popSize = 100,
                                run=20,
                                seed = 1234,
                                parallel = FALSE,
                                keepBest=TRUE,
                                lambda.mult = 0,
                                suggestions=NULL
                                ),
                  exSpArgs=list(),
                  simLengthNyrs=NULL,
                  IOmode="verbose",
                  arrayID=NULL,
                  nSeed=NULL,
                  seedID=1234,
                  simDirectory="Simulation1"
                  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A dataframe of observed climate data in the form \emph{Year Month Day P Temp}.}
  \item{modelTag}{A character vector of which stochastic models to use to create each climate variable.Supported tags are shown in \code{\link{modelTag}}}
  \item{attPerturb}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under \code{\link{attSel}}}
  \item{attHold}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under \code{\link{attSel}}}
  \item{attPenalty}{A character vector of climate attributes to place specific focus on during targeting via the use of a penalty function during the optimisation process.}
  \item{optimArgs}{A list controlling the search algorithm with the following components:
    \describe{
      \item{\code{pcrossover}}{a value of probability of crossover. Defaults to 0.8.}
      \item{\code{pmutation}}{a value of probability of mutation. Defaults to 0.1}
      \item{\code{maxiter}}{a value of the maximum number of generations. Defaults to 50}
      \item{\code{maxFitness}}{a value of the stopping criteria. Defaults to 0.001}
      \item{\code{popSize}}{a value of the population size. Defaults to 100}
      \item{\code{run}}{a value of an alternative stopping criteria - consecutive runs without improvement in fitness. Defaults to 0.001}
      \item{\code{seed}}{a value of the random seed. Defaults to NULL}
      \item{\code{parallel}}{specifies if parallel computing should be used. Defaults to False. Can be set to the number of desired cores, or \code{TRUE}, where it will detect the number of available cores and run. }
      \item{\code{keepBest}}{specifies if the optimisation should keep the best solution in each generation. Defaults to TRUE.}
      \item{\code{lambda.mult}}{A multiplier used during the optimisation for primary attributes. Defaults to zero.}
    }
  }
  \item{exSpArgs}{a list to control the exposure space creation with the following components:
    \describe{
      \item{\code{type}}{a string that specifies the type of sampling. Defaults to regular spacing.}
      \item{\code{samp}}{a vector indicating the number of targets for each attribute in attSel.}
      \item{\code{bounds}}{a list containing elements for attributes listed in attSel, where each attribute has bounds specified. This should be a single value for a stationary target, and a vector of min and max change for primary attributes. Works with samp to create number of step sizes. Defaults with samp to only reproduce historical weather.}
    }
  }
  \item{simLengthNyrs}{A scalar that specifies the length in years of each generated scenario. Only used with stochastic generation.}
  \item{IOmode}{A string that specifies the input-output mode for the scenarios = "verbose", "dev" or "suppress".} 
  \item{arrayID}{A scalar used to append output file names - only used in IOmode="dev".}
  \item{nSeed}{A scalar used to specify the number of seeds (in this case replicates) for the stochastic generation of time series.}
  \item{seedID}{determines how seeds are used - option "fixed","arrayID" or a number to set the seed.}
  \item{simDirectory}{A string used to label the output directory.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
library(foreSIGHT)                 ###Load package

data(tank)                         ###Load tank data

###Scenario generation arguments
modelTag="Simple-ann"
attPerturb<-c("P_ann_tot_m","Temp_ann_avg_m")
exSpArgs<-list(type = "regGrid",
               samp = c(7,6),
               bounds = list("P_ann_tot_m"=c(0.9,1.5),
                             "Temp_ann_avg_m"=c(-1,4)))

###Function call
out<-scenarioGenerator(obs=obs,
                  modelTag = modelTag,
                  attPerturb=attPerturb,
                  exSpArgs = exSpArgs,
                  simDirectory="Simulation1")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}% use one of  RShowDoc("KEYWORDS")
%\keyword{  }% __ONLY ONE__ keyword per line
