\name{quickSpace}
\alias{quickSpace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
quickSpace
}
\description{
Master function - generates perturbed climate scenarios, generates performance, visualizes performanace and superimposes climate projection information on to the system performance spaces
}
\usage{
  quickSpace(obs=NULL,                          
             modelTag="Simple-ann",                   
             attPerturb=NULL,                  
             attHold=NULL,                     
             attPenalty=NULL,                  
             optimArgs=list(pcrossover= 0.8,   
                            pmutation=0.1,
                            maxiter=10,
                            maxFitness=-0.001,
                            popSize = 100,
                            run=20,
                            seed = NULL,
                            parallel = FALSE,
                            keepBest=TRUE,
                            lambda.mult=0.0
                 ),
             exSpArgs=list(),
             simLengthNyrs=NULL,
             systemModel=NULL,
             systemArgs=NULL,
             plotTag="Heat",
             plotArgs=NULL,
             IOmode="suppress",    
             arrayID=NULL,        
             nSeed=NULL,
             climdata=NULL,
             climArgs=list(),
             simDirectory="Simulation1")
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
  \item{systemModel}{a function name. The function name of the system model used to generate system performance. The system model must have the arguments \emph{data} and \emph{systemArgs}.}
  \item{systemArgs}{A list containing all the arguments that are required to control the system model.}
  \item{plotTag}{A character vector which designates how the performance space is visualised. Options are: "Heat", "Contours" or "OAT".}
  \item{plotArgs}{A list controlling how the performance space is visualised with the following components:
    \describe{
      \item{title}{a string that is used as the title label.}
      \item{legendtitle}{a string that is used to label the legend.}
      \item{xlim}{x axis limits.}
      \item{ylim}{y axis limits.}
      \item{performancelimits}{z axis limits.}
      \item{lowfill}{a string specifying the colour for lower values of performance. Default is "red". For use with plotTag="Heat".}
      \item{highfill}{a string specificying the colour for the high values of performance. Default is "yellow". A colour ramp will be created between this and the lowfill. For use with plotTag="Heat".}
      \item{contour}{a TRUE/FALSE toggle for whether or not contour lines are overlaid. Default is TRUE. For use with plotTag="Heat".}
      \item{contourlevels}{a vector specifying the levels at which controus will be drawn. For use with plotTag = "Contours".}
    }
  }
  \item{IOmode}{A string that specifies the input-output mode for the scenarios = "verbose", "dev" or "suppress".} 
  \item{arrayID}{A scalar used to append output file names - only used in IOmode="dev".}
  \item{nSeed}{A scalar used to specify the number of seeds (in this case replicates) for the stochastic generation of time series.}
  \item{climdata}{a data frame of the projected climate data}
  \item{climArgs}{a list that controls the appearance of the superimposed climate projections with the following components:
     \describe{
      \item{colour}{a string that is used to set the colour of the superimposed climate drojections. Defaults to black.}
      \item{fill}{a string that is used to determine whether the system performance resulting from the climate projections should also be displayed. Set to "Performance" to infill climate projections according to their system performance.}
    }
  }
  \item{simDirectory}{A string used to label the output directory.}
}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

}
