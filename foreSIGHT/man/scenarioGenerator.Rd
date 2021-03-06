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
                  modelInfoMod=list(),
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
                  IOmode="suppress",
                  arrayID=NULL,
                  nSeed=NULL,
                  seedID=1234,
                  simDirectory="Simulation1"
                  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A dataframe of observed climate data in the form \emph{Year Month Day P Temp}.}
  \item{modelTag}{A character vector of which stochastic models to use to create each climate variable.Supported tags are shown under details below.}
  \item{modelInfoMod}{A list containing information for modifying stochastic model bounds and defining fixed parameters.} 
  \item{attPerturb}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under shown under details below.}
  \item{attHold}{A character vector of climate attributes to hold at a target. A list of all supported attributes can be found under shown under details below.}
  \item{attPenalty}{A character vector of climate attributes to place specific focus on during targeting via the use of a penalty function during the optimisation process.}
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
\details{
modelTag provides the main function with requested models. modelTag is vector of any of the following supported models:
  \itemize{
    \item \code{"Simple-ann"}{a simple annual scaling}
    \item \code{"P-ann-wgen"}{a four parameter annual rainfall model}
    \item \code{"P-seas-wgen"}{a 16 parameter seasonal rainfall model}
    \item \code{"P-har6-wgen"}{a harmonic rainfall model with 6 periods}
    \item \code{"P-har12-wgen"}{a harmonic rainfall model}
    \item \code{"P-har12-wgen-FS"}{a harmonic rainfall model where seasonality is fixed (phase angles must be specified via modelInfoMod=list("P-har12-wgen-FS"=fixedPars=c(x,x,x,x))}
    \item \code{"P-har26-wgen"}{a harmonic rainfall model}
    \item \code{"P-2har26-wgen"}{a double harmonic rainfall model}
    \item \code{"Temp-har26-wgen"}{a harmonic temperature model not conditional on rainfall}
    \item \code{"Temp-har26-wgen-wd"}{a harmonic temperature model dependent on wet or dry day}
    \item \code{"Temp-har26-wgen-wdsd"}{a harmonic temperature model where standard deviation parameters are dependent on wet or dry day}
    \item \code{"PET-har12-wgen"}{a harmonic potential evapotranspiration model}
    \item \code{"PET-har26-wgen"}{a harmonic potential evapotranspiration model}
    \item \code{"PET-har26-wgen-wd"}{a harmonic potential evapotranspiration model dependent on wet or dry day}
    \item \code{"Radn-har26-wgen"}{a harmonic solar radiation model (MJ/m2)}
  }
The list of attributes supported by attSel are:
  \itemize{
    \item \code{"P_ann_tot_m"}{}                    
    \item \code{"P_ann_R10_m"}{}    
    \item \code{"P_ann_maxDSD_m"}{}  
    \item \code{"P_ann_maxWSD_m"}{}  
    \item \code{"P_ann_P99_m"}{}
    \item \code{"P_ann_dyWet99p_m"}{}
    \item \code{"P_ann_ratioWS_m"}{}
    \item \code{"Temp_ann_avg_m"}{}
    \item \code{"Temp_ann_P5_m"}{}
    \item \code{"Temp_ann_P95_m"}{}
    \item \code{"Temp_ann_F0_m"}{}  
    \item \code{"P_ann_dyWet_m"}{}  
    \item \code{"P_ann_DSD_m"}{}
    \item \code{"P_seas_tot_cv"}{}
    \item \code{"P_mon_tot_cv"}{}
    \item \code{"P_ann_avgWSD_m"}{}
    \item \code{"P_ann_avgDSD_m"}{}
    \item \code{"P_JJA_avgWSD_m"}{}
    \item \code{"P_MAM_avgWSD_m"}{}
    \item \code{"P_DJF_avgWSD_m"}{}
    \item \code{"P_SON_avgWSD_m"}{}
    \item \code{"P_JJA_avgDSD_m"}{}
    \item \code{"P_MAM_avgDSD_m"}{}
    \item \code{"P_DJF_avgDSD_m"}{}
    \item \code{"P_SON_avgDSD_m"}{}
    \item \code{"Temp_ann_GSL_m"}{}
    \item \code{"Temp_ann_CSL_m"}{}
    \item \code{"P_JJA_dyWet_m"}{}
    \item \code{"P_MAM_dyWet_m"}{}
    \item \code{"P_DJF_dyWet_m"}{}
    \item \code{"P_SON_dyWet_m"}{}  
    \item \code{"P_JJA_tot_m"}{}   
    \item \code{"P_MAM_tot_m"}{}  
    \item \code{"P_DJF_tot_m"}{} 
    \item \code{"P_SON_tot_m"}{}    
    \item \code{"P_ann_nWet_m"}{}   
    \item \code{"P_ann_dyAll_m"}{} 
    \item \code{"P_JJA_dyAll_m"}{}
    \item \code{"P_MAM_dyAll_m"}{}
    \item \code{"P_DJF_dyAll_m"}{}
    \item \code{"P_SON_dyAll_m"}{}   
    \item \code{"PET_ann_avg_m"}{}   
    \item \code{"PET_ann_tot_m"}{}   
    \item \code{"PET_ann_rng_m"}{}   
    \item \code{"Temp_ann_rng_m"}{}   
    \item \code{"PET_ann_90pX_m"}{}   
    \item \code{"P_ann_90X_m"}{}     
    \item \code{"P_ann_seasRatio_m"}{}
    \item \code{"PET_ann_P5_m"}{}     
    \item \code{"PET_ann_P95_m"}{}   
    \item \code{"P_Jan_tot_m"}{}      
    \item \code{"P_Feb_tot_m"}{}      
    \item \code{"P_Mar_tot_m"}{}      
    \item \code{"P_Apr_tot_m"}{}      
    \item\code{"P_May_tot_m"}{}      
    \item \code{"P_Jun_tot_m"}{}      
    \item \code{"P_Jul_tot_m"}{}     
    \item \code{"P_Aug_tot_m"}{}      
    \item \code{"P_Sep_tot_m"}{}      
    \item \code{"P_Oct_tot_m"}{}      
    \item \code{"P_Nov_tot_m"}{}     
    \item \code{"P_Dec_tot_m"}{}     
    \item \code{"Temp_JJA_avg_m"}{}   
    \item \code{"Temp_MAM_avg_m"}{}   
    \item \code{"Temp_DJF_avg_m"}{}   
    \item \code{"Temp_SON_avg_m"}{}   
    \item \code{"Temp_Jan_avg_m"}{}   
    \item \code{"Temp_Feb_avg_m"}{}   
    \item \code{"Temp_Mar_avg_m"}{}   
    \item \code{"Temp_Apr_avg_m"}{}   
    \item \code{"Temp_May_avg_m"}{}   
    \item \code{"Temp_Jun_avg_m"}{}   
    \item \code{"Temp_Jul_avg_m"}{}   
    \item \code{"Temp_Aug_avg_m"}{}   
    \item \code{"Temp_Sep_avg_m"}{}   
    \item \code{"Temp_Oct_avg_m"}{}   
    \item \code{"Temp_Nov_avg_m"}{}   
    \item \code{"Temp_Dec_avg_m"}{}   
    \item \code{"PET_JJA_avg_m"}{}   
    \item \code{"PET_MAM_avg_m"}{}   
    \item \code{"PET_DJF_avg_m"}{}    
    \item \code{"PET_SON_avg_m"}{}    
    \item \code{"PET_JJA_tot_m"}{}    
    \item \code{"PET_MAM_tot_m"}{}    
    \item \code{"PET_DJF_tot_m"}{}    
    \item \code{"PET_SON_tot_m"}{}    
    \item \code{"PET_Jan_tot_m"}{}    
    \item \code{"PET_Feb_tot_m"}{}    
    \item \code{"PET_Mar_tot_m"}{}    
    \item \code{"PET_Apr_tot_m"}{}    
    \item \code{"PET_May_tot_m"}{}    
    \item \code{"PET_Jun_tot_m"}{}    
    \item \code{"PET_Jul_tot_m"}{}    
    \item \code{"PET_Aug_tot_m"}{}    
    \item \code{"PET_Sep_tot_m"}{}    
    \item \code{"PET_Oct_tot_m"}{}    
    \item \code{"PET_Nov_tot_m"}{}    
    \item \code{"PET_Dec_tot_m"}{}    
    \item \code{"PET_Jan_avg_m"}{}    
    \item \code{"PET_Feb_avg_m"}{}    
    \item \code{"PET_Mar_avg_m"}{}    
    \item \code{"PET_Apr_avg_m"}{}    
    \item \code{"PET_May_avg_m"}{}    
    \item \code{"PET_Jun_avg_m"}{}    
    \item \code{"PET_Jul_avg_m"}{}    
    \item \code{"PET_Aug_avg_m"}{}    
    \item \code{"PET_Sep_avg_m"}{}    
    \item \code{"PET_Oct_avg_m"}{}    
    \item \code{"PET_Nov_avg_m"}{}    
    \item \code{"PET_Dec_avg_m"}{}    
    \item \code{"PET_ann_seasRatio_m"}{}
    \item \code{"Radn_ann_avg_m"}{}
    \item \code{"Radn_ann_tot_m"}{}
    \item \code{"Radn_ann_rng_m"}{}
    \item \code{"Radn_ann_P5_m" }{}
    \item \code{"Radn_ann_P95_m"}{}
    \item \code{"Radn_JJA_avg_m"}{}
    \item \code{"Radn_MAM_avg_m"}{}
    \item \code{"Radn_DJF_avg_m"}{}
    \item \code{"Radn_SON_avg_m"}{}
    \item \code{"Radn_JJA_tot_m"}{}
    \item \code{"Radn_MAM_tot_m"}{}
    \item \code{"Radn_DJF_tot_m"}{}
    \item \code{"Radn_SON_tot_m"}{}
    \item \code{"Radn_Jan_tot_m"}{}
    \item \code{"Radn_Feb_tot_m"}{}
    \item \code{"Radn_Mar_tot_m"}{}
    \item \code{"Radn_Apr_tot_m"}{}
    \item \code{"Radn_May_tot_m"}{}
    \item \code{"Radn_Jun_tot_m"}{}
    \item \code{"Radn_Jul_tot_m"}{}
    \item \code{"Radn_Aug_tot_m"}{}
    \item \code{"Radn_Sep_tot_m"}{}
    \item \code{"Radn_Oct_tot_m"}{}
    \item \code{"Radn_Nov_tot_m"}{}
    \item \code{"Radn_Dec_tot_m"}{}
    \item \code{"Radn_Jan_avg_m"}{}
    \item \code{"Radn_Feb_avg_m"}{}
    \item \code{"Radn_Mar_avg_m"}{}
    \item \code{"Radn_Apr_avg_m"}{}
    \item \code{"Radn_May_avg_m"}{}
    \item \code{"Radn_Jun_avg_m"}{}
    \item \code{"Radn_Jul_avg_m"}{}
    \item \code{"Radn_Aug_avg_m"}{}
    \item \code{"Radn_Sep_avg_m"}{}
    \item \code{"Radn_Oct_avg_m"}{}
    \item \code{"Radn_Nov_avg_m"}{}
    \item \code{"Radn_Dec_avg_m"}{}
    \item \code{"Radn_ann_seasRatio_m"}{}
  }
}

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

data(tankDat)                         ###Load tank data

###Scenario generation arguments
modelTag="Simple-ann"
attPerturb<-c("P_ann_tot_m","Temp_ann_avg_m")
exSpArgs<-list(type = "regGrid",
               samp = c(7,6),
               bounds = list("P_ann_tot_m"=c(0.9,1.5),
                             "Temp_ann_avg_m"=c(-1,4)))

###Function call
out<-scenarioGenerator(obs=tank_obs,
                  modelTag = modelTag,
                  attPerturb=attPerturb,
                  exSpArgs = exSpArgs,
                  simDirectory="Simulation1")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}% use one of  RShowDoc("KEYWORDS")
%\keyword{  }% __ONLY ONE__ keyword per line
