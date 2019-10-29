\name{performanceSpaces}
\alias{performanceSpaces}
\title{performanceSpaces}
\description{Simulates and maps system performance using climate scenarios generated using \code{\link{scenarioGenerator}}. Currently only visualises 2D panels.}
\usage{
performanceSpaces(data = NULL,
                  plotTag = NULL,
                  plotArgs = NULL,
                  systemModel = NULL,
                  systemArgs = NULL,
                  simDirectory = "Simulation1",
                  performance=NULL,
                  IOmode="suppress"
                  )
}

\arguments{
  \item{data}{A list containing data frames of the form \emph{Year Month Day P Temp} output from \code{\link{scenarioGenerator}}}
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
  \item{systemModel}{a function name. The function name of the system model used to generate system performance. The system model must have the arguments \emph{data} and \emph{systemArgs}.}
  \item{systemArgs}{A list containing all the arguments that are required to control the system model.}
  \item{simDirectory}{A string used to label the output directory.}
  \item{performance}{A vector of performance values from an external system model.}
  \item{IOmode}{A string that specifies the input-output mode for the scenarios = "verbose", "dev" or "suppress".} 
}


\value{Returns a 2D plot of system performance.}

\seealso{See Also: \code{\link{scenarioGenerator}} and \code{\link{plotLayers}}}

\examples{
\dontrun{
data(tankSimpleScale)

# tank_simple_scenarios<-scenarioGenerator(obs=tank_obs,                     
#                           modelTag = modelTag,
#                           attPerturb=attPerturb,
#                           exSpArgs = exSpArgs)

# Example 1: Heat Map Example
systemArgs<-list(roofArea=100,
                 nPeople=1,
                 tankVol=2000,
                 firstFlush=1,
                 write.file=FALSE,
                 metric="reliability")

plotArgs=list(title="Scenario Neutral Space",
               legendtitle="Reliability",
               xlim=c(-2,2),
               ylim=c(0.7,1.3),
               performancelimits=c(0.6,0.85))

plot<-performanceSpaces(data=tank_simple_scenarios,
                        plotTag="Heat",
                        systemModel = tankWrapper,
                        systemArgs = systemArgs)

plot$plot

# Note options
# plotArgs$contour=FALSE
# plotArgs$lowfill="antiquewhite"   ###From supported R colour names
# plotArgs$highfill="#88CCEE"       ###Hexidecimal specification also okay
}

\dontrun{
#Example 2
result<-performanceSpaces(data=tank_simple_scenarios,
                          plotTag = "Contours",
                          plotArgs=plotArgs,
                          systemModel = tankWrapper,
                          systemArgs = systemArgs)

result$plot
}
#Example 3
\dontrun{
plotArgs$contourlevels=c(0.67,0.71)

result<-performanceSpaces(data=tank_simple_scenarios,
                          plotTag = "Contours",
                          plotArgs=plotArgs,
                          systemModel = tankWrapper,
                          systemArgs = systemArgs)

result$plot
}

\dontrun{
###Example 4 - One-at-a-time/"OAT" plot
data(oatScenarios)
systemArgs=list(roofArea=50,
                 nPeople=1,
                 tankVol=3000,
                 firstFlush=1,
                 write.file=FALSE,
                 metric="reliability")

plotArgs=list(title="Scenario Neutral Space",
               legendtitle="Reliability",
               xlim=c(-2,2),
               ylim=c(0.7,1.3),
               performancelimits=c(0.6,0.85))

oat_plot=performanceSpaces(data=oat_out,
                           plotTag = "OAT",
                           plotArgs=plotArgs,
                           systemModel = tankWrapper,
                           systemArgs = systemArgs)
oat_plot
}
}