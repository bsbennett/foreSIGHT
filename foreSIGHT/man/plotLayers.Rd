\name{plotLayers}
\alias{plotLayers}
\title{plotLayers}
\description{Superimposes climate projection information on to the 2D system performance spaces generated using \code{\link{performanceSpaces}}}
\usage{
  plotLayers(plot = NULL,
             plotArgs = NULL,
             climdata = NULL,
             climArgs = list(),
             simDirectory = "Simulation1",
             IOmode="verbose"
                  )
  }

\arguments{
  \item{plot}{A, editable plot output from \code{\link{performanceSpaces}}. The editable plot can be accessed via plot$plotEdit.}
  \item{plotArgs}{A list controlling how the performance space is visualised with the following components:
    \describe{
      \item{title}{a string that is used as the title label.}
      \item{legendtitle}{a string that is used to label the legend.}
      \item{xlim}{x axis limits.}
      \item{ylim}{y axis limits.}
      \item{performancelimits}{z axis limits.}
      \item{lowfill}{a string specifying the colour for lower values of performance. Default is 'red". For use with plotTag="Heat".}
      \item{highfill}{a string specificying the colour for the high values of performance. Default is "yellow". A colour ramp will be created between this and the lowfill. For use with plotTag="Heat".}
      \item{contour}{a TRUE/FALSE toggle for whether or not contour lines are overlaid. Default is TRUE. For use with plotTag="Heat".}
      \item{contourlevels}{a vector specifying the levels at which controus will be drawn. For use with plotTag = "Contours".}
    }
  }
  \item{climdata}{a data frame of the projected climate data}
  \item{climArgs}{a list that controls the appearance of the superimposed climate projections with the following components:
     \describe{
      \item{colour}{a string that is used to set the colour of the superimposed climate drojections. Defaults to black.}
      \item{fill}{a string that is used to determine whether the system performance resulting from the climate projections should also be displayed. Set to "performance" to infill climate projections according to their system performance.}
    }
  }
  \item{simDirectory}{A string used to label the output directory.}
  \item{IOmode}{A string that specifies the input-output mode for the scenarios = "verbose", "dev" or "suppress".} 
}

\value{climate projection information superimposed on a 2D system performance spaces plot.}
\seealso{See Also: \code{\link{scenarioGenerator}}, \code{\link{performanceSpaces}} and \code{\link{quickSpace}}}

\examples{
data(tankclim)
data(climdata2030)               ###loading climate data, can also use 2050, 2070 and 2085

###Example 1
plotArgs$title="Scenario neutral space with projections overlaid"

plotLayers(plot=tank.plot$plotEdit,
           plotArgs=plotArgs,
           climdata=climdata,
           climArgs=climArgs)

###Example 2
climArgs$fill="Performance"

plotLayers(plot=tank.plot$plotEdit,
           plotArgs=plotArgs,
           climdata=climdata,
           climArgs=climArgs)
}
