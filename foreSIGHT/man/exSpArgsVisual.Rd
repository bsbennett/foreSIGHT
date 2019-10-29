\name{exSpArgsVisual}
\alias{exSpArgsVisual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
exSpArgsVisual
}
\description{
Visualizes the geometry of a 2D exposure space. This visualizer only works for 2d spaces (samples of 2 attributes).
}
\usage{
  exSpArgsVisual(exSpArgs=NULL
                      )
  }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exSpArgs}{a list to control the exposure space creation with the following components:
    \describe{
      \item{\code{type}}{a string that specifies the type of sampling. Defaults to regular spacing.}
      \item{\code{samp}}{a vector indicating the number of targets for each attribute in attSel.}
      \item{\code{bounds}}{a list containing elements for attributes listed in attSel, where each attribute has bounds specified. This should be a single value for a stationary target, and a vector of min and max change for primary attributes. Works with samp to create number of step sizes. Defaults with samp to only reproduce historical weather.}
    }
  }
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
library(foreSIGHT)               ###Load package

###Example 1 - visualize a 2D exposure space.
exSpArgs=list(type="regGrid",
              samp=c(5,7),
              bounds=list(P_ann_tot_m=c(0.7,1.3),
                          Temp_ann_avg_m=c(-3,3)))
#windows()
exSpArgsVisual(exSpArgs=exSpArgs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}% use one of  RShowDoc("KEYWORDS")
%\keyword{  }% __ONLY ONE__ keyword per line