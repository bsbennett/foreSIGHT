\name{attributeCalculator}
\alias{attributeCalculator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
attributeCalculator
}
\description{
Calculates the attributes of a supplied set of time series data.
}
\usage{
  attributeCalculator(obs = NULL,
                      attSel = NULL,
                      slice = NULL,
                      window = 10
                      )
  }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A dataframe of observed climate data in the form \emph{Year Month Day P Temp}.}
  \item{attSel}{A character vector of climate attributes selected for calculation. A list of all supported attributes can be found under \code{\link{attSel}}}
  \item{slice}{Optional argument. A scalar value indicating the last year (e.g. 2050) of the supplied time series that is used as in input for attribute calcualtion.}
  \item{window}{the period (number of years) over which the attributes are calcualted. Note: the window is the number of years taken before the slice year. Default set at 10.}
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

###Example 1 - calculate attributes for full length of data
data(tank)
attSel=c("P_ann_tot_m","P_ann_dyWet_m","P_ann_dyAll_m")
out=attributeCalculator(obs = obs,
                        attSel = attSel,
                        slice=NULL,
                        window=NULL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}% use one of  RShowDoc("KEYWORDS")
%\keyword{  }% __ONLY ONE__ keyword per line