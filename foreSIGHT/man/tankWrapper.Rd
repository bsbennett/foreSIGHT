\name{tankWrapper}
\alias{tankWrapper}
\title{
A demo tank model of the format required for uses in foreSIGHT examples and vignette
}
\description{
A demo tank model with wrapper to tailor format to foreSIGHT
}
\usage{
     tankWrapper(data=NULL,
                 systemArgs=NULL,
                 repID=NULL)
}
\arguments{
  \item{data}{A dataframe of observed climate data in the form \emph{Year Month Day P Temp}.}
  \item{systemArgs}{a list to control the exposure space creation with the following components:
    \describe{
      \item{\code{roofArea}}{ roof area in m2}
      \item{\code{nPeople}}{number of people using water}
      \item{\code{tankVol}}{tank volume in L}
      \item{\code{firstFlush}}{first flush depth over roof in mm}
      \item{\code{write.file}}{logical. write output tank timeseries to file T/F?}
      \item{\code{fnam}}{string indicating name of file}
      \item{\code{metric}}{string indicating what metric to report: average daily deficit - "avDeficit", volumetric reliability - "volRel", reliability - "reliability", storage efficiency - "storEff", system efficiency - "sysEff"}
    }
  }
  \item{repID}{a number that is used of append file names to distiguish between replicates.}
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%\examples{
%data(modelTag)
%## maybe str(modelTag) ; plot(modelTag) ...
%}