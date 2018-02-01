\name{modelTag}
\alias{modelTag}
\docType{data}
\title{
Model Tags
}
\description{
modelTag provides the main function with requested models.
}
\usage{modelTag}
\format{
  modelTag is vector of any of the following supported models:
  \describe{
    \item{\code{"Simple-ann"}}{for simple annual scaling}
    \item{\code{"P-ann-wgen"}}{for a four parameter annual rainfall model}
    \item{\code{"P-seas-wgen"}}{for a 16 parameter seasonal rainfall model}
    \item{\code{"P-har12-wgen"}}{for a harmonic rainfall model}
    \item{\code{"P-har26-wgen"}}{for a harmonic rainfall model}
    \item{\code{"P-2har26-wgen"}}{for a double harmonic rainfall model}
    \item{\code{"Temp-har26-wgen-wd"}}{for a harmonic temperature model dependent on wet or dry day}
    \item{\code{"Temp-har26-wgen"}}{for a harmonic temperature model not conditional on rainfall}
    \item{\code{"PET-har26-wgen"}}{for a harmonic potential evapotranspiration model}
  }
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
\keyword{datasets}
