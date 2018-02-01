\name{attSel}
\alias{attSel}
\docType{data}
\title{
Selected attributes
}
\description{
attSel provides the main function with the attributes required. 
}
\usage{attSel}
\format{
  attSel is a vector of any of the following supported attributes:
  \describe{
    \item{\code{"P_ann_tot_m"}}{an annual total of rainfall (mm)}
    \item{\code{"P_ann_R10_m"}}{an annual count of days over 10mm of rainfall (days)}
    \item{\code{"P_ann_maxDSD_m"}}{an annual count of the maximum duration of consecutive dry days (days)}
    \item{\code{"P_ann_maxWSD_m"}}{an annual count of the maximum duration of consecutive wet days (days)}
    \item{\code{"P_ann_P99_m"}}{the 99th percentile day of rainfall (including zero days)(mm)}
    \item{\code{"P_ann_dyWet99p_m"}}{the 99th rainfall day of annual wet days (mm)}
    \item{\code{"P_ann_ratioWS_m"}}{an annual ratio of winter to summer rainfall}
    \item{\code{"P_ann_dyWet_m"}}{the annual wet day average (mm)}
    \item{\code{"P_ann_seasRatio_m"}}{ratio between the wet seasonal total / dry season total (unitless)}
    \item{\code{"P_seas_tot_cv"}}{a numeric vector}
    \item{\code{"P_mon_tot_cv"}}{a numeric vector}
    \item{\code{"P_ann_avgWSD_m"}}{an average of the annual dry spell durations (days)}
    \item{\code{"P_ann_avgDSD_m"}}{an average of the annual wet spell durations (days)}
    \item{\code{"P_JJA_avgWSD_m"}}{an average of the June-July-August wet spell durations (days)}
    \item{\code{"P_MAM_avgWSD_m"}}{an average of the March-April-May wet spell durations (days)}
    \item{\code{"P_DJF_avgWSD_m"}}{an average of the December-January-February wet spell durations (days)}
    \item{\code{"P_SON_avgWSD_m"}}{an average of the September-October-November wet spell durations (days)}
    \item{\code{"P_JJA_avgDSD_m"}}{an average of the June-July-August dry spell durations (days)}
    \item{\code{"P_MAM_avgDSD_m"}}{an average of the March-April-May dry spell durations (days)}
    \item{\code{"P_DJF_avgDSD_m"}}{an average of the December-January-February dry spell durations (days)}
    \item{\code{"P_SON_avgDSD_m"}}{an average of the September-October-November dry spell durations (days)}
    \item{\code{"P_JJA_dyWet_m"}}{an average of the June-July-August wet day amounts (mm)}
    \item{\code{"P_MAM_dyWet_m"}}{an average of the March-April-May wet day amounts (mm)}
    \item{\code{"P_DJF_dyWet_m"}}{an average of the December-Janurary-February wet day amounts (mm)}
    \item{\code{"P_SON_dyWet_m"}}{an average of the September-October-November wet day amounts (mm)}
    \item{\code{"P_JJA_tot_m"}}{a total of the June-July-August wet day amounts (mm)}
    \item{\code{"P_MAM_tot_m"}}{a total of the March-April-May wet day amounts (mm)}
    \item{\code{"P_DJF_tot_m"}}{a total of the December-January-February wet day amounts (mm)}
    \item{\code{"P_SON_tot_m"}}{a total of the September-October-November wet day amounts (mm)}
    \item{\code{"Temp_ann_avg_m"}}{the average annual temperature (C)}
    \item{\code{"Temp_ann_P5_m"}}{the annual 5th percentile temperature (C)}
    \item{\code{"Temp_ann_P95_m"}}{the annual 95th percentile temperature (C)}
    \item{\code{"Temp_ann_rng_m"}}{the annual range between the 5th and 95th percentiles (C)}
    \item{\code{"Temp_ann_F0_m"}}{an annual count of the frost days (temperature less than zero) (days)}
    \item{\code{"Temp_ann_GSL_m"}}{an annual count of the growing season length (NH)(days)}
    \item{\code{"Temp_ann_CSL_m"}}{an annual count of the cold season length (SH)(days)}
    \item{\code{"PET_ann_tot_m"}}{an annual total of potential evapotranspiration (mm)}
    \item{\code{"PET_ann_avg_m"}}{an annual average potential evapotranspiration (mm)}  
    \item{\code{"PET_ann_rng_m"}}{an annual range of potential evapotranspiration (mm) between the 5th and 95th percentiles}    
    \item{\code{"PET_ann_90pX_m"}}{percent above historical 90th percentile (6.7mm threshold) of potential evapotranspiration (mm)}
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
%data(attSel)
%## maybe str(attSel) ; plot(attSel) ...
%}
\keyword{datasets}
