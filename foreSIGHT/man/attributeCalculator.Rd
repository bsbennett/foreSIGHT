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
  \item{attSel}{A character vector of climate attributes selected for calculation. A list of all supported attributes can be found in details below.}
  \item{slice}{Optional argument. A scalar value indicating the last year (e.g. 2050) of the supplied time series that is used as in input for attribute calcualtion.}
  \item{window}{the period (number of years) over which the attributes are calcualted. Note: the window is the number of years taken before the slice year. Default set at 10.}
}
\details{
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
###Example 1 - calculate attributes for full length of data
data(tankDat)
attSel=c("P_ann_tot_m","P_ann_dyWet_m","P_ann_dyAll_m")
out=attributeCalculator(obs = tank_obs,
                        attSel = attSel,
                        slice=NULL,
                        window=NULL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}% use one of  RShowDoc("KEYWORDS")
%\keyword{  }% __ONLY ONE__ keyword per line