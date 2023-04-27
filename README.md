# R-Code for Final-Year Mathematics Project: Industries and Interdependence
Code used for results reviewed in Final Year Mathematics Project - Industries &amp; Interdependence: a Graph-based Study of Input-Output Economics
*All data used within this code was taken from the World Input-Output Database Timmer, et al. (2014) and Mcnerney,
et al. (2020), referenced in the report's main text. Code was written on local files.*

*For this Project's First Analysis, assuming constant prices and omitting exports/imports for 10-year productivity 
improvements:*

clean_data retrieves WIOTs for 1995-2011.

compute_ANNG generates 10-year producitivity growth rates and their average nearest neighbourh estimates.

data_analysis looks at some regressions of ANNG on observed growth.

data_visualisation generates plots on the ANNG correlation across variables. 

bootstrap_ANNG benchmarks the observed correlation measures against bootstrapped 'random matrix' estimates
of the correlation, following Pichler, Lafond, and Farmer (2018). 

xtra_ denotes additional research not included in the report. 

*For this report's second analysis, computing 1-year productivity improvements using the world industry network:*

compute_growth_rate_assortativity computes real price changes from WIOD data, productivity improvements, ANNG, and growth rate assortativity. 

benchmark_growth_rate_assortativity estimates mean and variance of growth rate assortativity on a random matrix, based on Algorithm 2 of the main report. 
