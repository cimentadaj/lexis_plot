1) Where and how to obtain the data, and under what conditions (license)

The data used in the application is directly taken from the Human Mortality Database (HMD) interactively as the user queries the information. This means that the user needs to have internet to access the data. We chose not to include the data in the paper to make the application work for newer updates of the HMD dataset and for newer countries.

This dataset can be accesible without any fees from their website: https://www.mortality.org/. The only requirement is that the user registers on their website. For example, to access the mortality and population data used in this paper, the user can enter the website above, click on the desired country and then on "1x1" Death rates for mortality rates and on the "1x1" exposure to risk for population data.

We include instructions in our README file on how to store your HMD credentials to reproduce the application from our paper.

2) How many records and which variables were used in the analysis

The variables used in the analysis are cohort mortality rates by gender and population size by gender. From these we constructed gender differences in mortality rates and first order differences in mortality rates between years separately by gender. These variables are available for all HMD countries meaning that the complete number of records can be different as countries have data for different years and age groups. However, all data is and always will be available for free.

3) A basic meta-description of these variables

Population Exposure-to-risk: Estimates of the population exposed to the risk of death during some age-time interval are based on annual (January 1st) population estimates, with a small correction that reflects the timing of deaths within the interval. 

Deaths: Death counts are collected at the finest level of detail available. If raw data are aggregated, uniform methods are used to estimate death counts by completed age (i.e., age-last-birthday at time of death), calendar year of death, and calendar year of birth. 


Both these variables are used for males and females.
