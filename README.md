# popApp
Shiny web app for Population Estimates


Original app: https://www.bcstats.gov.bc.ca/apps/PopulationEstimates.aspx
The idea is to create a shiny app (in R) which should be easier to update.


I began with downloading the data from BC Stats website:  https://www2.gov.bc.ca/gov/content/data/statistics/people-population-community/population/population-estimates

Population by Age and Sex > Provinces, Territories and Canada > 
B.C. and Other Provincial and Territorial Populations: 1971-2017 (July 1) (XLS)

Nope, I need the underlying sub-provincial data.


## Data, labels & values
PATH is \\SFP.IDIR.BCGOV\S152\PEOPLEPROJECTIONS\P18\ACCESSDATABASES\WEB\Estimates2017-2018
DATANAME.mdb: 
    PopEstimatesCustomAges.mdb for ages: A0, A1, A2, ..., A89, A90PL, TOTAL ~ DATA1 (i.e., every year)
    PopEstimatesStandardAges.mdb for age groups: LT1, A1_4, A5_9, ..., A85_89, A90PL, TOTAL ~ DATA5 (i.e., 5-yr groups)
    PATH\DATANAME.mdb with TYPE = CF and TYPEID = 0 equals data's ID = 59 (= "B.C.-C.-B.")
    GENDERID (1,2,3) = Gender (M,F,T): 1 = M, 2 = F, 3 = T (Total)

After familiarizing myself with the Access databases, I realized I was unable to figure out a way to read in the Access data, without installing ODBC drivers which seems like to much of a process for others to have to do also.
See sandbox/get_data_from_access_FAIL.R for my failed attempts and websites.

I asked if using FromR1.csv and FromR5.csv would be sufficient/equivalent, and was told yes!
