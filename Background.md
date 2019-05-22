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

After familiarizing myself with the Access databases, I realized I was unable to figure out a way to read in the Access data, without installing ODBC drivers which seems like too much of a process for others to have to do also.
See sandbox/get_data_from_access_FAIL.R for my failed attempts and websites.

I asked if using FromR1.csv and FromR5.csv would be sufficient/equivalent, and was told yes!
(NB: FromR5.csv column "-4" includes age 0, so must remove it from 1-4 group.)


### TYPEIDs and TYPEs
TYPE 0 = British Columbia
CF = Children and Family Development,             Levels 0,1-4          (5 in total)
DR = Development Region,                          Levels 0,1-8          (9 in total)
HA = Local Health Area,                           Levels 0,1-373        (105 in total)
HY = Health Authority,                            Levels 1-6            (6 in total, No 0=BC)
HS = Health Service Delivery Area,                Levels 0,11-53        (17 in total)
PS = College Region,                              Levels 0,1-15         (16 in total)
RD = Regional District,                           Levels 0,1000-59000   (30 in total)
SD = School District,                             Levels 0,5-92         (60 in total)
SR = Special Regions (CMAs and Vancouver Island), Levels 0,400-935      (7 in total)

