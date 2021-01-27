# Replication Data & Code for 'Accounting for Geographic Basis Risk in Heat Index Insurance: How Spatial Interpolation Can Reduce the Cost of Risk'
This repository contains R code and data used in Leppert, Dalhaus, and Lagerkvist (2021). Forthcoming. Researchers who wish to replicate our results are invited to consult the README-file. We encourage use of our code by others: Cite to this article. 

We use weather station observations from NOAA global historical climate network dataset from Illinois and Iowa, 1980 - 2019, available here: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt. To acknowledge the specific version of the dataset used, please cite: 

Menne, M.J., I. Durre, B. Korzeniewski, S. McNeal, K. Thomas, X. Yin, S. Anthony, R. Ray, R.S. Vose, B.E.Gleason, and T.G. Houston, 2012: Global Historical Climatology Network - Daily (GHCN-Daily), Version 3. [indicate subset used following decimal, e.g. Version 3.12]. NOAA National Climatic Data Center. http://doi.org/10.7289/V5D21VHZ [access date]. 

Compute cumulative annual values of variable 'TMAX' for each station and convert to CDDs > 84 degrees F. Alternatively, we provide the complete data files.

We use county-level survey corn yield data from the U.S. Department of Agriculture NASS QuickStats database available here: https://quickstats.nass.usda.gov/. Alternatively, we provide the complete data file.

To reconstruct our interpolated (IDW, OK, RK) and nearest-neighbor (NN) indices, follow the steps below: Download GHCN-Daily data into working directory, and co17_d00.shp from this repository into your RStudio workspace. Alternatively we also provide a file with pre-processed station data: stations_data.RData.
   1. Run indices_code.R as instructed which produces the four CDD indices data files.
   2. Run contract_code to calculate results presented in the paper. 
   3. Load results and run EU_RP_diffs.R to calculate relative changes in risk premiums between insurance contracts

To reconstruct the indices using different station sample sizes, change the sample within the indices_code.R script before running. 
   
To replicate plots from the paper: Consult plots.R upon replicating data
