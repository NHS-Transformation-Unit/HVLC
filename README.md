<img src="images/TU_logo_large.png" alt="TU logo" width="200" align="right"/>

# HVLC Activity Identification

This repository contains the scripts for identifying relevant HVLC activity within the Admitted Patient Care dataset using the criteria specified by GIRFT.

<br/>

## Using the Repo

The repository can be cloned locally and the `extract.sql` script can amended and ran on NCDR to return activity data for providers and time period of interest. This data currently then needs to be saved within the `data` folder as `extract.xlsx` to then be loaded and processed. The specialty scripts within `r_scripts` can be used to either filter the activity data for HVLC activity or to create a field to flag the HVLC activity.

<br/>

## Repo Structure

At present the structure of the repository is:

``` plaintext

├───data
├───images
└───src
    ├───config
    ├───processing
      ├───r_scripts
      └───sql_scripts
    └───requirements
    
```

<br/>

### `data`
Where the extract of APCE data will be saved for loading and processing.

### `images`

Images such as TU logos and branding to add to outputs.

### `src`

All code is stored in src. This is subdivided into three modules:

1. `config`: Files for configuring the output such as the `theme.css` for the html output.
2. `processing`: Files for extracting data from NCDR and creating the functions to filter or flag HVLC activity.
3. `requirements`: Requirements file for building the output html such as the `packages.R` script.

## Contributors

This repository has been created and developed by:

-   [Andy Wilson](https://github.com/ASW-Analyst)