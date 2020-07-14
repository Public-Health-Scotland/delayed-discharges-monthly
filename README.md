# Delayed Discharges Monthly
The [Delayed Discharges Monthly publication](https://beta.isdscotland.org/find-publications-and-data/health-and-social-care/delayed-discharges/delayed-discharges-in-nhsscotland-monthly/) is undergoing development to become a RAP (Reproducible Analytical Pipeline) Level 4A (version controlled and peer reviewed.)


## Folder Structure

All the publication files are stored in the following directory:

`/.../delayed_discharges/RAP development/...`

This directory contains the 'master' branch and each analyst's working branch.


### The Master Branch

The **master** folder is the **master copy** of the publication repository. This is the "production-ready" version that is used for the publication process each month. Within it there will be a folder called "data" which contains the necessary data collected and output files/basefiles for all previous publications. The master copy should **never be edited** and should only be updated from approved changes pulled from GitHub.


### Analyst Branches

These folders also contain up-to-date copies of the repository and these are the versions which are edited each time the publication is updated or each time a change to the process has to be made. Analysts should only work in their own folders on their own development branches. Once they are content that their changes are ready for the master branch, they must create a pull request on GitHub and have other analysts from the team review their changes and, if satisfied, merge them back into the master branch. It is then that the **master folder is updated.**


## The Repository

### Files

* **.gitignore:** Any files that should not be tracked by git should be added to this file.
* **00_setup_environment.R:** This script is edited each month to update dates.
* **01_validation.R:** This script checks each health board data submission and recodes and formats date variables, removes non-hospital locations and individuals aged under 18. Variables such as reason grouping, census flag and occupied bed days are derived. It requires the specialty lookup file to match on specialty description. The script outputs data query files which are fed back to health boards. The script also produces provisional census and bed day figures. This script is intended to be run more than once following resolution of queries by health boards.
* **02_read_in_and_combine_files.R:** This script reads in all health board validated data submissions and combines them to a single file. The script then recodes, formats and derives data variables (similar to the validation script). It requires the national reference file location lookup file to match on location description, the specialty lookup file to match on specialty description and postcode directory lookup file to match on geography data. The script outputs a validated file for Scotland.
* **03_census_and_bed_days.R:** This script creates all outputs required for the monthly publication spreadsheets for census and bed days. Data files are output for each of the different tabs required within the spreadsheets.
* **04_trend_file.R:** This script appends the latest monthly validated Scotland file to the previous trend file. The script outputs an updated trend file.


## Producing the Publication

### Running the Code

*	From the master branch, open `01_validation.R`, run the entire script
*	Check for any errors and investigate if necessary - when satisfied with output, move to the next step
* From the master branch, open and run `02_read_in_and_combine_files.R`
*	Check the output in data/output looks as it should
*	From the master branch, open and run `03_census_and_bed_days.R`
*	As above, check for any errors and look at the output to see if it looks as it should
* From the master branch, open and run `04_trend_file.R`
* Finally, check for any errors and look at the output to see if it looks as it should

The raw output files all have the publication date in the name, so there is no need to archive as each time the process is re-run, new files will be created. The only files which do get overwritten are the publication document files, but these are copied over to the publication folder as part of the normal publication process so are already archived. 


### Updating the code

To update the publication each month, the analyst responsible for updating the scripts/running the publication should complete the following steps:

* Pull the most recent version of the master branch into their own folder 
* Create a fresh branch to make necessary changes
* Update the dates in the `00_setup_environment.R` file
* Check filepaths for lookups in `01_validation.R` and `02_read_in_and_combine_files.R` are still correct 
* Push new branch to GitHub
* Create pull request for another analyst to review changes
* Once changes have been approved, merge the branch into the master and delete
* If no more changes are required, pull the updated master branch into the **master folder**

