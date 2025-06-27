# Jackson River Trout Spawning and Hatching Model

Description, brief overview

## How to Use

- Download or clone this repository. 
- Ensure you have R and RStudio downloaded. 
- In an R console, load the Shiny library: `library(shiny)`
- Set your working directory to the root of this repository. Run the code within app.R to start the app.

## Development

There are a few very important steps to follow if contributing to this repository. Before making any pushes to this repository, ensure each step below is followed:
- Make sure the date in `last_update.txt` is **one day later than** the last entry in the corresponding year's CSV file in the `data/` directory.
- **If you have ADDED or UPDATED any packages/libraries that this code depends on:** 
    - Run `rsconnect::writeManifest()` in your R console, and ensure `manifest.json` contains the updated info.
    - This adds the package details to `manifest.json`, which is necessary for the cloud deployment to load all necessary libraries.

## Attributions:

Research and model by [Dr. Robert Humston,](https://www.wlu.edu/profile/humston-robert) Professor and Director of Environmental Studies at Washington and Lee University 

Data from [USGS Water Services](https://waterservices.usgs.gov/)

Website and application by Maya Humston
