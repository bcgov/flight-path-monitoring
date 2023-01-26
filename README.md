# Flight Path Monitoring Application

This is an opportunity to work with the Skeena Ecosystems team within the Ministry of Lands, Water, and Resource Stewardship. We are looking for a Python and R programmer to translate and optimize Python scrips to take Flightline data submitted by tenure holders and overlay it over legal habitat areas to determine the time and distance spent in each habitat area. The python code and flight data will be provided to the contractor, input habitat databases are available on the BC Data Catalogue.

1. Translate & optimize Python to R code to monitor Flightline data over wildlife habitat areas
   - Detail the methodology laid out in the python scripts in plain English and use R language to re-create scripts
2. Develop a spatial module that will cover BC and incorporate habitat layers (all available in package bcmaps)
   - Input flight line data or pull data from TRACKS database (through API's; release date TBD)
   - Input collar data or pull data from BC Telemetry database (through API's)
   - Determine the best way to display outputs (table and maps) including time and distance spent in each habitat excursions zone

The following resources will be provided to the contractor and are available on BC Data Catalogue.

# Boostao's proposed approach

- Start by converting the Python script logic into R code functions. Open source R packages `sf` and `terra` will be used instead of the proprietary Arcpy Python package. The resulting code will have a smaller footprint, will be fully documented, and will run on a regular workstation. 
- Build a Shiny application to process flight data and display relevant information. The application will most likely have a few interfaces. Notably an interactive a `leaflet` map and an editable data table. The end user will be able to either upload data to modify the application state or download data for further analysis.
- Application will also handle the dynamic retrieval of data from the TRACKS database API and BC Telemetry database API. Static information like elevation model will be pulled at application startup from public sources or packaged with the application for better performances.
- Provide helper scripts to help the Ecosystem team reproduce the necessary environment for the Shiny application on their machine. Portable and reproducible science is key in the R community.
- For the `leaflet` map component, habitat layers will be overlaid on top of a simple base layer. Styling will be consistent with the BC Government colour scheme, but the Ecosystem team can request custom layers and variations to better suit their needs.
- Offer continuous support to help your team absorb the created assets.

# First draft delivery date

`February 3rd, 2023`

# Final Delivery date

`February 15th, 2023`

# Estimated workload

`80 hours`
