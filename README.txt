This project was built by Colin Averill. It pairs soils data to FIA composition and growth data, and bins composition by plant mycorrhizal type. It takes advantage of code written by Trevor Andrews' "Empirical Successional Mapping" and Ryan Kelly's FIA database in PSQL format. Much of Trevor Andrew's original code was modified by Ryan Kelly so that it queried the PSQL FIA database hosted on the BU server. The "required_products_utilities" directory contains some utilities for PSQL to work in R, lists of PFTs and mycorrhizal types by FIA species codes, as well as .bil and .hdr files for PRISM climate products. "FIA_soils" contains the FIA soils products. 

The first script requires access to an FIA database. There's one on the BU server, or you can see Ryan Kelly's fia_psql project for more information on creating one:

  https://github.com/ryankelly-uiuc/fia_psql


Contents:
- FIA_soils
  - FIA soils database in .csv format (files are not very big)

-required_products_utilities
  - has PSQL_utils.R, which is a standalone version of some of PEcAn's DB tools.
  
- Scripts
  - Sequential scripts to extract FIA data and filtering to isolate forested sites that have no evidence of past cutting/logging, and then calculate growth. Soils scripts calculates soil variables on an aerial basis. There is also a script to extract climate data from PRISM data products. 

CURRENT STATUS:

script test1. shows that we lose the entire west coast set of observations when we calculate soil C and N storage on an aerial basis.
script test2. shows that we lose the west coast when we exclude plots that lack bulk density information. This is driven by the fact that all the plots on the west coast do not have BD values for the forest floor layer. They do have total mass and forest floor depth, so in theory we could calculate bulk density based on the size of the sampling frame (the 12inch diameter bicycle tire). However, based on trying to predict bulk density from mass, depth and area in the FF samples where all values are known, its clear the FIA used a variety of different sized sampling frames in the field. I was able to successfully back calculate bulk density for MS1/2 and ORG1/2 profiles (all observations fell on the 1:1 line when all values were known).

Right now I have emailed the FIA to ask about this. I also plan to powow with MD on Wed, Jan 13, 2016 about it. 