Here's the updated **README** with the R 4.4.0 information added to the Software Requirements section:

***

# README

## Overview

This repository accompanies the *Nature Scientific Data* Data Descriptor:  
**[Subnational disaggregation of SSP-coherent population projections for Europe]()**

It provides the full set of scripts and replication materials required to reproduce all analyses, models, and derived results presented in the publication. The associated datasets are archived on **Zenodo** and can be accessed here:  
**10.5281/zenodo.18375524**

Please ensure that all replication files are downloaded from Zenodo and extracted before starting the workflow.

***

## Repository Structure

After extracting the replication package, you will find the following main directories:

- **00_0X/** — Data ingestion, harmonization, and preprocessing scripts  
- **NN_sieved_models/** — Model fitting and sieving routines for subnational population projections  
- **01_01/** — Generation of population projections according to the SSP framework  
- **02_01/** — Incorporation of the sex dimension into the generated projections  

Scripts are numbered to indicate the correct order of execution. Follow this sequence closely to ensure replicable outcomes.

***

## Reproduction Workflow

1. **Download replication files**  
   Retrieve all files from the Zenodo repository (**10.5281/zenodo.18375524**) and unzip them into your working directory.

2. **Run preprocessing scripts**  
   Execute the scripts in numerical order from `00_01` through `00_05`.  
   These files perform initial data preparation, cleaning, and formatting for model input.

3. **Run sieved models**  
   Move to the folder `NN_sieved_models` and run all scripts contained there.  
   This step fits the regional population models and applies the sieving adjustments as described in the manuscript.

4. **Generate projections**  
   Return to the main directory and run the script(s) in `01_01`.  
   This step produces the baseline and SSP-coherent population projections at the subnational level.

5. **Add the sex dimension**  
   Finally, run the scripts in `02_01`.  
   These scripts disaggregate the projections by sex, finalizing the full set of output files.

***

## Software Requirements

- **R version 4.4.0 (2024-04-24)**  
- **Key packages**: `tidymodels`, `recipes`, `Metrics`, `brulee`, `rsample`, `workflows`, `parsnip`, `tune`, `dcortools`, `reshape2`  

***

## Output

After completing all steps, the generated outputs will include:

- Subnational population projections for Europe (baseline and SSP scenarios)  
- Sex-disaggregated population results  
- Derived datasets used for visualization and Data Descriptor reproducibility  

All outputs will be saved to the relevant `results/` directory defined in the scripts.

***

## Contact

For any questions regarding the replication process or use of these materials, please contact:  
**[Andrea Tamburini]**  
**[IIASA]**  
**[tamburini@iiasa.ac.at]**

***

