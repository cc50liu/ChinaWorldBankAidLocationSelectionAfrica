# Using machine learning and satellite imagery to estimate aid's effect on wealth: comparing China and World Bank programs in Africa
Code in support of a thesis for a Masters of Computational Social Science at Linköping University.  

- [Thesis and Appendices (PDF)](http://urn.kb.se/resolve?urn=urn:nbn:se:liu:diva-205256) 

- [AnalysisSteps](https://github.com/cc50liu/ChinaWorldBankAidLocationSelectionAfrica/blob/main/AnalysisSteps.md) explains the order the files were executed.

Overview:
- R: contains majority of the analysis code.  The code
  - writes .csv files to the ./data/interim directory which are read by later scripts.
  - writes maps, charts, and figures to a ./figures directory
  - writes tables to a ./tables directory
  - reads shapefiles, aid project, and confounder data from a ./data directory
  - writes results to a ./results directory
- python: scripts used to download satellite imagery over DHS points to NAISS (National Academic Infrastructure for Supercomputing in Sweden)
- scripts:
  - batch utility scripts to run on Windows laptop and NAISS environment
  - slurm scripts to exeucte long-running R scripts on NAISS
- env_conf: apptainer recipes to configure the NAISS python and RStudio environment where analysis runs

