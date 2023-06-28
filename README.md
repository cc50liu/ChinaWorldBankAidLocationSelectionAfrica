# China and the World Bank: using machine learning and satellite imagery to identify how they select African communities for aid and to estimate that aid's effect on wealth in selected communities
Code in support of a thesis for a Masters of Computational Social Science at Linköping University.

Overview:
- R: contains majority of the analysis code.  [main.R](https://github.com/cc50liu/ChinaWorldBankAidLocationSelectionAfrica/blob/main/R/main.R) shows the order the files are intended to be executed (but is not yet ready to be executed itself).  The code
  - writes .csv files to the ./data/interim directory which are read by later scripts.
  - writes maps, charts, and figures to a ./figures directory
  - reads shapefiles, aid project, and confounder data from a ./data directory
- python: scripts used to download satellite imagery over DHS points to SNIC/NAISS
- slurm:  scripts to exeucte long-running R scripts on SNIC/NAISS

