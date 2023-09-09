#!/bin/bash
# cd to the specific results directory and then submit this script as 
# sh ../../ChinaWorldBankAidLocationSelectionAfrica/slurm/rename_output_sector_groups.sh
# Loop through each Causal Salience Map in the directory
for file in CSM_KW*; do
    # Extract the part after "Tag"
    suffix="${file#*Tag}"
    suffix="${suffix%.pdf}"


    # Extract the part before "Tag"
    prefix="${file%%Tag*}"
    prefix="${prefix%_}"


    # Rename the file, reversing order of the parts 
    new_filename="${suffix}_${prefix}.pdf"
    mv "$file" "$new_filename"
done


# Loop through each Propensity Histogram in the directory
for file in Hist_KW*; do
    # Extract the part after "Tag"
    suffix="${file#*Tag}"
    suffix="${suffix%.pdf}"


    # Extract the part before "Tag"
    prefix="${file%%Tag*}"
    prefix="${prefix%_}"


    # Rename the file, reversing order of the parts
    new_filename="${suffix}_${prefix}.pdf"
    mv "$file" "$new_filename"
done

# Loop through each Loss diagram in the directory
for file in Loss_KW*; do
    # Extract the part after "Tag"
    suffix="${file#*Tag}"
    suffix="${suffix%.pdf}"


    # Extract the part before "Tag"
    prefix="${file%%Tag*}"
    prefix="${prefix%_}"


    # Rename the file, reversing order of the parts
    new_filename="${suffix}_${prefix}.pdf"
    mv "$file" "$new_filename"
done

mkdir Infrastructure
mkdir Interventions
mkdir Other
mkdir BasicServices

mv *_310_* Interventions/
mv *_320_* Interventions/
mv *_330_* Interventions/


mv *_210_* Infrastructure/
mv *_220_* Infrastructure/

mv *_110_* BasicServices/
mv *_120_* BasicServices/
mv *_130_* BasicServices/
mv *_140_* BasicServices/
mv *_150_* BasicServices/
mv *_160_* BasicServices/
mv *_230_* BasicServices/
mv *_240_* BasicServices/
mv *_410_* BasicServices/
mv *_420_* BasicServices/

mv *_430_* Other/
mv *_520_* Other/
mv *_530_* Other/
mv *_600_* Other/
mv *_700_* Other/
mv *_920_* Other/
mv *_998_* Other/

