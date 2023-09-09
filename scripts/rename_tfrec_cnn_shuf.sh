#!/bin/bash
# repair file names for a run I called tfrec_cnn that should have been tfrec_cnn_shuf 
# (funder/sector both_110 was correct, so I moved it to a "correct" directory while I did this
# cd ./results/tfrec_cnn_shuf
# sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_tfrec_cnn_shuf.sh
# Loop through each Causal Salience Map in the directory
for file in ./*tfrec_cnn*; do
  # Check if the file exists
  if [ -e "$file" ]; then
    # Extract the filename without the directory path
    filename=$(basename "$file")
    
    # Replace "tfrec_cnn" with "tfrec_cnn_shuf" in the filename
    new_filename="${filename//tfrec_cnn/tfrec_cnn_shuf}"
    
    # Rename the file
    mv "$file" "./$new_filename"
    
    echo "Renamed: $filename to $new_filename"
  fi
done