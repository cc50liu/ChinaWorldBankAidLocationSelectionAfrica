#!/bin/bash
# cd to the specific results directory and then submit this script as 
# sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output.sh

# Loop through each Causal Salience Map in the directory
for file in CSM_KW*; do
    #Extract funder, sector, and rest of tag
    if [[ $file =~ .*Tag(wb|ch|both)_([0-9]{3})_(.*)\.pdf ]]; then
	    #put funder/sector first, followed by sort order and CSM title, then run name
        new_filename="${BASH_REMATCH[1]}_${BASH_REMATCH[2]}_80CSM_${BASH_REMATCH[3]}.pdf"
        mv "$file" "$new_filename"
    fi
done


# Loop through each Propensity Histogram in the directory
for file in Hist_KW*; do
    #Extract funder, sector, and rest of tag
    if [[ $file =~ .*Tag(wb|ch|both)_([0-9]{3})_(.*)\.pdf ]]; then
	    #put funder/sector first, followed by sort order and Hist title, then run name
        new_filename="${BASH_REMATCH[1]}_${BASH_REMATCH[2]}_60Hist_${BASH_REMATCH[3]}.pdf"
        mv "$file" "$new_filename"
    fi
done

# Loop through each Loss diagram in the directory
for file in Loss_KW*; do
    #Extract funder, sector, and rest of tag
    if [[ $file =~ .*Tag(wb|ch|both)_([0-9]{3})_(.*)\.pdf ]]; then
	    #put funder/sector first, followed by sort order and Hist title, then run name
        new_filename="${BASH_REMATCH[1]}_${BASH_REMATCH[2]}_70Loss_${BASH_REMATCH[3]}.pdf"
        mv "$file" "$new_filename"
    fi
done