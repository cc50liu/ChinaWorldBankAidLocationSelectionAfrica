rem Combined files with only treatment propensity charts
rem To run from windows command line:
rem change directory to the results directory, then change run name and funder (wb|ch|both) in command below
rem ..\..\..\code\slurm\combine_results_treatprop.bat rank_pcnl_sh_co_d1 . 

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "runName=%~1"
set "resultsDirectory=%~2"

rem Set the path to the executables
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"

rem Set the output filename
set "outputFile=%resultsDirectory%\comb_tr_prop_%runName%_results.pdf"

rem Specify input files, only including treatment propensity files 
set "inputFiles="
for %%F in ("%resultsDirectory%\*.pdf") do (
    set "currentFile=%%F"
    if "!currentFile:htreat=!" neq "!currentFile!" (
	  set "inputFiles=!inputFiles! "%%F""
	)
    if "!currentFile:Hist=!" neq "!currentFile!" (
	  set "inputFiles=!inputFiles! "%%F""
	)
    if "!currentFile:ridge=!" neq "!currentFile!" (
	  set "inputFiles=!inputFiles! "%%F""
	)	
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFile%

echo PDF files in %resultsDirectory% combined into %outputFile% successfully.
pause
