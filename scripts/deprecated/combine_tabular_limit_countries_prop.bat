rem To run from windows command line:
rem C:\Documents(Local)\MastersCSS\Thesis\ThesisCodeData\eoml_ch_wb\code\slurm
rem To avoid "Input line too long" run from results directory
rem ..\code\slurm\combine_tabular_limit_countries_prop.bat scaled . 

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "runName=%~1"
set "resultsDirectory=%~2"

rem Set the path to the pdftk executable
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"

rem Set the output filename
set "outputFile=%resultsDirectory%\combined_limit_countries_%runName%_tabular_treat_prop.pdf"

rem Specify input files 
set "inputFiles="
for %%F in ("%resultsDirectory%\limit_countries_*treat_prop.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

echo after loop inputFiles: %inputFiles%

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"

echo after leading space rm inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFile%

echo PDF files in %resultsDirectory% combined into %outputFile% successfully.
pause
