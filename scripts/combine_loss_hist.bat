rem To run from windows command line:
rem change directory to the results directory, then change run name and funder (wb|ch|both) in command below
rem ..\..\..\code\scripts\combine_loss_hist.bat . 

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "resultsDirectory=%~1"

rem Set the path to the executables
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"
set "magickPath=C:\"Program Files"\ImageMagick-7.1.1-Q16-HDRI\magick.exe"


rem Set the output filenames
set "outputFileLoss=%resultsDirectory%\combined_loss.pdf"
set "outputFileHist=%resultsDirectory%\combined_hist.pdf"

rem Specify input files 
set "inputFiles="
for %%F in ("%resultsDirectory%\Hist_*.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFileHist%

echo PDF files in %resultsDirectory% combined into %outputFileHist% successfully.
pause

rem Specify input files 
set "inputFiles="
for %%F in ("%resultsDirectory%\Loss_*.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFileLoss%

echo PDF files in %resultsDirectory% combined into %outputFileLoss% successfully.
pause