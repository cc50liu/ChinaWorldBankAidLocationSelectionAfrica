rem To run from windows command line:
rem change directory to the results directory, then change run name and funder (wb|ch|both) in command below
rem ..\..\code\slurm\combine_results_png_pdf.bat v3ongoing ch . 

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "runName=%~1"
set "funder=%~2"
set "resultsDirectory=%~3"

rem Set the path to the executables
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"
set "magickPath=C:\"Program Files"\ImageMagick-7.1.1-Q16-HDRI\magick.exe"

rem Convert png input files to pdf 
for %%F in ("%resultsDirectory%\%funder%*.png") do (
    set "NameNoExt=%%~nF"  
 	echo command: %magickPath% "%%F" "%resultsDirectory%\!NameNoExt!.pdf"
 	%magickPath% "%%F" "%resultsDirectory%\!NameNoExt!.pdf"
)

rem Set the output filename
set "outputFile=%resultsDirectory%\combined_%funder%_%runName%_results.pdf"

rem Specify input files 
set "inputFiles="
for %%F in ("%resultsDirectory%\%funder%*.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFile%

echo PDF files in %resultsDirectory% combined into %outputFile% successfully.
pause
