rem To run from windows command line:
rem C:\Documents(Local)\MastersCSS\Thesis\ThesisCodeData\eoml_ch_wb\results\no_trans_death
rem ..\..\code\slurm\combine_results_png_pdf.bat no_trans_death . 

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "runName=%~1"
set "resultsDirectory=%~2"

rem Set the path to the executables
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"
set "magickPath=C:\"Program Files"\ImageMagick-7.1.1-Q16-HDRI\magick.exe"

rem Convert png input files to pdf 
for %%F in ("%resultsDirectory%\*.png") do (
    set "NameNoExt=%%~nF"  
 	echo command: %magickPath% "%%F" "%resultsDirectory%\!NameNoExt!.pdf"
 	%magickPath% "%%F" "%resultsDirectory%\!NameNoExt!.pdf"
)

rem Set the output filename
set "outputFile=%resultsDirectory%\combined_%runName%_results.pdf"

rem Specify input files 
set "inputFiles="
for %%F in ("%resultsDirectory%\*.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFile%

echo PDF files in %resultsDirectory% combined into %outputFile% successfully.
pause
