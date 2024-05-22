rem To run from windows command line:
rem change directory to the figures directory.  Pass file_prefix of figures to combine as argument 1
rem Creates a file called combined_%arg1
rem Example calls:
rem ..\code\scripts\combine_figures_png_pdf.bat africa_disasters
rem ..\code\scripts\combine_figures_png_pdf.bat africa_plad_map
rem ..\code\scripts\combine_figures_png_pdf.bat africa_conflict_map_log_deaths
rem ..\code\scripts\combine_figures_png_pdf.bat africa_disasters

rem adjust runName and resultsDirectory before run
@echo off
setlocal enabledelayedexpansion

rem Process arguments
set "file_prefix=%~1"

rem Set the path to the executables
set "pdftkPath=C:\"Program Files (x86)"\PDFtk\bin\pdftk.exe"
set "magickPath=C:\"Program Files"\ImageMagick-7.1.1-Q16-HDRI\magick.exe"

rem Convert png input files to pdf 
for %%F in (".\%file_prefix%*.png") do (
    set "NameNoExt=%%~nF"  
 	echo command: %magickPath% "%%F" ".\!NameNoExt!.pdf"
 	%magickPath% "%%F" ".\!NameNoExt!.pdf"
)

rem Set the output filename
set "outputFile=.\combined_%file_prefix%.pdf"

rem Specify input files 
set "inputFiles="
for %%F in (".\%file_prefix%*.pdf") do (
    set "inputFiles=!inputFiles! "%%F""
)

rem Remove leading space from inputFiles
set "inputFiles=!inputFiles:~1!"
echo inputFiles: %inputFiles%

rem Combine the files
%pdftkPath% !inputFiles! cat output %outputFile%

echo PDF files in . combined into %outputFile% successfully.
pause
