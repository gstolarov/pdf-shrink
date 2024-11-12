# OCR image-only PDF files.

## Introduction
The purpose of this article is to show how OCR information can be added to scanned, image-only PDFs. The benefit of this approach is that the files can be indexed by standard Adobe/Microsoft iFilters but also the text can be selected using visual tools (Adobe Reader, Chome, Edge, ...)

## Background
I needed to add OCR information to thousands of PDF files (stored actually in the SQL server). I wanted to create a script/utility that could be executed daily to index any new PDF files that don't already contain searchable text.  After searching for a while I found a recipe:
- Use ghostscript to extract individual pages from PDF to image (JPG) files
- Use Tesseract to extract OCR from images
- Store extracted text back to PDF

## Take 1
Apparently since I touched Tesseract last time in 2009, they added a new feature: the image and the OCR text will be exported as PDF file. I think you need Tesseract version 4+. So the solution seems pretty simple and following batch file emerged within next 20 min (see ocr.bat in the attached project):
```
set gs="C:\Program Files\gs\gs9.52\bin\gswin64c.exe"
set tesseract="C:\Program Files\Tesseract-OCR\tesseract.exe"

if '%1'=='' goto :badParams
if '%2'=='' goto :badParams
mkdir %temp%\ocr\
set nm=%~n1
SETLOCAL ENABLEDELAYEDEXPANSION 

rem split pdf into multiple jpeg
%gs% -dSAFER -dBATCH -dNOPAUSE -sDEVICE=jpeg -r300 -dTextAlphaBits=4 -o "%temp%\ocr\ocr_%nm%_%%04d.jpg" -f "%1"

rem ocr each jpeg
for %%i in (%temp%\ocr\ocr_%nm%_*.jpg) do %tesseract% -l eng "%%i" %%~pni pdf
del %temp%\ocr\ocr_%nm%_*.jpg

rem combine pdfs
set ff=#
for %%i in (%temp%\ocr\ocr_%nm%_*.pdf) do set ff=!ff! %%i
set ff=%ff:#=%
%gs% -dNOPAUSE -dQUIET -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -o "%2" %ff%
del %temp%\ocr\ocr_%nm%_*.pdf

goto :eof

:badParams
echo usage %0 pdf-In pdf-Out
```
As you can see the script does following
- Uses GhostScript to extract individual pages into the %temp%\ocr\####.jpg files
- For each of the JPG file run Tesseract to create a %temp%\ocr\####.pdf file
- Use ghost script to combine all PDF files into the output file.

## Take 1 Problems
Very quickly the problems with solution 1 arose for large PDF files (10+ pages) 

- It was pretty slow. But I figured, since it's running as a background process only for a new files, may be I can live with this.
- The output file was much, much, much larger then source - like 4 times. Times thousands of files - became a show stopper.

## Take 2 - HOCR2PDF
Other people have the same problem. Enter HOCR2PDF (https://archive.codeplex.com/?p=hocrtopdf). Apparently Tesseract, aside from outputting OCR as text or PDF, can also output results as HOCR files - effectively encoded HTML files. So the task changes slightly

- Use GhostScript to split PDF files into multiple JPGs
- Use tesseract to convert JPGs into HOCR files
- Parse HOCR files
- Use PDF library (iTextShart) to add text information to the output PDF

## Take 2 - Problems
Well, HOCR2PDF had similar problems as my original script. It was still slow and files were still pretty large, even though about half the size of solution #1. 

#Take 3 - Final
So I went ahead and created my own project.

After some troubleshooting, and performance improvements such as enabling compression, using single font for a whole page, I found out that the size bloat boils down to a single iTextShart function call
```
stamp.GetImportedPage(stamp.Reader, pg)
```
That call alone seems to add about 30K per page. And the only reason it's needed is to get page height. After replacing this call with
```
stamp.Reader.GetPageSizeWithRotation(pg).Height
```
the size bloat went away, and the output file to my surprise became actually smaller then the source (probably due to enabling compression and removing unsused object).

To address performance problem I decided to run Tesseract for each page concurrently, using ThreadPool. For large (10+ pages) file, the performance boost was drastic. 

## Using the code
The project contains a class PdfOcr with one public method OcrFile. The usage as below:
```
string txt = new PdfOcr().OcrFile(fileIn, fileOut);
```

This code will OCR the fileIn pdf file, create fileOut and return OCR text. The class might need to be customised by changing/assigning to following static variables:
- GhostScript - location of GhostScript executable
- Tesseract - location of Tesseract executable.
- wdiTemp - folder where temp files will be generated
- tmpPrfx - prefix for all the temp files
The class is stored in Program.cs file along with the main program that takes 2 arguments - source and destination file names.                       
               

## Points of Interest
- https://github.com/UB-Mannheim/tesseract/wiki - TesserAct windows binary 
- https://www.ghostscript.com/download/gsdnld.html - GhostScript binary download.
- https://archive.codeplex.com/?p=hocrtopdf - HOCR2PDF .NET utility
