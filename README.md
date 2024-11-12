# PDF-Shrink

## Introduction
Now days the hard-drive storage costs peanuts. However if you are storing files submitted by the user on the server, it can quite quickly add up to hundreds of gigabytes. The traditional suspect is scanned PDFs. Those contain image blobs, sometimes scanned with full color and high resolution. Sometimes, those are generated on the smart-phones with 5-8 megapixel cameras and can take as much as 3 megabytes/page. I was surprised to find though that aside from those, some of the text PDFs can also take megabytes. Apparently a lot of applications that allow to save information into PDF don't think twice about saving actual font glyphes into the PDF. So the scope of the project was:
- Try to remove font glyphs from the PDF
- Scale down images
  
Due to legacy code I had to support, I used iTextSharp library (https://www.nuget.org/packages/itextsharp) to manipulate PDF. The migration to the latest supported iText (https://itextpdf.com/) should not be a huge problem.


## PDF structure
The tool that helped me to navigate through the PDF was RUPS (https://itextpdf.com/products/rups). Using this tool you can visualize PDF internal structure. Overall PDF have a list of objects stored under XRef node. The objects of the interest are Stream (can be either Image or Form) and Fonts. Separately there is an Root/Catalog/Pages array. For each page in this array, used resources are references in ./Resources/XObject and/or ./Resources/Font. Those are so-called indirect references to the Fonts and Images from XRef list.

## Fonts
Surprisingly even Acrobat/PDF Optimizer (older version though) can't properly identify all the space used by the fonts. Some of the space used by fonts, Acrobat attributes to Document Overhead.
The Font object is a dictionary of font attributes, e.g. BaseFont, Encoding, SubType. Additional information can be stored in the /FontDescriptor and/or /FontDescendants object. The key element here is /FontFile2 attribute that contains a stream of glyphs. This is where most of the space is used. In the symple case of TrueType fonts you can use the method descibed in https://github.com/QuestPDF/QuestPDF/issues/31 - rename a font and remove /FontFile2 attribute. However it only works with simple TrueType fonts without any particular encoding. A lot of apps however do not save a /TrueType font with default encoding, but rather to streamline the storage, they can only store a range of glyphs. E.g. to represent a 'ABC' text, /FontDescriptor element can specify to store /FirstChar=65, /LastChar=67 and instead of storing ASCII values for A,B,C store indexes 0,1,2. In it's own turn it means, that if we are to remove font glyphs specified in the /FontFile2 attribute, we also need to re-incode all the text that uses this font, first using the font decode it and then encode it as plain ASCII.
The hint on how to deal with it I found in https://stackoverflow.com/questions/49490199/how-to-replace-remove-text-from-a-pdf-file article: the subclass of PdfContentStreamProcessor object recreates page content, checks for each object placed in the stream, and optionally performs text replacements as needed.

After all the text re-incoded as ASCII, the /FontDescriptor object can be converted to standard font without /FontFile2 - font glyphs attribute.

## Images
The PDF coordinates are defined as 1/72 of an inch. The regular 8" page becomes something like 620 units. Most of the scanned images stored with much higher resolution. I decided internally to try to scale it at the most of the double of referenced width. In case of scanned background image, it becomes double page width. Sometimes though images are imbedded into the PDF - not a background image but rather as small inlined image. In this case I need to go through each page to determine how much space is reserved for the image and scale it down to double of that width. To get that I use the same PdfContentStreamProcessor object with IRenderListener class that saves a max width used by that image in any of the pages it's been referenced.
After I know how much space (width) the image uses on the page, I can go through each image in the /XFer node and scale it down to the double of the max size it's uses. 
Aside from scaling images down, the code also re-incodes it as 4 bits/pixel and saved as PNG stream if it's a background scanned image (takes full page) or 24rgb JPG stream if it's smaller in-place image. Also need to be mindfull of the high resolution images especially created with smart phones. Those save background off-white colors. I try to clean off-white background on those as I convert to 4bpp, otherwise a lot of that background gets converted to shades of gray and impede legibility.

### Heuristics
The current code have some very crude heuristics to check if image should be re-compressed. The original idea was have something like JPG compression quality, but I failed to find a generic solution to get it from all kind of possible image streams. So currently quality of the image is estimated 4.5% of StreamLength/NumberPixels. For the scanned images I encountered this value varies from about 6 for low-resolution monochrome scans to aboot 90 for AppleNotes. For images less then double referenced width and quality is less then 40 (or images less then 4-time width and quality less then 20) - assumption is that the images are compressed enough and no further compression will reduce the size.

## Usage
Even though application contains main() function, I use it as a class library that compresses stream as it's been submitted from the browser. If you want to try command line utility simply run it as
```
pdfshrink file-in file-out
```

## Results
The utility able to acheive on average about 60% compression on scanned PDFs and about 90% compression on text PDF.

## Problems/TODOs
- The utility should not be used if you expect a lot of non-ASCII text. Or at least font adjusting functionality should be disabled.
- Currently the code doesn't recursevely converts text in the imbedded forms. So if there is a text imbedded into the form that uses non-TrueType font, the text will come out garbled
- The code to clean image background is less then optimized. If somebody can suggest better/faster way of cleaning background - would be great.
