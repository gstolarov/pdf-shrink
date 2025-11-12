# [Shrink PDF files with C#/iTextSharp](https://github.com/gstolarov/pdf-shrink) 

## Introduction
Now days the hard-drive storage costs peanuts. However if you are storing files submitted by the user on the server, it can quite quickly add up to hundreds of gigabytes. The traditional suspect is scanned PDFs. Those contain image blobs, sometimes scanned with full color and high resolution. Sometimes, those are generated on the smart-phones with 5-8 megapixel cameras and can take as much as 3 megabytes/page. I was surprised to find though that aside from those, some of the text PDFs can also take megabytes. Apparently a lot of applications that allow to save information into PDF don't think twice about saving actual font glyphes into the PDF. So the scope of the project was:
- Try to remove font glyphs from the PDF.
- Scale down images.
  
Due to legacy code I had to support, I used iTextSharp library (https://www.nuget.org/packages/itextsharp) to manipulate PDF. The migration to the latest supported iText (https://itextpdf.com/) should not be a huge problem.


## PDF structure
The tool that helped me to navigate through the PDF was RUPS (https://itextpdf.com/products/rups). Using this tool you can visualize PDF internal structure. Overall PDF have a list of objects stored under XRef node. The objects of the interest are Stream (can be either Image or Form) and Fonts. Separately there is an Root/Catalog/Pages array. For each page in this array, used resources are references in ./Resources/XObject and/or ./Resources/Font. Those are so-called indirect references to the Fonts and Images from XRef list.

## Fonts
Surprisingly even Acrobat/PDF Optimizer (older version though) can't properly identify all the space used by the fonts. Some of the space used by fonts, Acrobat attributes to Document Overhead.
The Font object is a dictionary of font attributes, e.g. BaseFont, Encoding, SubType. Additional information can be stored in the /FontDescriptor and/or /FontDescendants object. The key element here is /FontFile2 attribute that contains a stream of glyphs. This is where most of the space is used. The idea for implementation came from the method descibed in https://github.com/QuestPDF/QuestPDF/issues/31. 

The idea is that PDF natively know how to implement 14 fonts based on Courier, TimesRoman and Helvetica fonts. Any other font should be embedded. When embedding, the font name is prefixed with a random prefix to make it unique and font glyphs are included in the stream stored in /FontFile2 attribute. So if we rename the font to exclude unique prefix and remove /FontFile2, then the renderer would just use matching system font.

That worked well as long as we use TrueType fonts. However quickly following different problems came to light:
- Sometimes font information is not stored in /FontDescriptor, but rather in /DescendantFonts[0]/FontDescriptor. The code need to be flexible to check for it.
- Font name is completely random (e.g. F1). PDF reader need to know what font to use (either build-in or system) to render the text. The solution was to use /FontDescriptor/Flags attribyte to map it to one of the PDF build-in fonts (https://pdfium.patagames.com/help/html/T_Patagames_Pdf_Enums_FontFlags.htm)
- If font encoding is non-ASCII (e.g. /Identity-H), then we need actual font information to decode the string. Once font information is lost/replaced with default, the text becomes meaningless. The solution is to go through all the text on the page, decode it using font, and then store it as ASCII string back. After that we are free to replace font with one of the build-in, ANSI, TrueType fonts.

### Text Re-encoding
Once PDF page is defined it's stored as a /Contents stream for that page and there is really not way to modify it. The hint on how to deal with it I found in https://stackoverflow.com/questions/49490199/how-to-replace-remove-text-from-a-pdf-file article. The idea is to use subclass of PdfContentStreamProcessor - same class you would use to extract text/images from PDF:
- Save page contents in a byte array
- Clear the page
- Parse previous page content one command at a time, modifying it as needed and adding it back to a blank page.

The commands we want to pay attention to are "Tj" and "TJ" - individual text and text array, and also "Do" - stream. For text command, we just going through command parameters de-coding text using currently selected font. Special attention is payed to the characters that are commonly replaced with non-ASCII values, e.g. apostrophy, quotes,... MS-Word usually uses non-ASCII values for those.

"Do" command is interesting. The stream could be either form or image. For image we want default processing to find out image size on the page (vs. internal image dimentions - see below section for images), however calling default handler for the form caused all kinds of problems, so I just ignore it, even though it creates an occasional problems (see last section on TODOs/Problems)

This processing is necessary only if page references non-standard encoding, however once I make sure that all the text in the file is using ASCII encoding, I'm free to remove any /FontFile2 glyph streams.

## Images
The PDF coordinates are defined as 1/72 of an inch. The regular 8" page becomes something like 620 units. Most of the scanned images stored with much higher resolution. I decided internally to try to scale it at the most of the double of referenced width (or 144 DPI). In case of scanned background image, it becomes double page width. 
Sometimes though images are imbedded into the PDF - not a background image but rather as small inlined image. In this case I need to go through each page to determine how much space is reserved for the image and scale it down to double of that width. To get that I use the same PdfContentStreamProcessor object with IRenderListener class that saves a max width used by that image in any of the pages it's been referenced.
After I know how much space (width) the image uses on the page, I can go through each image in the /XFer node and scale it down to the double of the max size it's uses. 
Aside from scaling images down, the code also re-incodes it as 4 bits/pixel and saves as PNG stream if it's a background scanned image (takes full page) or 24rgb JPG stream if it's smaller in-place image. Also need to be mindfull of the high resolution images especially created with smart phones. Those save background off-white colors. I try to clean off-white background on those as I convert to 4bpp, otherwise a lot of that background gets converted to shades of gray and impede legibility.

### Heuristics
Before trying to load and re-scale the images I want to know if there is a sense in doing so. My original thinking was to make sure I don't try to re-scale with loss of qualiy same JPEG files over and over. However JPEG provides no easy way to find out what is current image quality. So I thought there is 2 rules of thumb:
- compression ratio - ration of the image length to the image area - the larger this number is the more I can gain from re-scaling/re-compressing images
- image scale - ration of the image width to the viewport - if image is much larger then the viewport, even nicely compressed images can benefit from re-scaling

Obviously the final heuristics should be a factor (simple multiplication) of both. If the factor is low enough - no point in rescaling. Through multiple tests, I decided factor of those 2 parameters should be below 0.65 - feel free to tweak this number. Also If the original image length is below 1K, I do not try to re-compress/re-scale.

## Usage
Even though application contains main() function, I use it as a class library that compresses stream as it's been submitted from the browser. If you want to try command line utility simply run it as
```
pdfshrink file-in file-out
```

## Results
The utility able to acheive on average about 60% compression on scanned PDFs and about 90% compression on text PDF with embedded fonts.

## Problems/TODOs
- The utility should not be used if you expect a lot of non-ASCII text. Or at least font adjusting functionality should be disabled.
- Currently the code doesn't recursevely converts text in the imbedded forms. So if there is a text imbedded into the form that uses non-TrueType font, the text will come out garbled
- The code to clean image background is less then optimized. If somebody can suggest better/faster way of cleaning background - would be great.
