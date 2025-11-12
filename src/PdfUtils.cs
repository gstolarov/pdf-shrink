//https://github.com/dmester/pdftosvg.net
using iTextSharp.text.pdf;
using iTextSharp.text.pdf.codec;
using iTextSharp.text.pdf.parser;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.util;
using System.Windows.Media.Imaging;
using static iTextSharp.text.pdf.parser.LocationTextExtractionStrategy;

namespace WebLib {
	public class PdfUtils {
		/*
		//https://drive.google.com/file/d/0Bzkzt4QtX66pbmVtSXk0NXh2b2c/view?resourcekey=0-Nd4qeZq2hAw9mgOvNqwOjw
		public static byte[] Html2Pdf(string page) {
			byte[] ret = null;
			var css = new iTextSharp.text.html.simpleparser.StyleSheet();
			string styleSheet = "";         // add style sheets common.css, bootstrap...
			// might need work... e.g. css.LoadTagStyle("body", "font", "Bitstream Vera Sans");
			foreach (Match match in Regex.Matches(styleSheet, @"(?<selector>[^\{\s]+\w+(\s\[^\{\s]+)?)\s?\{(?<style>[^\}]*)\}"))
				css.LoadTagStyle(match.Groups["selector"].Value, "style", match.Groups["style"].Value);
			var doc = new iTextSharp.text.Document(iTextSharp.text.PageSize.A4, 10f, 10f, 10f, 0f);
			//???doc.Add(new iTextSharp.text.Header(iTextSharp.text.html.Markup.HTML_ATTR_STYLESHEET, "Style.css"));
			var parser = new iTextSharp.text.html.simpleparser.HTMLWorker(doc, null, css);
			MemoryStream mem = new MemoryStream();
			var writer = PdfWriter.GetInstance(doc, mem);
			doc.Open();
			parser.Parse(new StringReader(page));
			doc.Close();
			ret = mem.ToArray();
			return ret;
		}
		*/
		class xChunkLocStrat : ITextChunkLocationStrategy {
			class xTxtChunkLoc : TextChunkLocationDefaultImp {
				static int M = 3;
				public xTxtChunkLoc(Vector s, Vector e, float sw) : base(s, e, sw) { }
				public override bool SameLine(ITextChunkLocation oth) {
					return (OrientationMagnitude == oth.OrientationMagnitude)
						? Math.Abs(DistPerpendicular - oth.DistPerpendicular) < M : false;
				}
				public override int CompareTo(ITextChunkLocation oth) {
					if (this == oth) return 0;
					int num = OrientationMagnitude - oth.OrientationMagnitude;
					if (Math.Abs(num) >= M) return num;
					num = DistPerpendicular - oth.DistPerpendicular;
					if (Math.Abs(num) >= M) return num;
					return (int)(DistParallelStart - oth.DistParallelStart);
				}
			}
			public ITextChunkLocation CreateLocation(TextRenderInfo ri, LineSegment bl) {
				return new xTxtChunkLoc(bl.GetStartPoint(), bl.GetEndPoint(), ri.GetSingleSpaceWidth());
			}
		}
		// extract text from pdf. Assume if avg(pageSize) > 60k - scan
		// pgSz < 60k - use SimpleTextExtractionStrategy - dumps text as it comes in the PDF file.
		//			Potential problem with OCR, when text might be somewhat out of order
		// pgSz > 60K - use LocationTextExtractionStrategy - order text by x/y. Screws up when
		//			multiple columns, but not sensitive when OCR slightly vary Y coord
		public static string GetText(byte[] src) {
			if (src == null || src.Length == 0) return "";
			try {
				PdfReader rdr = new PdfReader(src);
				int nmpg = rdr.NumberOfPages;
				return PdfUtils.CleanText(Enumerable.Range(1, nmpg).GetString(i => {
					ITextExtractionStrategy strat = (src.Length < 60000 * nmpg)
							? new SimpleTextExtractionStrategy() as ITextExtractionStrategy
							: new LocationTextExtractionStrategy(new xChunkLocStrat()) as ITextExtractionStrategy;
					return PdfTextExtractor.GetTextFromPage(rdr, i, strat).Trim();
				}, "\f"));
			}
			catch (Exception ex) {
				Utils.Log(ex);
				return "";
			}
		}
		public static string CleanText(string txt) {
			// replace unicode chars with ASCII equivalents.
			var cr = new Dictionary<string,string> { { "’", "'" }, { "•", "-" }, { "“", "\"" }, { "”", "\"" } };
			cr.ForEach(x => txt = txt.Replace(x.Key, x.Value));
			return txt.Split(new char[] { '\f' }).GetString(y => {
				// lines that ends with the word char, comma or ( joint together into paragraphs.
				string rpl = y.reReplace(@"([\w,(])[ ]*\n(\w)", "$1 $2")
							  .reReplace(@"[ \t]+\n", "\n").Trim();     // trim EOL blanks 
				return rpl;
			}, "\n\f\n");
		}
		public static Stream Compress(Stream src) {
			var rs = new PdfCompress().Shrink(src);
			//rs = DelDups(rs);			// does a little better job then rdr.RemoveUnusedObjects
			return rs;
		}

		static PdfDictionary GetDict0(PdfDictionary o, PdfName n) {
			if (o == null) return null;
			var d = o.GetAsDict(n);
			if (d != null) return d;
			var a = o.GetAsArray(n) ?? new PdfArray();
			if (a.Count() == 0) return null;
			return PdfReader.GetPdfObject(a[0]) as PdfDictionary;
		}
		// alpha = false -> load just image w/o mask/transparency
		// alpha = true -> load image with mask as transparent areas.
		static Image Dict2Img(PdfDictionary dict, bool fAlpha = true, ImageRenderInfo ri = null) {
			var			dSMask	= PdfReader.GetPdfObject(dict.Get(PdfName.SMASK)) as PdfDictionary;
			var         dMask	= PdfReader.GetPdfObject(dict.Get(PdfName.MASK)) as PdfDictionary;
			var			decPrm	= GetDict0(dict, PdfName.DECODEPARMS);
			int			width	= dict.GetAsNumber(PdfName.WIDTH).IntValue,
						height	= dict.GetAsNumber(PdfName.HEIGHT).IntValue;
			Image		mask = null, wi = null;
			PRStream	prs		= dict as PRStream;
			PdfObject	tpo		= dict.Get(PdfName.FILTER);
			PdfArray	fltrArr = tpo != null ? ((tpo as PdfArray) ?? new PdfArray { tpo })
											  : new PdfArray();
			int			bpc		= dict.GetAsNumber(PdfName.BITSPERCOMPONENT).IntValue;
			var			clrSpc	= dict.GetAsArray(PdfName.COLORSPACE);
			tpo = clrSpc==null || clrSpc.Count()<2 ? null : PdfReader.GetPdfObject(clrSpc[1]);
			var tpClr = clrSpc == null || clrSpc.Count() < 2 ? null
					: (tpo as PdfName) ?? (tpo as PdfArray)?[0];
			bool chkPalette = (tpClr != null && (bpc == 1 || bpc == 4 || bpc == 8) 
						&& clrSpc.Count() >= 4 && PdfName.INDEXED.Equals(clrSpc[0])
						&& (PdfName.CALRGB.Equals(tpClr) || PdfName.DEVICERGB.Equals(tpClr) || PdfName.DEVICECMYK.Equals(tpClr)));
			byte[] pltClrs = getPallete(); 
			if (fAlpha && (dMask ?? dSMask) != null)
				try { mask = Dict2Img(dSMask ?? dMask, false, null); }
				catch (Exception) { throw; };
			if (fltrArr.Any(x => x.Equals(PdfName.JPXDECODE))) 
				wi = new XPdf.XPdfJpx(PdfReader.GetStreamBytesRaw(prs)).DecodeImage(pltClrs);
			if (fltrArr.Any(x => x.Equals(PdfName.JBIG2DECODE))) {
				var glob = dict.GetAsDict(PdfName.DECODEPARMS)?.GetAsStream(new PdfName("JBIG2Globals")) as PRStream;
				wi = new XPdf.PBoxJBig2(PdfReader.GetStreamBytesRaw(prs),
						 glob == null ? null : PdfReader.GetStreamBytesRaw(glob)).DecodeImage();
			}
			else if (bpc == 1 && fltrArr.Any(x => x.Equals(PdfName.CCITTFAXDECODE)))
				wi = imgLoadTiff();
			if (wi == null) {
				try {
					wi = (prs == null ? ri?.GetImage() : new PdfImageObject(prs)).GetDrawingImage();
				}
				catch (Exception ex) {
					if (chkPalette)
						wi = imgLoadPng();
					else
						throw new Exception("Failed to load Image " + DumpPdfObj(dict), ex);
				}
			}
			int Format32bppCmyk = 8207;                 // handle CMYK images 
			if ((int)wi.PixelFormat == Format32bppCmyk && ImageFormat.Jpeg.Equals(wi.RawFormat))
				wi = imgLoadCmykJpg();
			if (fAlpha && mask != null && wi != mask) {
				if (clrSpc != null && clrSpc.Count() == 4
				&& PdfName.INDEXED.Equals(clrSpc[0]) && PdfName.ICCBASED.Equals(tpClr)
				&& GetDict0(dSMask, PdfName.DECODEPARMS)?.Get(PdfName.COLORS).GetInt() == 1) {
					mask = mask.PixFrmt(PixelFormat.Format1bppIndexed).Invert();
					wi = wi.Invert();
				}
				wi = ApplyMask(wi, mask, dSMask == null);
			}
			else if (fAlpha && mask == null && dict.Get(PdfName.IMAGEMASK).GetString() == "true")
				wi = ApplyMask(wi, wi, true);
			//if (Image.IsAlphaPixelFormat(wi.PixelFormat)) {		// if alpha channel
			//	Bitmap fm = new Bitmap(wi);							// find transp pixels
			//	var mb = fm.LockBits(new Rectangle(Point.Empty, wi.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppRgb);
			//	var ma = new byte[mb.Height * mb.Stride]; Marshal.Copy(mb.Scan0, ma, 0, ma.Length);
			//	fm.UnlockBits(mb);
			//	int i = 3;
			//	for (; i < ma.Length; i += 4)
			//		if (ma[i] != 255)
			//			break;
			//	if (i >= ma.Length)									// no transp pixels
			//		wi = ScaleImage(wi, wi.Width, wi.Height, Color.White);	// cvt to 24bit.
			//}
			return wi;
			/////////////////////////////////////////////////////////////////////////////
			byte[] getPallete() {
				if (!chkPalette) return null;
				var pltObj = clrSpc.GetDirectObject(3);
				byte[] pltArr = (pltObj as PdfString)?.GetBytes()
							 ?? ((pltObj is PRStream p2) ? PdfReader.GetStreamBytes(p2) : null);
				if (pltArr == null || !PdfName.DEVICECMYK.Equals(tpClr))
					return pltArr;
				byte[] np = new byte[pltArr.Length * 3 / 4];
				for (int i = 0, j = 0; i < pltArr.Length; i += 4, j += 3) {
					float c = pltArr[i + 0], m = pltArr[i + 1],
							y = pltArr[i + 2], k = 255.0f - pltArr[i + 3];
					np[j + 0] = (byte)((1.0f - c / 255.0f) * k);  // might need to swap 
					np[j + 1] = (byte)((1.0f - m / 255.0f) * k);  // R <--> B
					np[j + 2] = (byte)((1.0f - y / 255.0f) * k);
				}
				return np;
			}
			Image imgLoadPng() {
				MemoryStream ms = new MemoryStream();
				var png = new PngWriter(ms);
				png.WriteHeader(width, height, bpc, 3);
				if (pltClrs != null) 
					png.WritePalette(pltClrs);
				png.WriteData(PdfReader.GetStreamBytes((PRStream)dict), (width * bpc + 7) / 8);
				png.WriteEnd();
				return Image.FromStream(ms);
			}
			Image imgLoadCmykJpg() {
				byte[] pd = new byte[height * width * 4];
				var rect = new System.Windows.Int32Rect(0, 0, width, height);
				var decoder = new JpegBitmapDecoder(
					new MemoryStream(PdfReader.GetStreamBytesRaw((PRStream)dict)),
					BitmapCreateOptions.PreservePixelFormat, BitmapCacheOption.Default);
				decoder.Frames[0].CopyPixels(rect, pd, width * 4, 0);
				for (int i = 0; i < pd.Length; i += 4)
					(pd[i], pd[i + 2]) = (pd[i + 2], pd[i]);
					//Utils.Swap(ref pd[i], ref pd[i + 2]);
				Bitmap bmp = new Bitmap(width, height);
				var data = bmp.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
				Marshal.Copy(pd, 0, data.Scan0, pd.Length);
				bmp.UnlockBits(data);					// might need to work on w/b transparency
				return ScaleImage(bmp, width, height, Color.Black);       // 32bpp -> 24bpp
			}
			Image imgLoadTiff() {
				var prmK = decPrm.GetAsNumber(PdfName.K).GetInt();
				var prmBlk1 = (decPrm.GetAsBoolean(PdfName.BLACKIS1) ?? PdfBoolean.PDFFALSE).BooleanValue;
				List<byte> raw = new List<byte>(PdfReader.GetStreamBytesRaw((PRStream)dict));
				if ((raw.Count % 2) != 0) raw.Add(0);
				MemoryStream ms = new MemoryStream();
				TiffWriter wr = new TiffWriter();
				wr.AddField(new TiffWriter.FieldShort(TIFFConstants.TIFFTAG_IMAGEWIDTH, width));
				wr.AddField(new TiffWriter.FieldShort(TIFFConstants.TIFFTAG_IMAGELENGTH, height));
				wr.AddField(new TiffWriter.FieldShort(TIFFConstants.TIFFTAG_COMPRESSION, prmK == -1 ? 4 : 3));
				wr.AddField(new TiffWriter.FieldShort(TIFFConstants.TIFFTAG_PHOTOMETRIC, prmBlk1 ? 1 : 0));
				wr.AddField(new TiffWriter.FieldLong(TIFFConstants.TIFFTAG_STRIPBYTECOUNTS, raw.Count));
				wr.AddField(new TiffWriter.FieldImage(raw.ToArray()));
				wr.WriteFile(ms);
				//ms.WriteBytes(new byte[] { 0x49, 0x49, 0x2a, 0 },
				//			BitConverter.GetBytes(raw.Count + 16),
				//			new byte[] { 0,0,0,0, 0,0,0,0 }, raw, new byte[] { 7, 0 });
				//int[] ctrl = new int[] {
				//		0x40100,1,wi.Width, 0x40101,1,wi.Height, 0x40117,1,raw.Count,
				//		0x40103,1,prmK == -1 ? 4 : 3,		// COMPRESSION
				//		0x40106,1,prmBlk1 ? 1 : 0,			// PHOTOMETRIC
				//		0x40102,1,1, 0x40111,1,16			// BITS/STRIPOFFSETS
				//	};
				//ctrl.ForEach(x => ms.WriteBytes(BitConverter.GetBytes(x)));
				return Bitmap.FromStream(ms);
			}
			Image ApplyMask(Image src, Image msk, bool inv) {
				int w = Math.Max(src.Width, msk.Width), h = Math.Max(src.Height, msk.Height);
				double scale = Math.Max(w, h) / 4096.0;				// make sure images are under 4k
				if (scale > 1) (w, h) = ((int)(w / scale), (int)(h / scale));
				if (src.Width < w || src.Height < h || scale > 1)	// resize mask to be same size
					src = ScaleImage(src, w, h, Color.Black);
				if (msk.Width < w || msk.Height < h || scale > 1)	// resize mask to be same size
					msk = ScaleImage(msk, w, h, Color.Black);
				var bmp = new Bitmap(src).Clone(new Rectangle(0, 0, w, h), PixelFormat.Format32bppArgb);
				var rb = bmp.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
				var ra = new byte[rb.Height * rb.Stride]; Marshal.Copy(rb.Scan0, ra, 0, ra.Length);
				Bitmap fm = new Bitmap(msk);
				var mb = fm.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
				var ma = new byte[mb.Height * mb.Stride]; Marshal.Copy(mb.Scan0, ma, 0, ma.Length);
				bool trans = false;
				for (int y = 0; y < h - 1; y++)
					for (int x = 0, sa = y * rb.Stride, sm = y * mb.Stride; x < w; x++, sa += 4, sm += 3) {
						//float r = 1.0f*(ma[y*mb.Stride+x*3])/255;	// mask ratio: 0-black; 1-white;
						//byte c = ra[sa+0];	ra[sa+0] = (byte)(c+r*(255-c));
						//c = ra[sa+1];		ra[sa+1] = (byte)(c+r*(255-c));
						//c = ra[sa+2];		ra[sa+2] = (byte)(c+r*(255-c));
						ra[sa + 3] = (byte)(inv ? 255 - ma[sm] : ma[sm]);
						trans = trans || (ra[sa + 3] != 255);
					}
				Marshal.Copy(ra, 0, rb.Scan0, ra.Length);
				bmp.UnlockBits(rb);
				fm.UnlockBits(mb);
				//if (dMask != null) bmp.MakeTransparent(Color.White);
				src = bmp;
				if (!trans)
					src = ScaleImage(src, w, h, Color.White);
				return src;
			}
		}

		static Image ScaleImage(Image s, int w, int h, Color c) {
			var rsi = new Bitmap(w, h, 
					c == Color.Transparent ? s.PixelFormat : PixelFormat.Format24bppRgb);
			using (Graphics g = Graphics.FromImage(rsi)) {
				if (c != Color.Transparent) g.Clear(c);
				g.DrawImage(s, 0, 0, w, h);
			}
			return rsi;
		}
		public class PdfRenderer : IExtRenderListener {
			public virtual void RenderText(TextRenderInfo renderInfo) { }
			public virtual void BeginTextBlock() { }
			public virtual void ClipPath(int rule) { }
			public virtual void EndTextBlock() { }
			public virtual void ModifyPath(PathConstructionRenderInfo renderInfo) { }
			public virtual void RenderImage(ImageRenderInfo renderInfo) { }
			public virtual iTextSharp.text.pdf.parser.Path RenderPath(PathPaintingRenderInfo renderInfo) {
				return null;
			}
		}
		class PgRenderListener : PdfRenderer { //  IRenderListener 
			internal List<String>	errs = null;
			internal PdfContentStreamProcessor proc = null;
			internal Graphics		g;
			internal Bitmap			bmp;
			//internal PdfDictionary	clrs;
			internal iTextSharp.text.Rectangle sz;
			internal bool			fTxt;
			internal int			imgCnt = 0;
			internal PdfDictionary	res;
			Dictionary<string,string>	fntMap = new Dictionary<string, string>();
			iTextSharp.text.pdf.parser.Path path = new iTextSharp.text.pdf.parser.Path();
			String MapFont(string nmFnt) {
				if (fntMap.ContainsKey(nmFnt))
					return fntMap[nmFnt];
				var fonts = res.GetAsDict(PdfName.FONT);
				var dct = fonts?.Keys.Select(x => fonts.GetAsDict(x))
					.FirstOrDefault(x => x.GetAsName(PdfName.BASEFONT).GetString() == "/" + nmFnt);
				dct = GetDict0(dct, PdfName.DESCENDANTFONTS) ?? dct;
				PdfDictionary dsc = dct?.GetAsDict(PdfName.FONTDESCRIPTOR);
				int flg = (dsc == null) ? 0 : dsc.GetAsNumber(PdfName.FLAGS).GetInt();
				FontStyle sf = FontStyle.Regular;
				if ((flg & 0x00040) != 0 || nmFnt.Contains("Italic")) sf |= FontStyle.Italic;
				if ((flg & 0x40000) != 0 || nmFnt.Contains("Bold"))   sf |= FontStyle.Bold;
				string nm = "Arial";
				if ((flg & 1) != 0 || nmFnt.Contains("Courier")) nm = "Courier New";
				if ((flg & 2) != 0 || nmFnt.Contains("Times"))   nm = "Times New Roman";
				if (nmFnt.Contains("Trebuchet"))		nm = "Trebuchet MS";
				else if (nmFnt.Contains("Wingdings"))	nm = "Wingdings";
				else if (nmFnt.Contains("Calibri"))		nm = "Calibri";
				else if (nmFnt.Contains("Tahoma"))		nm = "Tahoma";
				else if (nmFnt.Contains("Impact"))		nm = "Impact";
				return fntMap[nmFnt] = sf.GetInt().GetString() + "~" + nm;
			}
			public override void RenderText(TextRenderInfo ri) {
				string txt = ri.GetText();
				if (!fTxt || ri == null || txt.Trim().Length == 0)
					return;
				try {
					// CurGStateName is an ExtGState name entry (see SetColorSpace)
					var curGs = GetProcRes(proc, null, new PdfName("CurGStateName")) as PdfName;
					var transp = curGs == null ? null
							: (GetProcRes(proc, PdfName.EXTGSTATE, curGs) as PdfDictionary)?.GetAsNumber(PdfName.CA);
					if (transp != null && transp.IntValue == 0)		// CA == 0 -> transparent text...
						return;										// OCR text ignore
					var bLine = ri.GetBaseline();
					var vec = bLine.GetEndPoint().Subtract(bLine.GetStartPoint());
					if (vec.Length == 0) vec = new Vector(1f, 0f, 0f);
					int a = (int)(Math.Atan2(vec[1], vec[0]) * 180 / Math.PI);
					bool hor = ((a % 180) == 0);
					RectangleJ bl = ri.GetBaseline().GetBoundingRectange(),
							   al = ri.GetAscentLine().GetBoundingRectange();
					var bf = ri.GetFont();
					float size = Math.Abs(hor ? al.Y - bl.Y : al.X - bl.X),
						tw = bf.GetWidth(txt)*0.001f, span = (hor ? bl.Width : bl.Height);
					if (tw * size > span) size = span / tw; // find right font size
					if (size < 1) return;
					var fnm = MapFont(bf.PostscriptFontName).Split(new char[] { '~' });
					var fnt = new Font(fnm.Last(), size, (FontStyle)fnm.First().GetInt());
					var gs	= Utils.GetPrivVal<GraphicsState>(ri, "gs");
					var clr = new SolidBrush(Color.FromArgb(gs.StrokeColor?.RGB
										?? gs.FillColor?.RGB ?? Color.Black.ToArgb()));
					var sm = g.Transform;
					ri.GetCharacterRenderInfos().ForEach(x => {
						var xbl = x.GetBaseline().GetBoundingRectange();
						float sx = hor ? xbl.X : xbl.X-size+1, 
							  sy = (xbl.Y > 0 ? sz.Height : 0) - xbl.Y + (hor ? -size : size) - 2;
						g.TranslateTransform(sx, sy);
						g.RotateTransform(-a);
						g.DrawString(x.GetText(), fnt, clr, 0, 0);
						g.Transform = sm;
					});
				}
				catch (Exception ex) {
					Utils.Log(ex);
				}
			}
			public override void RenderImage(ImageRenderInfo ri) {
				try {
					var dict = PdfReader.GetPdfObject(ri.GetRef()) as PdfDictionary
						?? Utils.GetPrivVal<InlineImageInfo>(ri, "inlineImageInfo")?.ImageDictionary;
					var wi = Dict2Img(dict, true, ri);
					if (wi == null) return;
					var ctm = ri.GetImageCTM();
					//i0 and i3: Scaling in the x and y directions
					//i1 and i2: Rotation and skewing
					//i4 and i5: Translation(movement) in x and y directions                   
					Vector tl = new Vector(0,0,1).Cross(ctm), br = new Vector(1, 1, 1).Cross(ctm);
					float	x = Math.Min(tl[0], br[0]), w = Math.Max(tl[0], br[0]) - x,
							y = sz.Height - Math.Max(tl[1], br[1]),
							h = sz.Height - Math.Min(tl[1], br[1]) - y;
					if (tl[0] < br[0] && tl[1] < br[1]) 
						{ ; }
					else if (tl[0] < br[0] && tl[1] > br[1])
						wi.RotateFlip(RotateFlipType.Rotate270FlipXY);
					else if (tl[0] > br[0] && tl[1] < br[1])
						wi.RotateFlip(RotateFlipType.Rotate90FlipXY);
					else
						wi.RotateFlip(RotateFlipType.Rotate180FlipNone);
					g.DrawImage(wi, x, y, w, h);
					imgCnt++;
				}
				catch(Exception ex) {
					if (!(ex is MessageException))
						if (errs != null) errs.Add(ex.Message);
						else Utils.Log(ex);
				}
			}
			public override void ModifyPath(PathConstructionRenderInfo ri) {
				if (!fTxt) return;
				var pos = ri.SegmentData?.ToList();
				if (ri.Operation == PathConstructionRenderInfo.MOVETO)
					path.MoveTo(pos[0], pos[1]);
				else if (ri.Operation == PathConstructionRenderInfo.CLOSE)
					path.CloseSubpath();
				else if (ri.Operation == PathConstructionRenderInfo.CURVE_123)
					path.CurveTo(pos[0], pos[1], pos[2], pos[3], pos[4], pos[5]);
				else if (ri.Operation == PathConstructionRenderInfo.CURVE_13)
					path.CurveFromTo(pos[0], pos[1], pos[2], pos[3]);
				else if (ri.Operation == PathConstructionRenderInfo.CURVE_23)
					path.CurveTo(pos[0], pos[1], pos[2], pos[3]);
				else if (ri.Operation == PathConstructionRenderInfo.LINETO)
					path.LineTo(pos[0], pos[1]);
				else if (ri.Operation == PathConstructionRenderInfo.RECT)
					path.Rectangle(pos[0], pos[1], pos[2], pos[3]);
			}
			//https://stackoverflow.com/questions/51953098/itextsharp-get-reference-to-a-graphic-markup
			//https://stackoverflow.com/questions/44867468/itextsharp-how-to-find-the-fill-color-of-a-rectangle
			//http://www.java2s.com/example/java-src/pkg/mkl/testarea/itext5/pdfcleanup/pdfcleanuprenderlistener-3b7d7.html
			public override iTextSharp.text.pdf.parser.Path RenderPath(PathPaintingRenderInfo ri) {
				if (path.Subpaths.Count() == 0) // !fTxt || 
					return null;
				var gs = Utils.GetPrivVal<GraphicsState>(ri, "gs");
				path.ReplaceCloseWithLine();
				System.Drawing.Drawing2D.GraphicsPath gp = null;
				path.Subpaths.ForEach(sp => {
					List<iTextSharp.awt.geom.Point2D> pp = new List<iTextSharp.awt.geom.Point2D>();
					sp.GetSegments().ForEach(sh => pp.AddRange((sh is BezierCurve sc)
							? sc.GetPiecewiseLinearApproximation() : sh.GetBasePoints()));
					var pp1 = pp.Select(p => new Vector((float)p.GetX(), (float)p.GetY(), 1).Cross(ri.Ctm))
								.Select(f => new PointF(f[0], f[1] < 0 ? -f[1] : sz.Height - f[1])).ToArray();
					if (pp1.Count() > 1) {
						gp = gp ?? new System.Drawing.Drawing2D.GraphicsPath();
						var tp = new System.Drawing.Drawing2D.GraphicsPath();
						if (pp1.Count() == 2)
							tp.AddLine(pp1[0], pp1[1]);
						else
							tp.AddPolygon(pp1);
						gp.AddPath(tp, true);
					}
				});
				if (gp != null) {
					// TODO: iTextSharp bug looses 1 byte color index to colorSpace - using Gray for now.
					SolidBrush br = new SolidBrush(Color.FromArgb(gs.FillColor?.RGB
							?? (gs.ColorSpaceFill == null ? Color.Transparent : Color.Gray).ToArgb()));
					Pen pen = new Pen(Color.FromArgb(gs.StrokeColor?.RGB ?? Color.Black.ToArgb()), gs.LineWidth);
					if (0 != (ri.Operation & PathPaintingRenderInfo.FILL) && br.Color != Color.Transparent)
						g.FillPath(br, gp);
					if (0 != (ri.Operation & PathPaintingRenderInfo.STROKE) && gs.StrokeColor != null)
						g.DrawPath(pen, gp);
				}
				path.Subpaths.Clear();
				return path;
			}
			public override void ClipPath(int rule) {
				// TODO : g.Clip = ...
				path.Subpaths.Clear();
			}
		}
		static PdfObject GetProcRes(PdfContentStreamProcessor proc, PdfName sub, PdfName nm) {
			var d = Utils.GetPrivVal<PdfDictionary>(proc, "resources");
			if (d != null && sub != null)
				d = d.GetAsDict(sub);
			if (d != null && nm != null)
				return PdfReader.GetPdfObject(d.Get(nm));
			return d;
		}
		static String DumpPdfObj(PdfObject obj) {
			if (obj == null) return "<null>";
			if (obj is PdfArray a)
				return "PdfArray(" + a.Size + ") ["
					+ a.Take(10).GetString(x => DumpPdfObj(PdfReader.GetPdfObject(x)), "; ") + "], ";
			if (obj is PdfDictionary d)
				return "PdfDictionary [" + d.Keys.GetString(x => x.ToString() + "="
								+ DumpPdfObj(PdfReader.GetPdfObject(d.Get(x))), "; ") + "], ";
			return obj.ToString();
		}

		// iTextSharp bug prevents to process Colorspace colors unless it's /Device...
		// TODO: /Separated, /ICC, ...
		class SetColorSpace : IContentOperator {
			internal IContentOperator orig = null;
			public void Invoke(PdfContentStreamProcessor proc, PdfLiteral oper, List<PdfObject> prms) {
				if (oper.ToString() == "gs") {						// not the best to do it 
					var res = GetProcRes(proc, null, null) as PdfDictionary; // but min code/effort
					res?.Put(new PdfName("CurGStateName"), prms[0]);// stores ExtGState name in the 
					orig.Invoke(proc, oper, prms);                  // current Resource dict to be 
					return;                                         // used for printing text 
				}                                                   // to determine transparency (OCR)
				bool stroke = char.IsUpper(oper.ToString()[0]);
				var gs	= proc.Gs();
				var csn = stroke ? gs.ColorSpaceStroke : gs.ColorSpaceFill;
				var cs	= GetProcRes(proc, PdfName.COLORSPACE, csn) as PdfArray ?? new PdfArray();
				var dev = cs.FirstOrDefault(x => x.IsName() && x.ToString().StartsWith("/Device")) as PdfName;
				if (csn.ToString().StartsWith("/Device") || !prms[0].IsNumber() || cs.Count() < 4 || dev == null) {
					orig.Invoke(proc, oper, prms);
					return;
				}
				PdfObject objPlt = cs.GetDirectObject(3);
				byte[] plt = (objPlt as PdfString)?.GetBytes()
							?? ((objPlt is PRStream p2) ? PdfReader.GetStreamBytes(p2) : null);
				int idx = prms[0].GetInt(), bpc = 0;
				string nop = "";
				if (dev == PdfName.DEVICEGRAY)		(nop, bpc) = ("g",  1);
				else if (dev == PdfName.DEVICERGB)	(nop, bpc) = ("rg", 3);
				else if (dev == PdfName.DEVICECMYK) (nop, bpc) = ("k",  4);
				if (bpc == 0 || plt == null || plt.Length < idx * bpc + bpc) { // oopsie....
					orig.Invoke(proc, oper, prms);
					return;
				}
				var nprm = new List<PdfObject>();               // let default impl 
				for (int j = 0; j < bpc; j++)                   // for right oper handle clr set
					nprm.Add(new PdfNumber(plt[idx * bpc + j]));// populate color info
				if (stroke) nop = nop.ToUpper();				// fill -0 low case, stroke - upr
				var oo = proc.RegisterContentOperator(nop, null);// get original operator
				proc.RegisterContentOperator(nop, oo);			// and restore oper tbl
				oo.Invoke(proc, null, nprm);					// let orig do the work
			}
		}

		public static Image GetPageImage(PdfReader rdr, int pg, float zoom,
				bool fTxt = false, List<string> err = null) {
			var sz = rdr.GetPageSize(pg);
			Bitmap bmp = new Bitmap((int)(zoom * sz.Width), (int)(zoom * sz.Height), PixelFormat.Format24bppRgb);
			PdfDictionary res = rdr.GetPageN(pg).GetAsDict(PdfName.RESOURCES) ?? new PdfDictionary();
			var lstnr = new PgRenderListener { sz = sz, fTxt = fTxt, errs = err, res = res, bmp = bmp };
			PdfContentStreamProcessor proc = new PdfContentStreamProcessor(lstnr);
			lstnr.proc = proc;
			foreach (var o in new string[] { "scn", "SCN", "gs" }) {
				var co = new SetColorSpace();
				co.orig = proc.RegisterContentOperator(o, co);
			}
			using (Graphics g = Graphics.FromImage(bmp)) {
				g.Clear(Color.White);
				g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
				g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic;
				var tm = new System.Drawing.Drawing2D.Matrix();
				tm.Scale(zoom, zoom);
				(lstnr.g = g).Transform = tm;
				proc.ProcessContent(ContentByteUtils.GetContentBytesForPage(rdr, pg), res);
			}
			var rot = rdr.GetPageRotation(pg);
			var ra = new RotateFlipType[] { 0, RotateFlipType.Rotate270FlipXY,
								RotateFlipType.Rotate180FlipXY, RotateFlipType.Rotate90FlipXY };
			if (rot != 0) bmp.RotateFlip(ra[rot / 90]);
			if (!fTxt && lstnr.imgCnt == 0)
				return null;
			return bmp;
		}

		// A dummy render listener to give to the underlying content stream processor to feed events to
		//https://stackoverflow.com/questions/49490199/how-to-replace-remove-text-from-a-pdf-file
		// convert text from font-specific encoding to ASCII, so the fonts can be discarded.
		class PdfCompress {
			Dictionary<PdfDictionary, int> imgRefs = new Dictionary<PdfDictionary, int>();
			class CtxOperWrap : IContentOperator {
				static Dictionary<char, char> specChars = new Dictionary<char, char> {
					{ (char) 381, 'Z' }, { (char)8211, '-' }, { (char)8217, '`' },
					{ (char)8220, '"' }, { (char)8221, '"' }, { (char)8230, '.' },
					{ (char)8239, ' ' }
				};
				internal IContentOperator orig = null;
				PdfString StrCvt(PdfContentStreamProcessor proc, PdfString i) {
					//if (proc.Gs().Font.FontDictionary.Get(PdfName.SUBTYPE) == PdfName.TRUETYPE)
					//	return i;
					byte[] bytes = i.GetBytes();
					string str = proc.Gs().Font.Decode(bytes, 0, bytes.Length);
					str = new string(str.ToCharArray()
						.Select(x => specChars.ContainsKey(x) ? specChars[x] : x).ToArray());
					//str.Where(x => x > 255).ForEach(x => Console.WriteLine("{0} {1}", (int)x, x));
					(proc.RenderListener as ImgRendListener).mod |= (i.ToString() != str);
					return new PdfString(str);
				}
				void ShowTextArray(PdfContentStreamProcessor proc, PdfArray prms) {
					for (int i = 0; i < prms.Count(); i++)
						if (prms[i] is PdfString s)
							prms[i] = StrCvt(proc, s);
				}
				void ShowText(PdfContentStreamProcessor proc, List<PdfObject> prms) {
					prms[0] = StrCvt(proc, prms[0] as PdfString);
				}
				void ChkDoCmd(PdfContentStreamProcessor proc, PdfLiteral oper, List<PdfObject> prms) {
					PdfDictionary xObj = GetProcRes(proc, PdfName.XOBJECT, prms[0] as PdfName)
									as PdfDictionary ?? new PdfDictionary();
					if (xObj.GetAsName(PdfName.SUBTYPE).GetString() == "/Image")
						orig.Invoke(proc, oper, prms);
				}
				public void Invoke(PdfContentStreamProcessor proc, PdfLiteral oper, List<PdfObject> prms) {
					string op = oper.ToString();
					if (op != "Do")                     // calling for "Do/Form" messes up form data and positioning
						orig.Invoke(proc, oper, prms);  // but we need to call it for "Do/Image" to save image dims
					if (op == "TJ")
						ShowTextArray(proc, prms[0] as PdfArray);
					else if (op == "Tj")
						ShowText(proc, prms);
					else if (op == "Do")                // call def for Do/Image. Not processing Do/Form skips images
						ChkDoCmd(proc, oper, prms);     // imbedded in the form - TODO.
					//else Console.WriteLine(op);
					ImgRendListener lstnr = proc.RenderListener as ImgRendListener;
					int i = 0;
					foreach (var p in prms) {
						p.ToPdf(null, lstnr.Canvas.InternalBuffer);
						lstnr.Canvas.InternalBuffer.Append(prms.Count > ++i ? (byte)' ' : (byte)'\n');
					}
				}
			}
			class ImgRendListener : PdfRenderer {
				internal HashSet<PdfDictionary> procImg = new HashSet<PdfDictionary>();
				internal PdfContentByte Canvas = null;
				internal int	pgWidth;
				internal bool	mod = false;
				internal PdfStamper stmp = null;
				public override void RenderImage(ImageRenderInfo ri) {
					try {
						var dict = PdfReader.GetPdfObject(ri.GetRef()) as PdfDictionary;
						// ?? Utils.GetPrivVal<InlineImageInfo>(ri, "inlineImageInfo")?.ImageDictionary;
						if (dict == null || !dict.Get(PdfName.SUBTYPE).Equals(PdfName.IMAGE))
							return;
						if (procImg.Contains(dict))
							return;
						procImg.Add(dict);
						PdfObject fltr = dict.Get(PdfName.FILTER);
						if (fltr == null) return;
						PdfArray fa = (fltr as PdfArray) ?? new PdfArray(fltr);
						if (!fa.OfType<PdfObject>().Any(x => x.Equals(PdfName.DCTDECODE)
						|| x.Equals(PdfName.CCITTFAXDECODE) || x.Equals(PdfName.FLATEDECODE)
						|| x.Equals(PdfName.JPXDECODE)))
							return;
						//var msk = PdfReader.GetPdfObject(dict.Get(PdfName.SMASK)) as PRStream
						//	   ?? PdfReader.GetPdfObject(dict.Get(PdfName.MASK)) as PRStream;
						bool isMask = dict.Get(PdfName.IMAGEMASK).GetString() == "true";
						//if (isMask)		return false;
						//if (msk != null)	return false;
						var ctm = ri.GetImageCTM();
						Vector tl = new Vector(0, 0, 1).Cross(ctm), br = new Vector(1, 1, 1).Cross(ctm);
						int mxw = Math.Min(Math.Max(1, (int)Math.Abs(tl[0] - br[0])), Math.Min(pgWidth, 792));
						int oldLen = dict.Get(PdfName.LENGTH).GetInt();
						decimal oiw = dict.Get(PdfName.WIDTH).GetNum(), oih = dict.Get(PdfName.HEIGHT).GetNum();
						decimal scale = oiw / mxw,                  // ratio width of img over viewport
								rZip = oldLen * 100m / oiw / oih;   // ratio of arr width over num of pxls
						if (scale * rZip < 0.65m || oldLen < 1000)
							return;
						Image wi = Dict2Img(dict, !isMask, null);
						byte[] newBytes = null;
						if (dict.Get(PdfName.BITSPERCOMPONENT).GetInt() != 1) {
							wi = wi.ScaleDown(mxw * 2, 0);					// image at the most 2 rend rect
							if (Image.IsAlphaPixelFormat(wi.PixelFormat))	// only PNG has transparency
								newBytes = wi.GetBytes(ImageFormat.Png);
							else if (mxw >= pgWidth)						// whole page img - likely scan
								newBytes = wi.BgClean().GetBytes(ImageFormat.Png);	
							else											// part page - preserve clrs.
								newBytes = wi.PixFrmt(PixelFormat.Format24bppRgb).GetBytes(ImageFormat.Jpeg);
						}
						else {                                  // scaledown converts 1bpp to full color. 
							if (wi.Width >= mxw * 2)            // Converting back to 1bpp looses resolution. 
								wi = wi.ScaleDown(mxw * 2, 0).PixFrmt(PixelFormat.Format4bppIndexed);
							newBytes = wi.GetBytes(ImageFormat.Png);    // Keep it as 4bpp.
						}
						if (newBytes == null || newBytes.Length > oldLen * 0.8)
							return;
						var imgZip = iTextSharp.text.Image.GetInstance(newBytes);
						if (isMask) 
							imgZip.MakeMask();
						var sMask = dict.GetDirectObject(PdfName.SMASK) as PdfDictionary;
						if (imgZip.ImageMask == null) sMask = null;
						if (MakeImg(dict, sMask, imgZip) == null)		// somehow transp mask is 
							return;                                     // created but ref to it 
						if (sMask != null)  // is not in the dict
							MakeImg(sMask, null, imgZip.ImageMask);		// adding it manually
						mod = true;
					}
					catch (Exception ex) {
						Utils.Log(ex);
						return;
					}
					PdfDictionary MakeImg(PdfDictionary dct, PdfDictionary msk, iTextSharp.text.Image img) {
						PdfImage ret = new PdfImage(img, null, null);
						var bb = ret.GetBytes()
							?? Utils.GetPrivVal<MemoryStream>(ret, "streamBytes")?.GetBytes();
						if (bb == null)            // why ????
							return null;
						dct.Keys.Where(x => msk == null || !x.Equals(PdfName.SMASK))
								.ToArray().ForEach(x => dct.Remove(x));
						ret.Keys.ForEach(x => dct.Put(x, ret.Get(x)));
						(dct as PRStream).SetDataRaw(bb);
						return dct;
					}
				}
			}
			public Stream Shrink(Stream src) {
				MemoryStream so = new MemoryStream(), si = new MemoryStream(src.GetBytes());
				si.Position = src.Position = 0;
				bool ch = false;
				PdfReader rdr = null;
				try {
					rdr = new PdfReader(si);
					if (rdr.NumberOfPages == 0 || rdr.IsEncrypted())
						return src;                             //PdfReader.unethicalreading = true;
					int pgw = (int)Enumerable.Range(1, rdr.NumberOfPages)
						.Select(x => rdr.GetPageSizeWithRotation(x).Width).Average();
					using (PdfStamper stmp = new PdfStamper(rdr, so)) {
						stmp.SetFullCompression();              // delDups would do it again...
						stmp.Writer.CloseStream = false;
						ch = AdjustFonts(stmp) || ch;
						//stmp.FormFlattening = stmp.FreeTextFlattening = true;
						rdr.RemoveUnusedObjects();      // delDups does it better.
					}
				}
				catch (Exception ex) {
					if (rdr != null)
						Utils.Log(ex);
					ch = false;
				}
				so.Position = 0;
				return ch ? so : src;                   // if no change return original stream
			}
			// AdjustFonts doesn't go inside form, because of recusrive nature and at the moment I don't get exactly how it works,
			// But as result the text inside Do/Form element will not be converted to ASCII and when the Font converted to TrueType
			// it comes out as garbage. Also Adjust fonts will not process images inside the Do/Form. As a result we wouldn't know
			// the width of non-full page images and try to compress and clean background on those. To get this information
			// unfortunately we need this second pass, since it goes over all the pages
			//https://psycodedeveloper.wordpress.com/2013/01/10/how-to-extract-images-from-pdf-files-using-c-and-itextsharp/
			bool AdjustFonts(PdfStamper stmp) {
				ImgRendListener lstnr = new ImgRendListener { stmp = stmp };
				PdfContentStreamProcessor imgProc = new PdfContentStreamProcessor(lstnr);
				PdfContentStreamProcessor txtProc = new PdfContentStreamProcessor(lstnr);
				foreach (var x in txtProc.RegisteredOperatorStrings) {
					var op = new CtxOperWrap();
					op.orig = txtProc.RegisterContentOperator(x, op);
				}
				Func<PdfDictionary, PdfName, PdfDictionary[]> getObjList = (res, nm) => {
					var obj = (PdfDictionary)PdfReader.GetPdfObject(res.Get(nm)) ?? new PdfDictionary();
					var lst = obj.Keys.Select(x => PdfReader.GetPdfObject(obj.Get(x)) as PdfDictionary)
								.Where(x => x != null);
					return lst.ToArray();
				};
				string[] sRot = new string[] { "", // 0
					"0 -1 1 0 0 {1} cm\n",		// 1 - 90 rotation command; rotate 270 more - test more
					"-1 0 0 -1 {0} {1} cm\n",	// 2 - 180 - test more
					"0 1 -1 0 {0} 0 cm\n"		// 3 - 270 rotate command; rotate 90 more;
				};
				var rdr = stmp.Reader;
				List<PdfDictionary> allFonts = new List<PdfDictionary>();
				for (int pg = 1; pg <= rdr.NumberOfPages; pg++) {
					lstnr.pgWidth = (int)Math.Min(rdr.GetPageSize(pg).Width, 792);
					var page = rdr.GetPageN(pg);
					var res  = page.GetAsDict(PdfName.RESOURCES);
					string[] strms = getObjList(res, PdfName.XOBJECT)
						.Select(x => x.Get(PdfName.SUBTYPE).GetString()).ToArray();
					bool fFrm = strms.Any(x => x == "/Form"), fImg = strms.Any(x => x == "/Image");
					var pgFnt = getObjList(res, PdfName.FONT);		// page fonts
					allFonts.AddRange(pgFnt);						// add to all fonts
					bool fFnt = pgFnt.Any(x => x.GetAsName(PdfName.SUBTYPE) != PdfName.TRUETYPE
											|| x.GetAsDict(PdfName.ENCODING) != null);
					var ctx = ContentByteUtils.GetContentBytesForPage(rdr, pg);
					if (fFrm || (fImg && !fFnt))                    // there might be images inside the form
						imgProc.ProcessContent(ctx, res);			// this one doesn't change the page
					if (fFnt) {                                     // if non trueType fonts need to re-incode strings
						var sz = rdr.GetPageSizeWithRotation(page);
						var rot = rdr.GetPageRotation(pg);
						lstnr.Canvas = stmp.GetUnderContent(pg);	// save page contents and create a canvas
						page.Remove(PdfName.CONTENTS);				// clear page	
						// stamper wraps everything in Rotate upon close. Un-rotate.
						lstnr.Canvas.InternalBuffer.Append(String.Format(sRot[rot / 90], sz.Width, sz.Height));
						txtProc.ProcessContent(ctx, res);			// txtProc.CtxOperWrap - rebuilds the page
					}
					lstnr.mod |= fFnt;
				}
				allFonts.Distinct().ForEach(x => { 
					lstnr.mod = ChkFont(x, null, 0) || lstnr.mod;
				});
				return lstnr.mod;
			}
			//https://github.com/QuestPDF/QuestPDF/issues/31
			static string[][] baseFonts = new string[][] {
					new string[] { "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique" },
					new string[] { "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic" },
					new string[] { "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique" },
					new string[] { "Symbol", "Symbol", "Symbol", "Symbol" },
					new string[] { "ZapfDingbats","ZapfDingbats","ZapfDingbats","ZapfDingbats" }
				};
			bool ChkFont(PdfDictionary dct, PdfDictionary par, int refIdx) {
				if (dct == null || !dct.IsFont()) return false;
				PdfDictionary fd = GetDict0(dct, PdfName.DESCENDANTFONTS)?.GetAsDict(PdfName.FONTDESCRIPTOR);
				PdfDictionary dsc = fd ?? dct.GetAsDict(PdfName.FONTDESCRIPTOR);
				if ((dsc?.Get(PdfName.FONTFILE2) ?? dsc?.Get(PdfName.FONTFILE3)) == null)
					return false;           // base font or no font glyphs - nothing to optimize...
				var fact = iTextSharp.text.FontFactory.FontImp;
				CMapAwareDocumentFont cf = new CMapAwareDocumentFont(dct);
				string orgFont = dct.GetAsName(PdfName.BASEFONT).ToString().Substring(1);
				BaseFont nf = null;
				if (orgFont.Length > 8 && orgFont[6] == '+'
				&& !baseFonts.SelectMany(x=>x).Any(x => x == orgFont)) {
					if (fact.RegisteredFonts.Count() == 14)					// orig only 14 base fonts
						iTextSharp.text.FontFactory.RegisterDirectories();	// load windows fonts
					string res = "";										// try to match pdf->win
					Regex.Replace(orgFont.Substring(7).Replace(',', ' '), "([A-Z])", " $1")
						.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)									
						.ForEach(x => res += (res.Length > 0 && res.LastOrDefault() != '-' && x.Length > 1 ? " " : "") + x);
					nf = fact.GetFont(res).BaseFont;
				}
				if (nf == null) {											// can't match to windows
					int flg = (dsc == null) ? 0 : dsc.GetAsNumber(PdfName.FLAGS).GetInt();
					bool ff = (flg & 0x00001) != 0 || orgFont.Contains("Courier"),	// fixed
						fs = (flg & 0x00002) != 0 || orgFont.Contains("Times"),		// sarif
						fz = (flg & 0x00004) != 0;									// Symbolic
					int fi = ((flg & 0x00040) != 0 || orgFont.Contains("Italic")) ? 2 : 0,
						fb = ((flg & 0x40000) != 0 || orgFont.Contains("Bold")) ? 1 : 0;
					var fam = baseFonts[ff ? 2 : fs ? 1 : 0][fb + fi];		// replace with base fonts
					nf = fact.GetFont(fam).BaseFont;						// based on FontDescr.Flags
					//Console.WriteLine("Base:" + orgFont);
				}
				if (nf == null) {
					//Console.WriteLine("Not found:" + orgFont);
					return false;
				}
				int[] widths = nf.Widths.ToArray();							// load widths 
				Enumerable.Range(0, 255)									// and override from Dict
					.Select(x => new { c = cf.Decode(new byte[] { (byte)x }, 0, 1), w = cf.GetWidth(x) })
					.Where(x => x.c != "" && x.w != 0 && x.c[0] < 256).ForEach(x => widths[x.c[0]] = x.w);
				PdfDictionary ndsc = new PdfDictionary();
				ndsc.PutAll(dsc);											// copy orig descriptor
				dct.Remove(PdfName.DESCENDANTFONTS);						// to a new one
				dct.Remove(PdfName.FONTDESCRIPTOR);							// and clean all the 
				dct.Remove(PdfName.TOUNICODE);								// font stuff. Add basic
				dct.Put(PdfName.BASEFONT,		new PdfName(nf.PostscriptFontName));
				dct.Put(PdfName.SUBTYPE,		PdfName.TRUETYPE);
				dct.Put(PdfName.ENCODING,		PdfName.WIN_ANSI_ENCODING);
				dct.Put(PdfName.WIDTHS,			new PdfArray(widths));
				dct.Put(PdfName.FIRSTCHAR,		new PdfNumber(0));
				dct.Put(PdfName.LASTCHAR,		new PdfNumber(255));
				dct.Put(PdfName.FONTDESCRIPTOR, ndsc);
				ndsc.Remove(PdfName.FONTFILE2);								// most important 
				ndsc.Remove(PdfName.FONTFILE3);								// make sure gliphs are gone
				ndsc.Put(PdfName.FONTNAME, new PdfName(nf.PostscriptFontName));
				ndsc.Put(new PdfName("MissingWidth"), new PdfNumber(widths.Where(x => x != 0).Average()));
				return true;
			}
		}

		static bool IsScanPDF(Stream src) {
			try {
				MemoryStream si = new MemoryStream(src.GetBytes());
				si.Position = src.Position = 0;
				PdfReader rdr = new PdfReader(si);
				int pgw = (int)Enumerable.Range(1, rdr.NumberOfPages)
					.Select(x => rdr.GetPageSizeWithRotation(x).Width).Average();
				return Enumerable.Range(0, rdr.XrefSize)
					.Select(x => rdr.GetPdfObject(x) as PdfDictionary)
					.Any(x => x != null && x.Get(PdfName.WIDTH).GetInt() > pgw
								&& x.Get(PdfName.SUBTYPE).GetString() == "/Image");
			}
			catch {
				return false;
			}
		}
		static Stream DelDups(Stream src) {         // remove dup and unused obj
			MemoryStream fs = new MemoryStream(), fi = new MemoryStream(src.GetBytes());
			fi.Position = src.Position = 0;
			try {
				using (var rdr = new PdfReader(fi)) {
					if (rdr.NumberOfPages == 0 || rdr.IsEncrypted())
						return src;
					using (var doc = new iTextSharp.text.Document())
					using (var w = new PdfSmartCopy(doc, fs)) {
						w.CloseStream = false;
						w.SetFullCompression();         // Enable full compression
						doc.Open();
						for (int i = 1; i <= rdr.NumberOfPages; i++)
							w.AddPage(w.GetImportedPage(rdr, i));
					}
				}
				fs.Position = 0;
				return fs;
			}
			catch (Exception ex) {
				Utils.Log(ex);
				return src;                     // if no change return original stream
			}
		}
	}
}