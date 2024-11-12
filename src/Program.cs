using iTextSharp.text.pdf;
using iTextSharp.text.pdf.parser;
using System;
using System.Collections.Generic;
using System.Drawing.Imaging;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace PdfShrink {
	public static class Utils {
		public static void Log(string m) {
			Console.WriteLine(m);
		}
		public static string GetString(this object o) {
			if (o == null || Convert.IsDBNull(o))
				return "";
			if (o.GetType() == typeof(decimal)) {
				decimal d = (decimal)o;
				if (d < Int64.MaxValue && (d - (long)d) == 0L)
					return ((long)d).ToString();
			}
			return o.ToString();
		}
		public static int GetInt(this object o) {
			if (o == null)
				return 0;
			if (o is int || o is decimal || o is long || o is float || o is double || o is short)
				return Convert.ToInt32(o);
			int i = 0;
			int.TryParse(o.GetString(), out i);
			return i;	
		}
		public static System.Drawing.Image ScaleDown(this System.Drawing.Image img, int mxw = 0, int mxh = 0) {
			if (mxw == 0) mxw = 1280;
			if (mxh == 0) mxh = mxw;
			if (img.Width <= mxw && img.Height <= mxh)
				return img;
			if (img.RawFormat.Guid == ImageFormat.Gif.Guid                      // can't process gifs 
			&& (img.GetFrameCount(FrameDimension.Time) > 1                      // /w animation or transparency
				|| img.PropertyIdList.FirstOrDefault(i => i == 0x5104) != 0))   // PropertyTagIndexTransparent
				return img;
			float ratio = Math.Min(Math.Min((float)mxw / img.Width, (float)mxh / img.Height), 1);
			int dw = Math.Max(1, (int)(img.Width * ratio)), dh = Math.Max(1, (int)(img.Height * ratio));
			Bitmap bmp = new Bitmap(dw, dh);
			using (Graphics g = Graphics.FromImage((System.Drawing.Image)bmp)) {
				g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
				g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic;
				g.DrawImage(img, 0, 0, dw, dh);
			}
			return bmp;
		}
		public static System.Drawing.Image PixFrmt(this System.Drawing.Image img, PixelFormat pf) {
			if (img.PixelFormat == pf || pf == PixelFormat.Undefined)
				return img;
			var bmp = new Bitmap(img).Clone(new Rectangle(0, 0, img.Width, img.Height), pf);
			return bmp;
		}
		public static System.Drawing.Image BgClean(this System.Drawing.Image img) {
			var bmp = new Bitmap(img).Clone(new Rectangle(0, 0, img.Width, img.Height), PixelFormat.Format4bppIndexed);
			var data = bmp.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format8bppIndexed);
			var bytes = new byte[data.Height * data.Stride];                // first convert to 8bit
			Marshal.Copy(data.Scan0, bytes, 0, bytes.Length);
			Dictionary<byte, int> map = new Dictionary<byte, int>();        // map of all the color frq
			for (int y = 0; y < data.Height; y++)
				for (int p = y * data.Stride, x = 0; x < data.Width; x++, p++) {
					byte c = bytes[p];
					if (!map.ContainsKey(c)) map[c] = 1;
					else map[c]++;
				}
			var cc = map.Count();                                        // bg clr used in > 20%
			map.Where(x => x.Value < bmp.Width * bmp.Height * 2 / 10).ToArray().ForEach(x => map.Remove(x.Key));
			if (map.Count() > 1 && map.Count < cc) {                     // multiple bgs
				var bg = map.OrderByDescending(x => x.Value).First().Key;// most frq color - prim bg
				int[] io = new int[] { -data.Stride-1, -data.Stride, -data.Stride+1,
							-1, 0, +1, +data.Stride-1, +data.Stride, +data.Stride+1};
				for (int y = 1; y < data.Height - 1; y++)               // repl oth bg with prim
					for (int x = 1, p = y * data.Stride + x; x < data.Width - 1; x++, p++)
						if (bytes[p] != bg && map.ContainsKey(bytes[p]) // if misc bg around
						&& io.Select(z => bytes[p + z]).All(z => map.ContainsKey(z)))
							bytes[p] = bg;
			}
			Marshal.Copy(bytes, 0, data.Scan0, bytes.Length);
			bmp.UnlockBits(data);
			return bmp;
		}
		public static System.Drawing.Image MaskWith(this System.Drawing.Image img, System.Drawing.Image mask) {
			if (img.Width != mask.Width || img.Height != mask.Height)
				return img;
			var bi = new Bitmap(img).Clone(new Rectangle(0, 0, img.Width, img.Height), PixelFormat.Format24bppRgb);
			var bm = new Bitmap(mask).Clone(new Rectangle(0, 0, img.Width, img.Height), PixelFormat.Format24bppRgb);
			var di = bi.LockBits(new Rectangle(Point.Empty, bi.Size), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
			var dm = bm.LockBits(new Rectangle(Point.Empty, bm.Size), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
			var ai = new byte[di.Height * di.Stride]; Marshal.Copy(di.Scan0, ai, 0, ai.Length);
			var am = new byte[dm.Height * dm.Stride]; Marshal.Copy(dm.Scan0, am, 0, am.Length);
			for (int i = 0; i < ai.Length; i++)
				ai[i] |= (byte)~am[i];          //if (am[i] == 0) ai[i] = 255;
			Marshal.Copy(ai, 0, di.Scan0, ai.Length);
			bm.UnlockBits(dm);
			bi.UnlockBits(di);
			return bi;
		}
		public static byte[] GetBytes(this System.Drawing.Image bmp, ImageFormat oif = null, long jpgQty = 0) {
			MemoryStream ms = new MemoryStream();
			ImageCodecInfo ci = null;
			oif = oif ?? bmp.RawFormat;
			if (new ImageFormat[] { ImageFormat.Emf, ImageFormat.Bmp, ImageFormat.Exif, ImageFormat.Icon,
				ImageFormat.MemoryBmp, ImageFormat.Wmf }.Any(x => x.Guid == oif.Guid))
				oif = ImageFormat.Jpeg;
			if (oif.Guid == ImageFormat.Jpeg.Guid && jpgQty != 0
			&& null != (ci = ImageCodecInfo.GetImageEncoders().FirstOrDefault(c => c.FormatID == oif.Guid))) {
				EncoderParameters prm = new EncoderParameters(1);
				prm.Param[0] = new EncoderParameter(System.Drawing.Imaging.Encoder.Quality, jpgQty);
				bmp.Save(ms, ci, prm);
			}
			else
				bmp.Save(ms, oif);
			ms.Position = 0;
			return ms.ToArray();
		}
		//https://www.codeguru.com/dotnet/inverting-image-colors-in-net/
		//https://www.codeproject.com/Questions/5251461/Invert-color-image-in-MVC-Csharp
		public static System.Drawing.Image Invert(this System.Drawing.Image img) {
			ColorMatrix clrMtrx = new ColorMatrix(new float[][] {
									new float[] {-1, 0,  0, 0, 0},
									new float[] {0, -1,  0, 0, 0},
									new float[] {0,  0, -1, 0, 0},
									new float[] {0,  0,  0, 1, 0},
									new float[] {1,  1,  1, 0, 1}
								});
			ImageAttributes attr = new ImageAttributes();
			attr.SetColorMatrix(clrMtrx);
			Bitmap pic = new Bitmap(img.Width, img.Height);
			using (Graphics g = Graphics.FromImage(pic))
				g.DrawImage(img, new Rectangle(0, 0, img.Width, img.Height),
							0, 0, img.Width, img.Height, GraphicsUnit.Pixel, attr);
			return pic;
		}
		//https://learn.microsoft.com/en-us/answers/questions/507266/how-to-use-colormatrix-to-highlight-a-specific-col
		//https://stackoverflow.com/questions/55849048/resize-bitmap-image-and-make-bolder-lines
		public static System.Drawing.Image Contrast(this System.Drawing.Image img, float r) {
			Bitmap pic = new Bitmap(img.Width, img.Height);
			ImageAttributes attr = new ImageAttributes();
			attr.SetGamma(r, ColorAdjustType.Bitmap);
			using (Graphics g = Graphics.FromImage(pic))
				g.DrawImage(img, new Rectangle(0, 0, img.Width, img.Height),
							0, 0, img.Width, img.Height, GraphicsUnit.Pixel, attr);
			return pic;
		}
		public static Stream WriteBytes(this Stream s, params IEnumerable<byte>[] b) {
			b.ForEach(x => s.Write(x.ToArray(), 0, x.Count()));
			return s;
		}
		public static IEnumerable<T> ForEach<T>(this IEnumerable<T> arr, Action<T> exp) {
			foreach (T o in arr)
				exp(o);
			return arr;
		}
		public static T DeleteAt<T>(this List<T> arr, int i) {
			if (i < 0) i = arr.Count - 1;
			T ret = arr[i];
			arr.RemoveAt(i);
			return ret;
		}
		public static byte[] GetBytes(this Stream s) {
			MemoryStream ms = new MemoryStream();
			s.CopyTo(ms);
			ms.Position = 0;
			return ms.ToArray();
		}
	}
	class PdfCompress : PdfContentStreamProcessor {
			Dictionary<PdfDictionary, int> imgRefs = new Dictionary<PdfDictionary, int>();
			class xRendListener : IRenderListener {
				internal PdfCompress par = null;
				public void BeginTextBlock() { }
				public void EndTextBlock() { }
				public void RenderText(TextRenderInfo ri) { }
				public void RenderImage(ImageRenderInfo ri) {
					try {
						PdfImageObject img = ri.GetImage();		// can throw exception if 
						if (img == null) return;				// don't know how to handle img
						var ctm = ri.GetImageCTM();
						var key = img.GetDictionary();
						var w = par.imgRefs.ContainsKey(key) ? par.imgRefs[key] : 0;
						par.imgRefs[key] = Math.Max(w, (int)ctm[0]);
					}
					catch { };
				}
			}

			// constructor giving the parent a dummy listener to talk to
			public PdfCompress() : base(new xRendListener()) { 
				(this.RenderListener as xRendListener).par = this;
			}
			public Stream Shrink(Stream src) {
				MemoryStream so = new MemoryStream(), si = new MemoryStream();
				src.CopyTo(si);
				si.Position = src.Position = 0;
				bool ch = false;
				PdfReader rdr = null;
				try {
					rdr = new PdfReader(si);
					if (rdr.NumberOfPages == 0 || rdr.IsEncrypted())
						return src;								//PdfReader.unethicalreading = true;
					int pgw = (int)Enumerable.Range(1, rdr.NumberOfPages)
						.Select(x => rdr.GetPageSizeWithRotation(x).Width).Average();
					using (PdfStamper stmp = new PdfStamper(rdr, so)) {
						stmp.SetFullCompression();				// delDups would do it again...
						stmp.Writer.CloseStream = false;
						AdjustFonts(stmp);
						//stmp.FormFlattening = stmp.FreeTextFlattening = true;
						for (int i = rdr.XrefSize; i > 0; i--) {
							PdfDictionary obj = rdr.GetPdfObject(i) as PdfDictionary;
							if (obj == null) continue;
							if (obj.IsFont())
								ch = ChkFont(obj, null) || ch;
							else if (obj.IsStream() && obj.Get(PdfName.SUBTYPE) != null)
								ch = ChkImage(obj, pgw) || ch;
						}
						rdr.RemoveUnusedObjects();		// delDups does it better.
					}
				}
				catch (Exception ex) {
					if (rdr != null)
						Utils.Log(ex.ToString());
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
			// 
			//https://psycodedeveloper.wordpress.com/2013/01/10/how-to-extract-images-from-pdf-files-using-c-and-itextsharp/
			void AdjustFonts(PdfStamper stmp) {
				var rdr = stmp.Reader;
				var prs = new PdfReaderContentParser(rdr);
				Func<PdfDictionary, PdfName, string[]> getObjList = (res, nm) => {
					var obj = (PdfDictionary)PdfReader.GetPdfObject(res.Get(nm)) ?? new PdfDictionary();
					var lst = obj.Keys.Select(x => PdfReader.GetPdfObject(obj.Get(x)) as PdfDictionary)
								.Where(x => x != null).Select(x => x.Get(PdfName.SUBTYPE).GetString());
					return lst.ToArray();
				};
				for (int pg = 1; pg <= rdr.NumberOfPages; pg++) {
					var page = rdr.GetPageN(pg);
					var res = page.GetAsDict(PdfName.RESOURCES);
					string[] strms = getObjList(res, PdfName.XOBJECT), fonts = getObjList(res, PdfName.FONT);
					if (strms.Any(x => x == "/Form"))				// there might be images inside the form
						prs.ProcessContent(pg, this.RenderListener);// try to get their dims. Still missing text to convert
					if (fonts.Count(x => x != "/TrueType") > 0 || strms.Any(x => x == "/Image")) {
						var ctx = ContentByteUtils.GetContentBytesForPage(rdr, pg);
						Canvas = stmp.GetUnderContent(pg);
						page.Remove(PdfName.CONTENTS);
						ProcessContent(ctx, res);
						Canvas = null;
					}
				}																// inside the form
			}
			PdfDictionary GetDict0(PdfDictionary o, PdfName n) {
				if (o == null) return null;
				var d = o.GetAsDict(n);
				if (d != null) return d;
				var a = o.GetAsArray(n) ?? new PdfArray();
				if (a.Count() == 0) return null;
				return PdfReader.GetPdfObject(a[0]) as PdfDictionary;
			}
			//https://github.com/QuestPDF/QuestPDF/issues/31
			PdfName MapFontName(PdfDictionary dct, PdfName fnt, string sType) {               // map font name to one of the internals
				string nmFnt = fnt.ToString();
				if (iTextSharp.text.FontFactory.IsRegistered(nmFnt))
					return null;
				var parts = nmFnt.Split(new char[] { '+' }, StringSplitOptions.RemoveEmptyEntries);      // we only remove TTF fonts
				if (sType == "/TrueType" && parts.Length > 1 && parts[0].Length > 5 && parts[1].Length > 4)
					return new PdfName(parts[1]);
				PdfDictionary dsc = dct.GetAsDict(PdfName.FONTDESCRIPTOR);
				int flg = (dsc == null) ? 0 : dsc.GetAsNumber(PdfName.FLAGS).GetInt();
				bool fs = (flg & 0x00002) != 0 || nmFnt.Contains("Times"),  // sarif
						ff = (flg & 0x00001) != 0 || nmFnt.Contains("Courier"), // fixed
						fi = (flg & 0x00040) != 0 || nmFnt.Contains("Italic"),  // italic
						fb = (flg & 0x40000) != 0 || nmFnt.Contains("Bold");    // bold
				if (fi && !ff && fs)                                            // sarif & italic & not fixed
					return new PdfName(fb ? BaseFont.TIMES_BOLDITALIC : BaseFont.TIMES_ITALIC);
				if (fb)                                                         // bold
					return new PdfName(ff ? BaseFont.COURIER_BOLD : (fs ? BaseFont.TIMES_BOLD : BaseFont.HELVETICA_BOLD));
				return new PdfName(ff ? BaseFont.COURIER : (fs ? BaseFont.TIMES_ROMAN : BaseFont.HELVETICA));
			}
			bool ChkFont(PdfDictionary dct, PdfDictionary par) {
				if (dct == null || !dct.IsFont()) return false;
				PdfDictionary dsc = dct.GetAsDict(PdfName.FONTDESCRIPTOR);
				PdfDictionary ch0 = GetDict0(dct, PdfName.DESCENDANTFONTS);
				if (ch0 == null && (dsc == null || dsc.Get(PdfName.FONTFILE2) == null))
					return false;
				string sType = dct.GetAsName(PdfName.SUBTYPE).GetString();
				PdfName font = dct.GetAsName(PdfName.BASEFONT),
						mFnt = MapFontName(ch0 ?? dct, font, sType);
				if (mFnt == null) return false;
				String msg = "\t" + dct.Get(PdfName.NAME).GetString() + " - " + font.ToString() + " ";
				msg += "Subtype: " + dct.GetAsName(PdfName.SUBTYPE).ToString() + "; ";
				msg += "Kids: " + (ch0 == null ? 0 : 1) + "; ";
				msg += (dsc ?? new PdfDictionary()).Get(PdfName.FONTFILE2) == null ? "" : "FontFile2; ";
				if (ch0 != null) {
					//Console.WriteLine(msg + " map to " + mFnt.ToString() + " ***** Deleting FONTFILE2");
					dct.Put(PdfName.BASEFONT, mFnt);
					dct.Put(PdfName.SUBTYPE, PdfName.TRUETYPE);
					dct.Put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
					ch0.Put(PdfName.BASEFONT, mFnt);
					ch0.Put(PdfName.SUBTYPE, PdfName.TRUETYPE);
					ch0.Put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
					PdfDictionary fd = ch0.GetAsDict(PdfName.FONTDESCRIPTOR);
					if (fd != null) {
						fd.Remove(PdfName.FONTFILE2);
						fd.Remove(PdfName.FONTBBOX);
						fd.Put(PdfName.FONTNAME, mFnt);
					}
					return true;
				}
				if (sType == "/TrueType" || dsc != null) {
					//Console.WriteLine(msg + " map to " + mFnt.ToString() + " ***** Deleting FONTFILE2");
					dct.Put(PdfName.BASEFONT, mFnt);
					dct.Put(PdfName.SUBTYPE, PdfName.TRUETYPE);
					dct.Put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
					if (dsc != null) {
						dsc.Put(PdfName.FONTNAME, mFnt);      // replace the fontname 
						dsc.Remove(PdfName.FONTFILE2);        // remove the font file
					}
					return true;
				}
				//Console.WriteLine(msg);
				return false;
			}
			//https://kb.itextpdf.com/it5kb/how-to-extract-embedded-streams
			//https://stackoverflow.com/questions/8740244/pdf-compression-with-itextsharp
			// compress PDF -
			// https://cjhaas.com/2012/01/06/how-to-recompress-images-in-a-pdf-using-itextsharp/
			// https://kb.itextpdf.com/home/it5kb/examples/reduce-image
			bool ChkImage(PdfDictionary dict, int pgWidth) {
				if (dict == null || !dict.Get(PdfName.SUBTYPE).Equals(PdfName.IMAGE))
					return false;
				//if (imgDict .Get(PdfName.IMAGEMASK).GetString() == "true") return false;
				PdfObject fltr = dict.Get(PdfName.FILTER);
				if (fltr == null)
					return false;
				PdfArray fa = fltr.IsArray() ? fltr as PdfArray : new PdfArray { fltr };
				if (!fa.OfType<PdfObject>().Any(x => x.Equals(PdfName.DCTDECODE)
						|| x.Equals(PdfName.CCITTFAXDECODE) || x.Equals(PdfName.FLATEDECODE)))
					return false;
				System.Drawing.Image wi;
				int oldLen = 0;
				try {
					wi = new PdfImageObject((PRStream)dict).GetDrawingImage();
					oldLen = ((PRStream)dict).Length;
				}
				catch (Exception ex) {
					Utils.Log(ex.ToString());
					return false;
				}
				var msk = PdfReader.GetPdfObject(dict.Get(PdfName.MASK)) as PRStream;
				if (msk != null) try {
						wi = new PdfImageObject(msk).GetDrawingImage();
						oldLen += msk.Length;
					}
					catch (Exception) {
						return false;
					};
				int mxw = (imgRefs.ContainsKey(dict)) ? imgRefs[dict] : pgWidth;
				// Interpolate % compression(qty) as size/(w*h/4.5)*100 
				// AppleNotes~90, MicrosoftLens~33, SafeScan~14
				double qty = oldLen * 4.5 * 100 / (wi.Width * wi.Height);   // width ratio image to page
				if (wi.Width <= mxw*2 && qty < 40) return false;			// simplest check - image is page width*2
				if (wi.Width <= mxw*4 && qty < 20) return false;            // any further image size reduction affects legibility...
				byte[] newBytes = null;
				if (dict.Get(PdfName.BITSPERCOMPONENT).GetInt() != 1) {
					if (PdfName.DEVICECMYK.Equals(dict.GetAsName(PdfName.COLORSPACE))) {
						Utils.Log("Image with CMYK colorspace");
						wi = wi.Invert().Contrast(5);
					}
					msk = PdfReader.GetPdfObject(dict.Get(PdfName.SMASK)) as PRStream;
					if (msk != null) try {
							wi = wi.MaskWith(new PdfImageObject(msk).GetDrawingImage());
							//Console.WriteLine("Has SMASK");
						}
						catch (Exception) {
							//Console.WriteLine("Bad SMASK");
						};
					wi = wi.ScaleDown(mxw*2, 0);
					newBytes = (mxw >= pgWidth) ?  wi.BgClean().GetBytes(ImageFormat.Png) 
								: wi.PixFrmt(PixelFormat.Format24bppRgb).GetBytes(ImageFormat.Jpeg);
				}
				else {                                                      // bug in iTextSharp skewes images 
					/* For following no need to redo image???
					 * Width=2550; BitsPerComponent=1; ColorSpace=/DeviceGray; Decode=[0, 1]; Filter=[/CCITTFaxDecode]; 
					 *		/DecodeParms[] : K=-1
					 * Need conv for:
					 * Filter=/CCITTFaxDecode; BitsPerComponent=1; ColorSpace=/DeviceGray; Width=1668
					 *		/DecodeParms : K=0; BlackIs1=true
					 */
					var dp = GetDict0(dict, PdfName.DECODEPARMS);
					if (dp != null && 0 != (wi.Width % 8)
					&& fa.Any(x => x.Equals(PdfName.CCITTFAXDECODE))
					&& dp.GetAsNumber(PdfName.K).GetInt() == 0) {           // not sure it's width%8 or /K != -1
						var prmK = dp.GetAsNumber(PdfName.K).GetInt();
						var prmBlk1 = (dp.GetAsBoolean(PdfName.BLACKIS1) ?? PdfBoolean.PDFFALSE).BooleanValue;
						List<byte> raw = new List<byte>(PdfReader.GetStreamBytesRaw((PRStream)dict));
						if ((raw.Count % 2) != 0) raw.Add(0);
						MemoryStream ms = new MemoryStream();
						ms.WriteBytes(new byte[] { 0x49, 0x49, 0x2a, 0 },
									BitConverter.GetBytes(raw.Count + 16),
									new byte[] { 0, 0, 0, 0, 0, 0, 0, 0 }, raw, new byte[] { 7, 0 });
						int[] ctrl = new int[] {
							0x40100,1,wi.Width, 0x40101,1,wi.Height, 0x40117,1,raw.Count,
							0x40103,1,prmK == -1 ? 4 : 3,				// COMPRESSION
							0x40106,1,prmBlk1 ? 1 : 0,					// PHOTOMETRIC
							0x40102,1,1, 0x40111,1,16					// BITS/STRIPOFFSETS
						};
						ctrl.ForEach(x => ms.WriteBytes(BitConverter.GetBytes(x)));
						wi = Bitmap.FromStream(ms);
					}
					// scaledown converts 1bpp to full color. Converting back to 1bpp looses resolution. Keep it as 4bpp.
					if (wi.Width >= mxw*2)
						wi = wi.ScaleDown(mxw*2, 0).PixFrmt(PixelFormat.Format4bppIndexed);
					newBytes = wi.GetBytes(ImageFormat.Png);
				}
				if (newBytes == null || newBytes.Length > oldLen * 0.8)
					return false;
				var imgZip = iTextSharp.text.Image.GetInstance(newBytes);
				if (dict.Get(PdfName.IMAGEMASK).GetString() == "true")
					imgZip.MakeMask();
				PdfImage nImg = new PdfImage(imgZip, null, null);
				dict.Keys.ToArray().ForEach(x => dict.Remove(x)); //.Where(x => x.ToString() != "/Mask")
				nImg.Keys.ForEach(x => dict.Put(x, nImg.Get(x)));
				(dict as PRStream).SetDataRaw(nImg.GetBytes());
				return true;
			}
			public override IContentOperator RegisterContentOperator(string sOper, IContentOperator objOper) {
				var wrapper = new CtxOperWrap();
				wrapper.orig = objOper;
				var prev = base.RegisterContentOperator(sOper, wrapper);
				return (prev is CtxOperWrap operWrap ? operWrap.orig : prev);
			}
			public override void ProcessContent(byte[] contentBytes, PdfDictionary resources) {
				this.Resources.Add(resources);
				base.ProcessContent(contentBytes, resources);
				this.Resources.DeleteAt(-1);
			}
			// members holding the output canvas and the resources
			protected PdfContentByte Canvas = null;
			protected List<PdfDictionary> Resources = new List<PdfDictionary>();
			// A content operator class to wrap all content operators to forward the invocation to the editor
			class CtxOperWrap : IContentOperator { 
				static Dictionary<char, char> specChars = new Dictionary<char, char> {
					{ (char) 381, 'Z' }, { (char)8211, '-' }, { (char)8217, '`' }, 
					{ (char)8220, '"' }, { (char)8221, '"' }, { (char)8230, '.' },
					{ (char)8239, ' ' }
				};
				internal IContentOperator orig = null;
				PdfString StrCvt(PdfContentStreamProcessor proc, PdfString i) {
					if (proc.Gs().Font.FontDictionary.Get(PdfName.SUBTYPE) == PdfName.TRUETYPE)
						return i;
					byte[] bytes = (i as PdfString).GetBytes();
					string str = proc.Gs().Font.Decode(bytes, 0, bytes.Length);
					str = new string(str.ToCharArray()
						.Select(x => specChars.ContainsKey(x) ? specChars[x] : x).ToArray());
					//str.Where(x => x > 255).ForEach(x => Console.WriteLine("{0} {1}", (int)x, x));
					return new PdfString(str);
				}
				void ShowTextArray(PdfContentStreamProcessor proc, PdfArray prms) {
					for (int i = 0; i < prms.Count(); i++)
						if (prms[i] is PdfString) 
							prms[i] = StrCvt(proc, prms[i] as PdfString);
				}
				void ShowText(PdfContentStreamProcessor proc, List<PdfObject> prms) {
					prms[0] = StrCvt(proc, prms[0] as PdfString);
				}
				void ChkDoCmd(PdfCompress proc, PdfLiteral oper, List<PdfObject> prms) {
					PdfDictionary xObj = PdfReader.GetPdfObjectRelease(proc.Resources.Last().GetAsDict(PdfName.XOBJECT).Get(prms[0] as PdfName))
							as PdfDictionary ?? new PdfDictionary();					
					if (xObj.GetAsName(PdfName.SUBTYPE).GetString() == "/Image")		
						orig.Invoke(proc, oper, prms);
				}
				public void Invoke(PdfContentStreamProcessor proc, PdfLiteral oper, List<PdfObject> prms) {
					string op = oper.ToString();
					var i = 0;
					var p = (proc as PdfCompress);
					if (op != "Do")						// calling for "Do/Form" messes up form data and positioning
						orig.Invoke(proc, oper, prms);	// but we need to call it for "Do/Image" to save image dims
					if (op == "TJ")
						ShowTextArray(p, prms[0] as PdfArray);
					else if (op == "Tj")
						ShowText(p, prms);
					else if (op == "Do")				// call def for Do/Image. Not processing Do/Form skips images
						ChkDoCmd(p, oper, prms);		// imbedded in the form - TODO.
					//else Console.WriteLine(op);
					foreach (var pdfObject in prms) {
						pdfObject.ToPdf(null, p.Canvas.InternalBuffer);
						p.Canvas.InternalBuffer.Append(prms.Count > ++i ? (byte)' ' : (byte)'\n');
					}
				}
			}
		}

	internal class Program {
		static void Main(string[] args) {
			using (Stream fin = new FileStream(args[0], FileMode.Open, FileAccess.Read, FileShare.Read))
				File.WriteAllBytes(args[1], new PdfCompress().Shrink(fin).GetBytes());
		}
	}
}
