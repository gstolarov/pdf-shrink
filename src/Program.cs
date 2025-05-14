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
using System.util;
using System.Runtime.CompilerServices;
using System.Windows.Media.Imaging;
using System.Reflection;

namespace PdfShrink {
	public class MessageException : Exception {
		public MessageException(string msg) : base(msg) {  }
	}
	public static class Utils {
		public static Decimal GetNum(this object o) {
			if (o == null || Convert.IsDBNull(o))
				return 0;
			if (o is int || o is decimal || o is long || o is float || o is double || o is short)
				return Convert.ToDecimal(o);
			return Decimal.TryParse(o.ToString(), out decimal r) ? r : 0;
		}
		public static int GetInt(this object o) {
			decimal r = o.GetNum();
			return (r > int.MinValue && r < int.MaxValue) ? (int)r : 0;
		}
		public static string GetString(this object o) {
			if (o == null || Convert.IsDBNull(o))
				return "";
			return o.ToString();
		}
		public static string GetString<T>(this IEnumerable<T> arr, Func<T, string> exp, string sep) {
			StringBuilder ret = new StringBuilder();
			foreach (T o in arr) {
				if (ret.Length > 0) ret.Append(sep);
				ret.Append(exp == null ? o.GetString() : exp(o));
			}
			return ret.ToString();
		}
		public static IEnumerable<T> ForEach<T>(this IEnumerable<T> arr, Action<T> exp) {
			foreach (T o in arr)
				exp(o);
			return arr;
		}
		public static Stream WriteBytes(this Stream s, params IEnumerable<byte>[] b) {
			b.ForEach(x => s.Write(x.ToArray(), 0, x.Count()));
			return s;
		}
		public static T GetPrivVal<T>(object obj, string nm) {
			var fld = obj.GetType().GetField(nm, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
			return (T)fld?.GetValue(obj);
		}
		public static byte[] GetBytes(this Stream s) {
			MemoryStream ms = new MemoryStream();
			s.CopyTo(ms);
			ms.Position = 0;
			return ms.ToArray();
		}
		public static System.Drawing.Image ScaleDown(this System.Drawing.Image img, int mxw = 0, int mxh = 0) {
			if (mxw == 0 && mxh == 0) mxw = mxh = 1280;
			float ratio = Math.Min(mxw > 0 ? (float)mxw / img.Width : 1, mxh > 0 ? (float)mxh / img.Height : 1);
			int dw = Math.Max(1, (int)(img.Width * ratio)), dh = Math.Max(1, (int)(img.Height * ratio));
			if (img.Width <= mxw && img.Height <= mxh)
				return img;
			if (img.RawFormat.Guid == ImageFormat.Gif.Guid                      // can't process gifs 
			&& (img.GetFrameCount(FrameDimension.Time) > 1                      // /w animation or transparency
				|| img.PropertyIdList.FirstOrDefault(i => i == 0x5104) != 0))   // PropertyTagIndexTransparent
				return img;
			Bitmap bmp = new Bitmap(dw, dh, System.Drawing.Image.IsAlphaPixelFormat(img.PixelFormat)
										? PixelFormat.Format32bppArgb : PixelFormat.Format24bppRgb);
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
				Marshal.Copy(bytes, 0, data.Scan0, bytes.Length);
			}
			bmp.UnlockBits(data);
			return bmp;
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
		public static System.Drawing.Image XForm(this System.Drawing.Image img, float[] c) {
			ImageAttributes attr = new ImageAttributes();
			attr.SetColorMatrix(new ColorMatrix(new float[][] {
									new float[] {  c[0],  c[1],  c[2],  c[3],  c[4] },
									new float[] {  c[5],  c[6],  c[7],  c[8],  c[9] },
									new float[] { c[10], c[11], c[12], c[13], c[14] },
									new float[] { c[15], c[16], c[17], c[18], c[19] },
									new float[] { c[20], c[21], c[22], c[23], c[24] },
								}));
			Bitmap pic = new Bitmap(img.Width, img.Height);
			using (Graphics g = Graphics.FromImage(pic))
				g.DrawImage(img, new Rectangle(0, 0, img.Width, img.Height),
							0, 0, img.Width, img.Height, GraphicsUnit.Pixel, attr);
			return pic;
		}
		public static System.Drawing.Image Invert(this System.Drawing.Image img, float c1 = -1, float c2 = 0) {
			return img.XForm(new float[] {
									c1, c2, c2, 0, 0,
									c2, c1, c2, 0, 0,
									c2, c2, c1, 0, 0,
									0,   0,  0, 1, 0,
									1,   1,  1, 0, 1
								});
		}
		public static void Log(Exception ex) {
			if (ex != null)
				Console.WriteLine(ex.ToString());
		}
		public static void Swap<T>(ref T a, ref T b) {
			T t = a; a = b; b = t;
		}
	}
	namespace JBig2Dec {
		class FastBitSet {
			public long[] w; // BitArray???
			static readonly int BITS = IntPtr.Size * 8;                                                                              // 32
			public static readonly int POT = (BITS == 32) ? 5 : (BITS == 16) ? 4 : (BITS == 64) ? 6 : 7;                    // 5
			public static readonly int MASK = (int)(((BITS == 64) ? UInt64.MaxValue : UInt32.MaxValue) >> (BITS - POT));    // 1 1111

			public FastBitSet(int length) {
				w = new long[(length + BITS - 1) / BITS];
			}
			public void setAll(bool value) {
				long v = value ? -1L : 0;
				for (int i = 0; i < w.Length; i++)
					w[i] = v;
			}
			public int this[int i] {
				[MethodImpl(MethodImplOptions.AggressiveInlining)]
				get {
					return (int)(w[((uint)i) >> POT] >> i) & 1;
				}
				[MethodImpl(MethodImplOptions.AggressiveInlining)]
				set {
					if (value == 1) w[(((uint)i) >> POT)] |= (1L << i);
					else w[(((uint)i) >> POT)] &= ~(1L << i);
				}
			}
			public void or(int si, FastBitSet oth, int osi, int len) {
				int shift = si - osi;
				long k = oth.w[osi >> POT];
				k = (k << shift) | (((uint)k) >> (BITS - shift));
				if ((osi & MASK) + len <= BITS) {           // dont think this is needed
					osi += shift;
					for (int i = 0; i < len; i++, osi++, si++)
						w[((uint)si) >> POT] |= k & (1L << osi);
				}
				else
					for (int i = 0; i < len; i++, osi++, si++) {
						if ((osi & MASK) == 0) {
							k = oth.w[(osi) >> POT];
							k = (k << shift) | (((uint)k) >> (BITS - shift));
						}
						w[((uint)si) >> POT] |= k & (1L << (osi + shift));
					}
			}
		}
		class StreamReader {
			private byte[] data;
			private int pos = 0;
			public StreamReader(byte[] data) {
				this.data = data;
			}
			public byte readByte() {
				byte bite = data[pos++];// (byte)(data[bytePointer++] & 255); //????????????????????& 255
				return bite;
			}
			public int readNum(int b) {
				int ret = 0;
				for (int i = 0; i < b; i++)
					ret = (ret << 8) | data[pos++];
				return ret;
			}
			public void readByte(byte[] buf) {
				for (int i = 0, len = buf.Length; i < len; i++)
					buf[i] = data[pos++];
			}
			public bool isFinished() {
				return pos == data.Length;
			}
			public int move(int inc) {
				return pos += inc;
			}
		}
		class JBIG2Bitmap {
			private int width, height, line, bitNum;
			public FastBitSet data; // BitArray
			private ArithmeticDecoder decArithm;
			private MMRDecoder decMMR;

			public JBIG2Bitmap(int width, int height, ArithmeticDecoder arithmeticDecoder, MMRDecoder mmrDecoder) {
				this.width = width;
				this.height = height;
				this.decArithm = arithmeticDecoder;
				this.decMMR = mmrDecoder;
				this.line = (width + 7) >> 3;
				this.data = new FastBitSet(width * height);
			}
			public int Width() { return width; }
			public int Height() { return height; }
			public void clear(int def) { data.setAll(def == 1); }

			public void readBitmap(bool useMMR, int template, bool fTypPredGenDec, bool useSkip, JBIG2Bitmap skipBitmap, short[] adaptTmpltX, short[] adaptTmpltY, int mmrDatLen) {
				if (useMMR) {
					decMMR.reset();
					int[] lnRef = new int[width + 2];
					int[] lnCode = new int[width + 2];
					lnCode[0] = lnCode[1] = width;
					for (int row = 0; row < height; row++) {
						int i = 0;
						for (; lnCode[i] < width; i++)
							lnRef[i] = lnCode[i];
						lnRef[i] = lnRef[i + 1] = width;
						int refI = 0, codingI = 0, a0 = 0;
						do {
							int code1 = decMMR.get2DCode(), code2, code3;
							switch (code1) {
								case MMRDecoder.Dim2Pass:
									if (lnRef[refI] < width) {
										a0 = lnRef[refI + 1];
										refI += 2;
									}
									break;
								case MMRDecoder.Dim2Hor:
									if ((codingI & 1) != 0) {
										for (code1 = 0, code3 = 99; code3 >= 64;)
											code1 += code3 = decMMR.getBlackCode();
										for (code2 = 0, code3 = 99; code3 >= 64;)
											code2 += code3 = decMMR.getWhiteCode();
									}
									else {
										for (code1 = 0, code3 = 99; code3 >= 64;)
											code1 += code3 = decMMR.getWhiteCode();
										for (code2 = 0, code3 = 99; code3 >= 64;)
											code2 += code3 = decMMR.getBlackCode();
									}
									if (code1 > 0 || code2 > 0) {
										a0 = lnCode[codingI++] = a0 + code1;
										a0 = lnCode[codingI++] = a0 + code2;
										while (lnRef[refI] <= a0 && lnRef[refI] < width)
											refI += 2;
									}
									break;
								case MMRDecoder.Dim2Ver0:
									a0 = lnCode[codingI++] = lnRef[refI];
									if (lnRef[refI] < width) refI++;
									break;
								case MMRDecoder.Dim2VerR1:
									a0 = lnCode[codingI++] = lnRef[refI] + 1;
									if (lnRef[refI] < width)
										for (refI++; lnRef[refI] <= a0 && lnRef[refI] < width;)
											refI += 2;
									break;
								case MMRDecoder.Dim2VerR2:
									a0 = lnCode[codingI++] = lnRef[refI] + 2;
									if (lnRef[refI] < width)
										for (refI++; lnRef[refI] <= a0 && lnRef[refI] < width;)
											refI += 2;
									break;
								case MMRDecoder.Dim2VerR3:
									a0 = lnCode[codingI++] = lnRef[refI] + 3;
									if (lnRef[refI] < width)
										for (refI++; lnRef[refI] <= a0 && lnRef[refI] < width;)
											refI += 2;
									break;
								case MMRDecoder.Dim2VerL1:
									a0 = lnCode[codingI++] = lnRef[refI] - 1;
									if (refI > 0) refI--;
									else refI++;
									while (lnRef[refI] <= a0 && lnRef[refI] < width)
										refI += 2;
									break;
								case MMRDecoder.Dim2VerL2:
									a0 = lnCode[codingI++] = lnRef[refI] - 2;
									if (refI > 0) refI--;
									else refI++;
									while (lnRef[refI] <= a0 && lnRef[refI] < width)
										refI += 2;
									break;
								case MMRDecoder.Dim2VerL3:
									a0 = lnCode[codingI++] = lnRef[refI] - 3;
									if (refI > 0) refI--;
									else refI++;
									while (lnRef[refI] <= a0 && lnRef[refI] < width)
										refI += 2;
									break;
							}
						} while (a0 < width);
						lnCode[codingI++] = width;
						for (int j = 0; lnCode[j] < width; j += 2)
							for (int col = lnCode[j]; col < lnCode[j + 1]; col++)
								setPixel(col, row, 1);
					}
					if (mmrDatLen >= 0)
						decMMR.skipTo(mmrDatLen);
				}
				else {
					BitmapPointer cxPtr0 = new BitmapPointer(this), cxPtr1 = new BitmapPointer(this);
					BitmapPointer atPtr0 = new BitmapPointer(this), atPtr1 = new BitmapPointer(this),
								  atPtr2 = new BitmapPointer(this), atPtr3 = new BitmapPointer(this);
					long ltpCX = 0, cx, cx0, cx1, cx2;
					if (fTypPredGenDec)
						switch (template) {
							case 0: ltpCX = 0x3953; break;
							case 1: ltpCX = 0x079a; break;
							case 2: ltpCX = 0x0e3; break;
							case 3: ltpCX = 0x18a; break;
						}
					bool ltp = false;
					for (int row = 0; row < height; row++) {
						if (fTypPredGenDec) {
							int bit = decArithm.decodeBit(ltpCX, decArithm.genRegStats);
							if (bit != 0) ltp = !ltp;
							if (ltp) {
								for (int i = 0, r = row - 1; i < width; i++)
									setPixel(i, row, getPixel(i, r));
								continue;
							}
						}
						int pixel;
						switch (template) {
							case 0:
								cxPtr0.setPointer(0, row - 2);
								cx0 = (cxPtr0.nextPixel() << 1) | cxPtr0.nextPixel();
								cxPtr1.setPointer(0, row - 1);
								cx1 = (cxPtr1.nextPixel() << 2) | (cxPtr1.nextPixel() << 1) | cxPtr1.nextPixel();
								cx2 = 0;
								atPtr0.setPointer(adaptTmpltX[0], row + adaptTmpltY[0]);
								atPtr1.setPointer(adaptTmpltX[1], row + adaptTmpltY[1]);
								atPtr2.setPointer(adaptTmpltX[2], row + adaptTmpltY[2]);
								atPtr3.setPointer(adaptTmpltX[3], row + adaptTmpltY[3]);
								for (int col = 0; col < width; col++) {
									// SLOW!!!!!!
									cx = (cx0 << 13) | (cx1 << 8) | (cx2 << 4) | (atPtr0.nextPixel() << 3) | (atPtr1.nextPixel() << 2) | (atPtr2.nextPixel() << 1) | atPtr3.nextPixel();
									if (useSkip && skipBitmap.getPixel(col, row) != 0)
										pixel = 0;
									else {
										pixel = decArithm.decodeBit(cx, decArithm.genRegStats);
										if (pixel != 0)
											data[row * width + col] = 1;
									}
									cx0 = ((cx0 << 1) | cxPtr0.nextPixel()) & 0x07;
									cx1 = ((cx1 << 1) | cxPtr1.nextPixel()) & 0x1f;
									cx2 = ((cx2 << 1) | pixel) & 0x0f;
								}
								break;
							case 1:
								cxPtr0.setPointer(0, row - 2);
								cx0 = (cxPtr0.nextPixel() << 2) | (cxPtr0.nextPixel() << 1) | cxPtr0.nextPixel();
								cxPtr1.setPointer(0, row - 1);
								cx1 = (cxPtr1.nextPixel() << 2) | (cxPtr1.nextPixel() << 1) | cxPtr1.nextPixel();
								cx2 = 0;
								atPtr0.setPointer(adaptTmpltX[0], row + adaptTmpltY[0]);
								for (int col = 0; col < width; col++) {
									cx = (cx0 << 9) | (cx1 << 4) | (cx2 << 1) | atPtr0.nextPixel();
									if (useSkip && skipBitmap.getPixel(col, row) != 0)
										pixel = 0;
									else {
										pixel = decArithm.decodeBit(cx, decArithm.genRegStats);
										if (pixel != 0)
											data[row * width + col] = 1;
									}
									cx0 = ((cx0 << 1) | cxPtr0.nextPixel()) & 0x0f;
									cx1 = ((cx1 << 1) | cxPtr1.nextPixel()) & 0x1f;
									cx2 = ((cx2 << 1) | pixel) & 0x07;
								}
								break;
							case 2:
								cxPtr0.setPointer(0, row - 2);
								cx0 = (cxPtr0.nextPixel() << 1) | cxPtr0.nextPixel();
								cxPtr1.setPointer(0, row - 1);
								cx1 = (cxPtr1.nextPixel() << 1) | cxPtr1.nextPixel();
								cx2 = 0;
								atPtr0.setPointer(adaptTmpltX[0], row + adaptTmpltY[0]);
								for (int col = 0; col < width; col++) {
									cx = (cx0 << 7) | (cx1 << 3) | (cx2 << 1) | atPtr0.nextPixel();
									if (useSkip && skipBitmap.getPixel(col, row) != 0)
										pixel = 0;
									else {
										pixel = decArithm.decodeBit(cx, decArithm.genRegStats);
										if (pixel != 0)
											data[row * width + col] = 1;
									}
									cx0 = ((cx0 << 1) | cxPtr0.nextPixel()) & 0x07;
									cx1 = ((cx1 << 1) | cxPtr1.nextPixel()) & 0x0f;
									cx2 = ((cx2 << 1) | pixel) & 0x03;
								}
								break;
							case 3:
								cxPtr1.setPointer(0, row - 1);
								cx1 = (cxPtr1.nextPixel() << 1) | cxPtr1.nextPixel();
								cx2 = 0;
								atPtr0.setPointer(adaptTmpltX[0], row + adaptTmpltY[0]);
								for (int col = 0; col < width; col++) {
									cx = (cx1 << 5) | (cx2 << 1) | atPtr0.nextPixel();
									if (useSkip && skipBitmap.getPixel(col, row) != 0)
										pixel = 0;
									else {
										pixel = decArithm.decodeBit(cx, decArithm.genRegStats);
										if (pixel != 0)
											data[row * width + col] = 1;
									}
									cx1 = ((cx1 << 1) | cxPtr1.nextPixel()) & 0x1f;
									cx2 = ((cx2 << 1) | pixel) & 0x0f;
								}
								break;
						}
					}
				}
			}

			private void setPixel(int col, int row, FastBitSet oth, int value) {
				oth[(row * width) + col] = value;
			}
			public void setPixel(int col, int row, int value) {
				setPixel(col, row, data, value);
			}
			public int getPixel(int col, int row) {
				return data[(row * width) + col];
			}

			public void expand(int newHeight, int defaultPixel) {
				FastBitSet newData = new FastBitSet(newHeight * width);
				for (int row = 0; row < height; row++)
					for (int col = 0; col < width; col++)
						setPixel(col, row, newData, getPixel(col, row));
				this.height = newHeight;
				this.data = newData;
			}

			public void combine(JBIG2Bitmap bitmap, int x, int y, long combOp) {
				int srcWidth = bitmap.width, srcHeight = bitmap.height, minWidth = srcWidth;
				if (x + srcWidth > width)           //Should not happen but occurs sometimes because 
					minWidth = width - x;           //there is something wrong with halftone pics
				if (y + srcHeight > height)         //Should not happen but occurs sometimes because 
					srcHeight = height - y;         //there is something wrong with halftone pics
				int srcIndx = 0, indx = y * width + x;
				if (combOp == 0) {
					if (x == 0 && y == 0 && srcHeight == height && srcWidth == width)
						for (int i = 0; i < data.w.Length; i++)
							data.w[i] |= bitmap.data.w[i];
					for (int row = y; row < y + srcHeight; row++, srcIndx += srcWidth)
						data.or(indx = row * width + x, bitmap.data, srcIndx, minWidth);
				}
				else if (combOp == 1) {
					if (x == 0 && y == 0 && srcHeight == height && srcWidth == width)
						for (int i = 0; i < data.w.Length; i++)
							data.w[i] &= bitmap.data.w[i];
					for (int row = y; row < y + srcHeight; row++, srcIndx += srcWidth) {
						indx = row * width + x;
						for (int col = 0; col < minWidth; col++, indx++)
							data[indx] = bitmap.data[srcIndx + col] * data[indx];
					}
				}
				else if (combOp == 2) {
					if (x == 0 && y == 0 && srcHeight == height && srcWidth == width)
						for (int i = 0; i < data.w.Length; i++)
							data.w[i] ^= bitmap.data.w[i];
					else
						for (int row = y; row < y + srcHeight; row++, srcIndx += srcWidth) {
							indx = row * width + x;
							for (int col = 0; col < minWidth; col++, indx++)
								data[indx] = bitmap.data[srcIndx + col] ^ data[indx];
						}
				}
				else if (combOp == 3)
					for (int row = y; row < y + srcHeight; row++, srcIndx += srcWidth) {
						indx = row * width + x;
						for (int col = 0; col < minWidth; col++, indx++)
							data[indx] = (bitmap.data[srcIndx + col] == data[indx]) ? 1 : 0;
					}
				else if (combOp == 4) {
					if (x == 0 && y == 0 && srcHeight == height && srcWidth == width)
						for (int i = 0; i < data.w.Length; i++)
							data.w[i] = bitmap.data.w[i];
					else
						for (int row = y; row < y + srcHeight; row++, srcIndx += srcWidth) {
							indx = row * width + x;
							for (int col = 0; col < minWidth; col++, srcIndx++, indx++)
								data[indx] = bitmap.data[srcIndx + col];
						}
				}
			}

			public byte[] getData(bool switchPixelColor) {
				byte[] bytes = new byte[height * line];
				long k = 0;
				for (int ps = 0, pd = 0, r = 0; r < height; pd = ++r * line * 8)
					for (int c = 0; c < width; c++, ps++, pd++) {
						if ((ps & FastBitSet.MASK) == 0)                // cache 1 long at a time.
							k = data.w[((uint)ps) >> FastBitSet.POT];
						int b = (int)((((ulong)k) >> ps) & 1);
						bytes[pd >> 3] |= (byte)(b << (7 - (pd & 0x7)));
					}
				if (switchPixelColor)
					for (int i = 0; i < bytes.Length; i++)
						bytes[i] ^= 0xff;
				return bytes;
			}

			internal void setBmpNum(int seg) {
				this.bitNum = seg;
			}
		}
		class BitmapPointer {
			private int x, y, width, height, count;
			private bool output;
			private JBIG2Bitmap bitmap;
			public BitmapPointer(JBIG2Bitmap bmp) {
				bitmap = bmp;
				height = bmp.Height();
				width = bmp.Width();
			}
			public void setPointer(int c, int r) {
				x = c; y = r; count = r * width;
				output = (r >= 0 && r < height && c < width);
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			public int nextPixel() {
				if (!output)
					return 0;
				if (x < 0 || x >= width) {
					x++;
					return 0;
				}
				int i = count + x++;
				//return bitmap.data[i];		// 25% faster to inline.
				return (int)(bitmap.data.w[((uint)i) >> FastBitSet.POT] >> i) & 1;
			}
		}
		abstract class Flags {
			protected int flagsAsInt;
			protected Dictionary<string, int> flags = new Dictionary<string, int>();
			public int getFlagValue(String key) {
				int value = 0;
				flags.TryGetValue(key, out value);
				return value;
			}
			public abstract void setFlags(int flagsAsInt);
		}
		class SegmentHeader {
			private int segmentType;
			public int SegNum, RefSegCnt, DataLen, PgAssoc;
			private bool pageAssociationSizeSet;
			public void setSegmentHeaderFlags(short SegmentHeaderFlags) {
				segmentType = SegmentHeaderFlags & 63; // 63 = 00111111
				pageAssociationSizeSet = (SegmentHeaderFlags & 64) == 64; // 64 = // 01000000
			}
			public byte[] RetFlgs { set; get; }
			public int[] RefSegs { get; set; }
			public bool isPageAssociationSizeSet() {
				return pageAssociationSizeSet;
			}
			public int getSegmentType() {
				return segmentType;
			}
		}
		abstract class Segment {
			public const int INTRM_GEN_REG = 36, IMM_GEN_REG = 38, IMM_LL_GEN_REG = 39, PAGE_INFO = 48,
								END_OF_PAGE = 49, END_OF_FILE = 51, PROFILES = 52, TABLES = 53;
			protected SegmentHeader hdr;
			protected ArithmeticDecoder defArithm;
			protected MMRDecoder decMMR;
			protected JBIG2Decoder decoder;

			public Segment(JBIG2Decoder streamDecoder) {
				decoder = streamDecoder;
				defArithm = decoder.getArithmeticDecoder();
				decMMR = decoder.getMMRDecoder();
			}

			public abstract void readSegment();
			public SegmentHeader SegHdr { get { return hdr; } set { hdr = value; } }
			protected short readATValue() {
				short atValue;
				short c0 = atValue = decoder.readByte();
				if ((c0 & 0x80) != 0)
					atValue |= -1 - 0xff;
				return atValue;
			}
		}
		abstract class RegionSegment : Segment {
			internal class RegionFlags : Flags {
				public static String XCOMBO_OP = "EXTERNAL_COMBINATION_OPERATOR";
				public override void setFlags(int flagsAsInt) {
					this.flagsAsInt = flagsAsInt;
					flags.Add(XCOMBO_OP, flagsAsInt & 7);

				}
			}
			protected int regBmpWidth, regBmpHeight, regBmpX, regBmpY;
			protected RegionFlags regionFlags = new RegionFlags();
			public RegionSegment(JBIG2Decoder streamDecoder) : base(streamDecoder) { }
			public override void readSegment() {
				regBmpWidth = decoder.readNum(4);
				regBmpHeight = decoder.readNum(4);
				regBmpX = decoder.readNum(4);
				regBmpY = decoder.readNum(4);
				regionFlags.setFlags(decoder.readByte());
			}
		}
		class PageInfoSeg : Segment {
			public static String PX_VAL = "DEFAULT_PIXEL_VALUE", COMBO_OP = "DEFAULT_COMBINATION_OPERATOR";
			class PageInfoFlags : Flags {
				public override void setFlags(int flagsAsInt) {
					this.flagsAsInt = flagsAsInt;
					flags.Add(PX_VAL, (flagsAsInt >> 2) & 1);
					flags.Add(COMBO_OP, (flagsAsInt >> 3) & 3);
				}
			}
			PageInfoFlags info = new PageInfoFlags();
			JBIG2Bitmap bmp;
			public PageInfoSeg(JBIG2Decoder streamDecoder) : base(streamDecoder) { }
			public Flags Flags { get { return info; } }
			public JBIG2Bitmap Bitmap { get { return bmp; } }

			public override void readSegment() {
				int width = decoder.readNum(4), height = decoder.readNum(4);
				decoder.readNum(4); decoder.readNum(4); // x/y resolution
				info.setFlags(decoder.readByte());
				int stripping = decoder.readNum(2);
				int defPix = info.getFlagValue(PageInfoSeg.PX_VAL);
				if (height == -1) height = (stripping & 0x7fff);
				bmp = new JBIG2Bitmap(width, height, defArithm, decMMR);
				bmp.clear(defPix);
			}
		}
		class GenRegSeg : RegionSegment {
			class GenericRegionFlags : Flags {
				public const String MMR = "MMR", GB = "GB_TEMPLATE", TPGDON = "TPGDON";
				public override void setFlags(int flagsAsInt) {
					this.flagsAsInt = flagsAsInt;
					flags.Add(MMR, flagsAsInt & 1);             /** extract MMR */
					flags.Add(GB, (flagsAsInt >> 1) & 3);           /** extract GB_TEMPLATE */
					flags.Add(TPGDON, (flagsAsInt >> 3) & 1);       /** extract TPGDON */
				}
			}
			GenericRegionFlags regFlg = new GenericRegionFlags();
			bool inlineImage, unknownLength = false;

			public GenRegSeg(JBIG2Decoder streamDecoder, bool inlineImage) : base(streamDecoder) {
				this.inlineImage = inlineImage;
			}

			public override void readSegment() {
				base.readSegment();
				readGenericRegionFlags();
				bool useMMR = regFlg.getFlagValue(GenericRegionFlags.MMR) != 0;
				int template = regFlg.getFlagValue(GenericRegionFlags.GB);
				short[] tmpltX = new short[4]; //??? short or byte
				short[] tmpltY = new short[4];
				if (!useMMR) {
					tmpltX[0] = readATValue();
					tmpltY[0] = readATValue();
					if (template == 0) {
						tmpltX[1] = readATValue(); tmpltY[1] = readATValue();
						tmpltX[2] = readATValue(); tmpltY[2] = readATValue();
						tmpltX[3] = readATValue(); tmpltY[3] = readATValue();
					}
					defArithm.resetGenericStats(template, null);
					defArithm.start();
				}
				bool decodeOn = regFlg.getFlagValue(GenericRegionFlags.TPGDON) != 0;
				int length = hdr.DataLen, bytesRead = 0;
				if (length == -1) {
					// length of data is unknown, so it needs to be determined through examination of the data.
					// See 7.2.7 - Segment data length of the JBIG2 specification.
					unknownLength = true;
					byte match1, match2;
					if (useMMR)         // look for 0x00 0x00 (0, 0)
						match1 = match2 = 0;
					else {              // look for 0xFF 0xAC (255, 172)
						match1 = 255;
						match2 = 172;
					}
					while (true) {
						byte tmp = decoder.readByte(); bytesRead++;
						if (tmp == match1) {
							tmp = decoder.readByte(); bytesRead++;
							if (tmp == match2) {
								length = bytesRead - 2;
								break;
							}
						}
					}
					decoder.movePointer(-bytesRead);
				}
				JBIG2Bitmap bitmap = new JBIG2Bitmap(regBmpWidth, regBmpHeight, defArithm, decMMR);
				bitmap.clear(0);
				bitmap.readBitmap(useMMR, template, decodeOn, false, null, tmpltX, tmpltY, useMMR ? bytesRead : length - 18);
				if (inlineImage) {
					PageInfoSeg seg = decoder.findPageSegement(hdr.PgAssoc);
					JBIG2Bitmap bmp = seg.Bitmap;
					int extCombOp = regionFlags.getFlagValue(RegionFlags.XCOMBO_OP);
					if (bmp.Height() == -1 && regBmpY + regBmpHeight > bmp.Height()) {
						bmp.expand(regBmpY + regBmpHeight,
							seg.Flags.getFlagValue(PageInfoSeg.PX_VAL));
					}
					bmp.combine(bitmap, regBmpX, regBmpY, extCombOp);
				}
				else {
					bitmap.setBmpNum(this.SegHdr.SegNum);
					decoder.appendBitmap(bitmap);
				}
				if (unknownLength)
					decoder.movePointer(4);
			}
			private void readGenericRegionFlags() {
				regFlg.setFlags(decoder.readByte());
			}
		}
		class MMRDecoder {
			public const int ccittEndOfLine = -2, Dim2Pass = 0, Dim2Hor = 1, Dim2Ver0 = 2,
				Dim2VerR1 = 3, Dim2VerL1 = 4, Dim2VerR2 = 5, Dim2VerL2 = 6, Dim2VerR3 = 7, Dim2VerL3 = 8;
			private int[][] tbl2Dim1 = {
			 new int []{ -1, -1 },  new int []{ -1, -1 },  new int []{ 7, Dim2VerL3 },  new int []{ 7, Dim2VerR3 }, new int []{ 6, Dim2VerL2 }, new int []{ 6, Dim2VerL2 },  new int []{ 6, Dim2VerR2 },  new int [] { 6, Dim2VerR2 },  new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 4, Dim2Pass },       new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor },  new int [] { 3, Dim2Hor },  new int [] { 3, Dim2Hor },
			 new int []{ 3, Dim2Hor },  new int []{ 3, Dim2Hor },  new int []{ 3, Dim2Hor }, new int []{ 3, Dim2Hor }, new int []{ 3, Dim2Hor }, new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor },  new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor }, new int [] { 3, Dim2Hor }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 },  new int [] { 3, Dim2VerL1 },  new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 },
			 new int []{ 3, Dim2VerL1 }, new int []{ 3, Dim2VerL1 },  new int []{ 3, Dim2VerL1 }, new int []{ 3, Dim2VerL1 }, new int []{ 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerL1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 },
			 new int []{ 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 }, new int [] { 3, Dim2VerR1 },  new int []{ 3, Dim2VerR1 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 },
			 new int []{ 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 },
			 new int []{ 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 },
			 new int []{ 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }, new int [] { 1, Dim2Ver0 }
		};
			/** white run lengths */
			static readonly private int[][] whtTbl1 = { new int[] { -1, -1 }, new int[] { 12, ccittEndOfLine }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { -1, -1 }, new int[] { 11, 1792 }, new int[] { 11, 1792 }, new int[] { 12, 1984 }, new int[] { 12, 2048 }, new int[] { 12, 2112 }, new int[] { 12, 2176 }, new int[] { 12, 2240 }, new int[] { 12, 2304 }, new int[] { 11, 1856 }, new int[] { 11, 1856 }, new int[] { 11, 1920 }, new int[] { 11, 1920 }, new int[] { 12, 2368 }, new int[] { 12, 2432 }, new int[] { 12, 2496 }, new int[] { 12, 2560 } };
			static readonly private int[][] whtTbl2 = { new int []{ -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { 8, 29 }, new int [] { 8, 29 }, new int [] { 8, 30 }, new int [] { 8, 30 }, new int [] { 8, 45 }, new int [] { 8, 45 }, new int [] { 8, 46 }, new int [] { 8, 46 }, new int [] { 7, 22 }, new int [] { 7, 22 }, new int [] { 7, 22 }, new int [] { 7, 22 }, new int [] { 7, 23 }, new int [] { 7, 23 }, new int [] { 7, 23 }, new int [] { 7, 23 }, new int [] { 8, 47 }, new int [] { 8, 47 }, new int [] { 8, 48 }, new int [] { 8, 48 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 6, 13 }, new int [] { 7, 20 }, new int [] { 7, 20 }, new int [] { 7, 20 }, new int [] { 7, 20 }, new int [] { 8, 33 }, new int [] { 8, 33 }, new int [] { 8, 34 }, new int [] { 8, 34 }, new int [] { 8, 35 }, new int [] { 8, 35 }, new int [] { 8, 36 }, new int [] { 8, 36 }, new int [] { 8, 37 }, new int [] { 8, 37 }, new int [] { 8, 38 }, new int [] { 8, 38 }, new int [] { 7, 19 }, new int [] { 7, 19 }, new int []
			{ 7, 19 }, new int [] { 7, 19 }, new int [] { 8, 31 }, new int [] { 8, 31 }, new int [] { 8, 32 }, new int [] { 8, 32 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 1 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 6, 12 }, new int [] { 8, 53 }, new int [] { 8, 53 }, new int [] { 8, 54 }, new int [] { 8, 54 }, new int [] { 7, 26 }, new int [] { 7, 26 }, new int [] { 7, 26 }, new int [] { 7, 26 }, new int [] { 8, 39 }, new int [] { 8, 39 }, new int [] { 8, 40 }, new int [] { 8, 40 }, new int [] { 8, 41 }, new int [] { 8, 41 }, new int [] { 8, 42 }, new int [] { 8, 42 }, new int [] { 8, 43 }, new int [] { 8, 43 }, new int [] { 8, 44 }, new int [] { 8, 44 }, new int [] { 7, 21 }, new int [] { 7, 21 }, new int [] { 7, 21 }, new int [] { 7, 21 }, new int [] { 7, 28 }, new int [] { 7, 28 }, new int [] { 7, 28 }, new int [] { 7, 28 }, new int [] { 8, 61 }, new int [] { 8, 61 }, new int []
			{ 8, 62 }, new int [] { 8, 62 }, new int [] { 8, 63 }, new int [] { 8, 63 }, new int [] { 8, 0 }, new int [] { 8, 0 }, new int [] { 8, 320 }, new int [] { 8, 320 }, new int [] { 8, 384 }, new int [] { 8, 384 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 10 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 5, 11 }, new int [] { 7, 27 }, new int [] { 7, 27 }, new int [] { 7, 27 }, new int [] { 7, 27 }, new int [] { 8, 59 }, new int [] { 8, 59 }, new int [] { 8, 60 }, new int [] { 8, 60 }, new int [] { 9, 1472 }, new int []
			{ 9, 1536 }, new int [] { 9, 1600 }, new int [] { 9, 1728 }, new int [] { 7, 18 }, new int [] { 7, 18 }, new int [] { 7, 18 }, new int [] { 7, 18 }, new int [] { 7, 24 }, new int [] { 7, 24 }, new int [] { 7, 24 }, new int [] { 7, 24 }, new int [] { 8, 49 }, new int [] { 8, 49 }, new int [] { 8, 50 }, new int [] { 8, 50 }, new int [] { 8, 51 }, new int [] { 8, 51 }, new int [] { 8, 52 }, new int [] { 8, 52 }, new int [] { 7, 25 }, new int [] { 7, 25 }, new int [] { 7, 25 }, new int [] { 7, 25 }, new int [] { 8, 55 }, new int [] { 8, 55 }, new int [] { 8, 56 }, new int [] { 8, 56 }, new int [] { 8, 57 }, new int [] { 8, 57 }, new int [] { 8, 58 }, new int [] { 8, 58 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 192 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 6, 1664 }, new int [] { 8, 448 }, new int []
			{ 8, 448 }, new int [] { 8, 512 }, new int [] { 8, 512 }, new int [] { 9, 704 }, new int [] { 9, 768 }, new int [] { 8, 640 }, new int [] { 8, 640 }, new int [] { 8, 576 }, new int [] { 8, 576 }, new int [] { 9, 832 }, new int [] { 9, 896 }, new int [] { 9, 960 }, new int [] { 9, 1024 }, new int [] { 9, 1088 }, new int [] { 9, 1152 }, new int [] { 9, 1216 }, new int [] { 9, 1280 }, new int [] { 9, 1344 }, new int [] { 9, 1408 }, new int [] { 7, 256 }, new int [] { 7, 256 }, new int [] { 7, 256 }, new int [] { 7, 256 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int []
			{ 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 2 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 4, 3 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 128 }, new int [] { 5, 8 }, new int []
			{ 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 8 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 5, 9 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 16 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 6, 17 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int []
			{ 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 4 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int []
			{ 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 14 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 6, 15 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 5, 64 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int []
			{ 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 }, new int [] { 4, 7 } };
			/** black run lengths */
			static readonly int[][] blkTbl1 = { new int []{ -1, -1 }, new int [] { -1, -1 }, new int [] { 12, ccittEndOfLine }, new int [] { 12, ccittEndOfLine }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { 11, 1792 }, new int [] { 11, 1792 }, new int [] { 11, 1792 }, new int [] { 11, 1792 }, new int [] { 12, 1984 }, new int [] { 12, 1984 }, new int [] { 12, 2048 }, new int [] { 12, 2048 }, new int [] { 12, 2112 }, new int [] { 12, 2112 }, new int []
			{ 12, 2176 }, new int [] { 12, 2176 }, new int [] { 12, 2240 }, new int [] { 12, 2240 }, new int [] { 12, 2304 }, new int [] { 12, 2304 }, new int [] { 11, 1856 }, new int [] { 11, 1856 }, new int [] { 11, 1856 }, new int [] { 11, 1856 }, new int [] { 11, 1920 }, new int [] { 11, 1920 }, new int [] { 11, 1920 }, new int [] { 11, 1920 }, new int [] { 12, 2368 }, new int [] { 12, 2368 }, new int [] { 12, 2432 }, new int [] { 12, 2432 }, new int [] { 12, 2496 }, new int [] { 12, 2496 }, new int [] { 12, 2560 }, new int [] { 12, 2560 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 10, 18 }, new int [] { 12, 52 }, new int [] { 12, 52 }, new int [] { 13, 640 }, new int [] { 13, 704 }, new int [] { 13, 768 }, new int [] { 13, 832 }, new int [] { 12, 55 }, new int [] { 12, 55 }, new int [] { 12, 56 }, new int [] { 12, 56 }, new int [] { 13, 1280 }, new int [] { 13, 1344 }, new int []
			{ 13, 1408 }, new int [] { 13, 1472 }, new int [] { 12, 59 }, new int [] { 12, 59 }, new int [] { 12, 60 }, new int [] { 12, 60 }, new int [] { 13, 1536 }, new int [] { 13, 1600 }, new int [] { 11, 24 }, new int [] { 11, 24 }, new int [] { 11, 24 }, new int [] { 11, 24 }, new int [] { 11, 25 }, new int [] { 11, 25 }, new int [] { 11, 25 }, new int [] { 11, 25 }, new int [] { 13, 1664 }, new int [] { 13, 1728 }, new int [] { 12, 320 }, new int [] { 12, 320 }, new int [] { 12, 384 }, new int [] { 12, 384 }, new int [] { 12, 448 }, new int [] { 12, 448 }, new int [] { 13, 512 }, new int [] { 13, 576 }, new int [] { 12, 53 }, new int [] { 12, 53 }, new int [] { 12, 54 }, new int [] { 12, 54 }, new int [] { 13, 896 }, new int [] { 13, 960 }, new int [] { 13, 1024 }, new int [] { 13, 1088 }, new int [] { 13, 1152 }, new int [] { 13, 1216 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 }, new int [] { 10, 64 } };
			static readonly int[][] blkTbl2 = { new int []{ 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 8, 13 }, new int [] { 11, 23 }, new int [] { 11, 23 }, new int [] { 12, 50 }, new int [] { 12, 51 }, new int [] { 12, 44 }, new int [] { 12, 45 }, new int [] { 12, 46 }, new int [] { 12, 47 }, new int [] { 12, 57 }, new int [] { 12, 58 }, new int [] { 12, 61 }, new int [] { 12, 256 }, new int [] { 10, 16 }, new int [] { 10, 16 }, new int [] { 10, 16 }, new int [] { 10, 16 }, new int [] { 10, 17 }, new int [] { 10, 17 }, new int [] { 10, 17 }, new int [] { 10, 17 }, new int [] { 12, 48 }, new int [] { 12, 49 }, new int [] { 12, 62 }, new int [] { 12, 63 }, new int [] { 12, 30 }, new int [] { 12, 31 }, new int [] { 12, 32 }, new int [] { 12, 33 }, new int [] { 12, 40 }, new int [] { 12, 41 }, new int [] { 11, 22 }, new int []
			{ 11, 22 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 8, 14 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 10 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int []
			{ 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 7, 11 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 9, 15 }, new int [] { 12, 128 }, new int [] { 12, 192 }, new int [] { 12, 26 }, new int [] { 12, 27 }, new int [] { 12, 28 }, new int [] { 12, 29 }, new int [] { 11, 19 }, new int [] { 11, 19 }, new int [] { 11, 20 }, new int [] { 11, 20 }, new int [] { 12, 34 }, new int [] { 12, 35 }, new int []
			{ 12, 36 }, new int [] { 12, 37 }, new int [] { 12, 38 }, new int [] { 12, 39 }, new int [] { 11, 21 }, new int [] { 11, 21 }, new int [] { 12, 42 }, new int [] { 12, 43 }, new int [] { 10, 0 }, new int [] { 10, 0 }, new int [] { 10, 0 }, new int [] { 10, 0 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 }, new int [] { 7, 12 } };
			static readonly int[][] blkTbl3 = { new int []{ -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { -1, -1 }, new int [] { 6, 9 }, new int [] { 6, 8 }, new int [] { 5, 7 }, new int [] { 5, 7 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 6 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 4, 5 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 1 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 3, 4 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 3 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int []
			{ 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 }, new int [] { 2, 2 } };

			StreamReader reader;
			long bufLen = 0, buffer = 0, noOfBytesRead = 0;
			public MMRDecoder(StreamReader reader) { this.reader = reader; }
			public void reset() { bufLen = noOfBytesRead = buffer = 0; }
			public int get2DCode() {
				int[] tuple;
				if (bufLen == 0) {
					buffer = (reader.readByte() & 0xff);
					bufLen = 8;
					noOfBytesRead++;
					int lookup = (int)(((buffer >> 1)) & 0x7f);
					tuple = tbl2Dim1[lookup];
				}
				else if (bufLen == 8) {
					int lookup = (int)(((buffer >> 1)) & 0x7f);
					tuple = tbl2Dim1[lookup];
				}
				else {
					int lookup = (int)(((buffer << (int)(7 - bufLen))) & 0x7f);
					tuple = tbl2Dim1[lookup];
					if (tuple[0] < 0 || tuple[0] > (int)bufLen) {
						int right = reader.readByte() & 0xff;
						long left = buffer << 8;
						buffer = left | (long)right;
						bufLen += 8;
						noOfBytesRead++;
						int look = (int)((buffer >> (int)(bufLen - 7)) & 0x7f);
						tuple = tbl2Dim1[look];
					}
				}
				if (tuple[0] < 0)
					return 0;
				bufLen -= tuple[0];
				return tuple[1];
			}
			public int getBlackCode() {
				if (bufLen == 0) {
					buffer = (reader.readByte() & 0xff);
					bufLen = 8;
					noOfBytesRead++;
				}
				for (; true; bufLen += 8, noOfBytesRead++) {
					int[] tuple;
					long code;
					if (bufLen >= 6 && ((buffer >> (int)(bufLen - 6)) & 0x3f) == 0) {
						code = (bufLen <= 13) ? buffer << (int)(13 - bufLen) : buffer >> (int)(bufLen - 13);
						tuple = blkTbl1[(int)(code & 0x7f)];
					}
					else if (bufLen >= 4 && ((buffer >> (int)(bufLen - 4)) & 0x0f) == 0) {
						code = (bufLen <= 12) ? buffer << (int)(12 - bufLen) : buffer >> (int)(bufLen - 12);
						int k = (int)((code & 0xff) - 64);
						tuple = (k >= 0) ? blkTbl2[k] : blkTbl1[blkTbl1.Length + k];
					}
					else {
						code = (bufLen <= 6) ? (buffer << (int)(6 - bufLen)) : (buffer >> (int)(bufLen - 6));
						int k = (int)(code & 0x3f);
						tuple = (k >= 0) ? blkTbl3[k] : blkTbl2[blkTbl2.Length + k];
					}
					if (tuple[0] > 0 && tuple[0] <= (int)bufLen) {
						bufLen -= tuple[0];
						return tuple[1];
					}
					if (bufLen >= 13)
						break;
					buffer = (buffer << 8) | (reader.readByte() & 0xff);
				}
				bufLen--;
				return 1;
			}
			public int getWhiteCode() {
				int[] tuple;
				long code;
				if (bufLen == 0) {
					buffer = (reader.readByte() & 0xff);
					bufLen = 8;
					noOfBytesRead++;
				}
				for (; true; bufLen += 8, noOfBytesRead++) {
					if (bufLen >= 7 && (((buffer >> (int)(bufLen - 7))) & 0x7f) == 0) {
						code = (bufLen <= 12) ? (buffer << (int)(12 - bufLen)) : (buffer >> (int)(bufLen - 12));
						tuple = whtTbl1[(int)(code & 0x1f)];
					}
					else {
						code = (bufLen <= 9) ? (buffer << (int)(9 - bufLen)) : (buffer >> (int)(bufLen - 9));
						int k = (int)(code & 0x1ff);
						tuple = (k >= 0) ? whtTbl2[k] : whtTbl2[whtTbl2.Length + k];
					}
					if (tuple[0] > 0 && tuple[0] <= (int)bufLen) {
						bufLen -= tuple[0];
						return tuple[1];
					}
					if (bufLen >= 12)
						break;
					buffer = (buffer << 8) | reader.readByte() & 0xff;
				}
				bufLen--;
				return 1;
			}
			public long get24Bits() {
				for (; bufLen < 24; bufLen += 8, noOfBytesRead++)
					buffer = (buffer << 8) | (reader.readByte() & 0xff);
				return ((buffer >> (int)(bufLen - 24))) & 0xffffff;
			}
			public void skipTo(int length) {
				for (; noOfBytesRead < length; noOfBytesRead++)
					reader.readByte();
			}
		}
		class ArithmeticDecoder {
			private StreamReader reader;
			public int[] genRegStats;
			static readonly int[] contextSize = new int[] { 16, 13, 10, 10 };
			static readonly int[] tblQE = { 0x56010000, 0x34010000, 0x18010000, 0x0AC10000, 0x05210000, 0x02210000, 0x56010000, 0x54010000, 0x48010000, 0x38010000, 0x30010000, 0x24010000, 0x1C010000, 0x16010000, 0x56010000, 0x54010000, 0x51010000, 0x48010000, 0x38010000, 0x34010000, 0x30010000, 0x28010000, 0x24010000, 0x22010000, 0x1C010000, 0x18010000, 0x16010000, 0x14010000, 0x12010000, 0x11010000, 0x0AC10000, 0x09C10000, 0x08A10000, 0x05210000, 0x04410000, 0x02A10000, 0x02210000, 0x01410000, 0x01110000, 0x00850000, 0x00490000, 0x00250000, 0x00150000, 0x00090000, 0x00050000, 0x00010000, 0x56010000 };
			static readonly int[] tblNMPS = { 1, 2, 3, 4, 5, 38, 7, 8, 9, 10, 11, 12, 13, 29, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 45, 46 };
			static readonly int[] tblSwitch = { 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			static readonly int[] tblNLPS = { 1, 6, 9, 12, 29, 33, 6, 14, 14, 14, 17, 18, 20, 21, 14, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 46 };
			long buffer0, buffer1, c, a;
			int counter;
			public ArithmeticDecoder(StreamReader reader) {
				this.reader = reader;
				genRegStats = new int[1 << 1];
			}
			public void start() {
				buffer0 = reader.readByte();
				buffer1 = reader.readByte();
				c = ((buffer0 ^ 0xff) << 16);
				readByte();
				c <<= 7;
				counter -= 7;
				a = 0x80000000L;
			}
			private void readByte() {
				if (buffer0 != 0xff) {
					buffer0 = buffer1;
					buffer1 = reader.readByte();
					c += 0xff00 - (buffer0 << 8);
					counter = 8;
				}
				else if (buffer1 > 0x8f)
					counter = 8;
				else {
					buffer0 = buffer1;
					buffer1 = reader.readByte();
					c += 0xfe00 - (buffer0 << 9);
					counter = 7;
				}
			}
			internal void resetGenericStats(int template, int[] previousStats) {
				int size = contextSize[template];
				if (previousStats != null && previousStats.Length == size) {
					if (genRegStats.Length == size)
						Buffer.BlockCopy(previousStats, 0, genRegStats, 0, genRegStats.Length);
					else
						genRegStats = previousStats.ToArray();
				}
				else {
					if (genRegStats.Length == size)
						Array.Clear(genRegStats, 0, genRegStats.Length);
					else
						genRegStats = new int[1 << size];
				}
			}
			internal int decodeBit(long context, int[] stats) {
				int ctx = stats[(int)context], iCX = ctx >> 1, mpsCX = ctx & 1;
				int qe = tblQE[iCX], bit;
				a -= qe;
				if (c < a) {
					if ((a & 0x80000000) != 0)
						bit = mpsCX;
					else {
						if (a < qe) {
							bit = 1 - mpsCX;
							stats[(int)context] = (tblNLPS[iCX] << 1) | (tblSwitch[iCX] != 0 ? 1 - mpsCX : mpsCX);
						}
						else {
							bit = mpsCX;
							stats[(int)context] = (tblNMPS[iCX] << 1) | mpsCX;
						}
						do {
							if (counter == 0) readByte();
							a <<= 1; c <<= 1; counter--;
						} while ((a & 0x80000000) == 0);
					}
				}
				else {
					c -= a;
					if (a < qe) {
						bit = mpsCX;
						stats[(int)context] = (tblNMPS[iCX] << 1) | mpsCX;
					}
					else {
						bit = 1 - mpsCX;
						stats[(int)context] = (tblNLPS[iCX] << 1) | (tblSwitch[iCX] != 0 ? 1 - mpsCX : mpsCX);
					}
					a = qe;
					do {
						if (counter == 0) readByte();
						a <<= 1; c <<= 1; counter--;
					} while ((a & 0x80000000) == 0);
				}
				return bit;
			}
		}
		public class JBIG2Decoder {
			private StreamReader rdr;
			private bool noOfPagesKnown, randAxsOrg;
			private int noOfPages = -1;
			private List<Segment> segments = new List<Segment>();
			private List<JBIG2Bitmap> bitmaps = new List<JBIG2Bitmap>();
			private ArithmeticDecoder decArithm;
			private MMRDecoder decMMR;
			internal int movePointer(int i) {
				return rdr.move(i);
			}
			private void readSegments() {
				for (bool finished = false; !rdr.isFinished() && !finished;) {
					SegmentHeader hdr = new SegmentHeader();
					hdr.SegNum = readNum(4);
					hdr.setSegmentHeaderFlags(rdr.readByte());
					handleSegmentReferredToCountAndRententionFlags(hdr);
					handleReferedToSegmentNumbers(hdr);
					handlePageAssociation(hdr);
					if (hdr.getSegmentType() != Segment.END_OF_FILE)
						handleSegmentDataLength(hdr);
					Segment seg = null;     // read the Segment data
					int tp = hdr.getSegmentType();
					switch (tp) {
						case Segment.PAGE_INFO: seg = new PageInfoSeg(this); break;
						case Segment.IMM_GEN_REG: seg = new GenRegSeg(this, true); break;
						case Segment.IMM_LL_GEN_REG: seg = new GenRegSeg(this, true); break;
						case Segment.INTRM_GEN_REG: seg = new GenRegSeg(this, false); break;
						case Segment.END_OF_PAGE: continue;
						case Segment.END_OF_FILE: finished = true; continue;
						case Segment.PROFILES: break;
						case Segment.TABLES: break;
						default: throw new Exception("Unknown Segment type in JBIG2 stream " + tp);
					}
					if (seg != null) {
						seg.SegHdr = hdr;
						if (!randAxsOrg)
							seg.readSegment();
						segments.Add(seg);
					}
				}
				if (randAxsOrg)
					foreach (Segment segment in segments)
						segment.readSegment();
			}
			private void handleSegmentDataLength(SegmentHeader segmentHeader) {
				segmentHeader.DataLen = readNum(4);
			}
			private void handlePageAssociation(SegmentHeader segHdr) {
				bool sz = segHdr.isPageAssociationSizeSet();
				segHdr.PgAssoc = readNum(sz ? 4 : 1);
			}
			private void handleReferedToSegmentNumbers(SegmentHeader segHdr) {
				int cnt = segHdr.RefSegCnt;
				int[] seg = new int[cnt];
				int no = segHdr.SegNum, b = (no <= 256 ? 1 : (no <= 65536 ? 2 : 4));
				for (int i = 0; i < cnt; i++)
					seg[i] = rdr.readNum(b);
				segHdr.RefSegs = seg;
			}
			private void handleSegmentReferredToCountAndRententionFlags(SegmentHeader hdr) {
				byte refFlg = rdr.readByte();
				byte firstByte = (byte)(refFlg & 31);               // 31 = 00011111
				int refCnt = (refFlg & 224) >> 5;                   // 224 = 11100000
				byte[] retFlags = null;
				/** take off the first three bits of the first byte */
				if (refCnt <= 4)                                    // short form
					retFlags = new byte[1] { firstByte };
				else if (refCnt == 7) {                             // long form
					rdr.move(-1);
					refCnt = rdr.readNum(4);
					/** calculate the number of bytes in this field */
					int flgBytes = (int)Math.Ceiling(4 + (refCnt + 1) / 8d) - 4;
					retFlags = new byte[flgBytes];
					rdr.readByte(retFlags);
				}
				else  // error
					throw new Exception("Error, 3 bit Segment count field = " + refCnt);
				hdr.RefSegCnt = refCnt;
				hdr.RetFlgs = retFlags;
			}
			internal int readNum(int b) {
				return rdr.readNum(b);
			}
			internal MMRDecoder getMMRDecoder() {
				return decMMR;
			}
			internal ArithmeticDecoder getArithmeticDecoder() {
				return decArithm;
			}
			internal byte readByte() {
				return rdr.readByte();
			}
			internal PageInfoSeg findPageSegement(int page) {
				foreach (Segment segment in segments) {
					SegmentHeader hdr = segment.SegHdr;
					if (hdr.getSegmentType() == Segment.PAGE_INFO && hdr.PgAssoc == page)
						return (PageInfoSeg)segment;
				}
				return null;
			}
			public JBIG2Decoder(byte[] data) {
				rdr = new StreamReader(data);
				resetDecoder();
				bool validFile = checkHeader();
				if (!validFile) {
					// Assume this is a stream from a PDF so there is no file header, end of page segments,
					// or end of file segments. Organisation must be sequential, and the number of pages is assumed to be 1.
					noOfPagesKnown = true;
					randAxsOrg = false;
					noOfPages = 1;
					rdr.move(-8);                           // pointer back to the start of the stream
				}
				else {                                       // We have the file header, so assume 
					setFileHeaderFlags();                    // it is a valid stand-alone file.
					if (noOfPagesKnown)
						noOfPages = rdr.readNum(4);
				}
				decMMR = new MMRDecoder(rdr);
				decArithm = new ArithmeticDecoder(rdr);
				readSegments();                              /** read in the main segment data */
			}
			private void setFileHeaderFlags() {
				byte f = rdr.readByte();
				randAxsOrg = (f & 1) == 0;
				noOfPagesKnown = (f & 2) == 0;
			}
			private bool checkHeader() {
				//throw new NotImplementedException();
				byte[] controlHeader = new byte[] { 151, 74, 66, 50, 13, 10, 26, 10 };
				byte[] actualHeader = new byte[8];
				rdr.readByte(actualHeader);
				return Array.Equals(controlHeader, actualHeader);
			}
			private void resetDecoder() {
				noOfPagesKnown = randAxsOrg = false;
				noOfPages = -1;
				segments.Clear();
				bitmaps.Clear();
			}
			internal void appendBitmap(JBIG2Bitmap bitmap) {
				bitmaps.Add(bitmap);
			}
			public Bitmap GetImage(int pg = 1) {
				JBIG2Bitmap pageBitmap = this.findPageSegement(pg).Bitmap;
				byte[] bytes = pageBitmap.getData(true);
				if (bytes == null) return null;
				int width = pageBitmap.Width(), height = pageBitmap.Height();
				Bitmap bmp = new Bitmap(width, height, PixelFormat.Format1bppIndexed);
				var bd = bmp.LockBits(new Rectangle(0, 0, width, height), ImageLockMode.ReadWrite, PixelFormat.Format1bppIndexed);
				byte[] data = new byte[bd.Stride * height];
				for (int w = width / 8, rem = width % 8, l = 0, y = 0; y < height; y++) {
					int x = y * bd.Stride;
					for (; x < y * bd.Stride + w; x++)
						data[x] = bytes[l++];
					if (rem == 0) continue;
					data[x] = (byte)(bytes[l++] << (8 - rem));
				}
				Marshal.Copy(data, 0, bd.Scan0, data.Length);
				bmp.UnlockBits(bd);
				return bmp;
			}
		}
	}

	public class PdfUtils {
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
		static Image Dict2Img(PdfDictionary dict, bool fAlpha = true) {
			PdfDictionary dSMask = PdfReader.GetPdfObject(dict.Get(PdfName.SMASK)) as PdfDictionary,
						   dMask = PdfReader.GetPdfObject(dict.Get(PdfName.MASK)) as PdfDictionary;
			int width = dict.GetAsNumber(PdfName.WIDTH).IntValue,
				height = dict.GetAsNumber(PdfName.HEIGHT).IntValue;
			Image mask = null, wi = null;
			PRStream prs = (PRStream)dict;
			var decPrm = GetDict0(dict, PdfName.DECODEPARMS);
			PdfObject fltr = dict.Get(PdfName.FILTER);
			PdfArray fltrArr = fltr == null ? new PdfArray()
				: ((fltr as PdfArray) ?? new PdfArray { fltr });
			int bpc = dict.GetAsNumber(PdfName.BITSPERCOMPONENT).IntValue;
			var clrSpc = dict.GetAsArray(PdfName.COLORSPACE);
			if (fAlpha && (dMask ?? dSMask) != null)
				try { mask = Dict2Img(dSMask ?? dMask, false); }
				catch (Exception) { throw; };
			if (fltrArr.Any(x => x.Equals(PdfName.JBIG2DECODE))) {
				var data = PdfReader.GetStreamBytesRaw(prs);
				wi = new JBig2Dec.JBIG2Decoder(data).GetImage();
			}
			if (wi == null) {
				try {
					wi = new PdfImageObject(prs).GetDrawingImage();
				}
				catch (Exception ex) {
					if (fltrArr.Any(x => x.Equals(PdfName.JPXDECODE)))
						throw new MessageException("JP2K image");
					//Console.WriteLine("trying to load " + DumpPdfObj(dict));
					var ct = (clrSpc?[1] as PdfName) ?? (clrSpc?[1] as PdfArray)?[0];
					if (clrSpc == null || ct == null || clrSpc.Count() < 4
					|| !(bpc == 1 || bpc == 4 || bpc == 8) || !PdfName.INDEXED.Equals(clrSpc[0])
					|| !(PdfName.CALRGB.Equals(ct) || PdfName.DEVICERGB.Equals(ct) || PdfName.DEVICECMYK.Equals(ct)))
						throw new Exception("Failed to load Image " + DumpPdfObj(dict), ex);
					PdfObject plt = clrSpc.GetDirectObject(3);
					byte[] palette = (plt as PdfString)?.GetBytes()
							?? ((plt is PRStream p2) ? PdfReader.GetStreamBytes(p2) : null);
					MemoryStream ms = new MemoryStream();
					var png = new iTextSharp.text.pdf.codec.PngWriter(ms);
					png.WriteHeader(width, height, bpc, 3);
					if (palette != null) {
						if (PdfName.DEVICECMYK.Equals(ct)) {        // convert CMYK palette to RGB
							byte[] np = new byte[palette.Length * 3 / 4];
							for (int i = 0, j = 0; i < palette.Length; i += 4, j += 3) {
								float c = 1.0f / 255 * palette[i + 0], m = 1.0f / 255 * palette[i + 1],
									  y = 1.0f / 255 * palette[i + 2], k = 255 - palette[i + 3];
								np[j + 0] = (byte)((1 - c) * k);    // might need to swap 
								np[j + 1] = (byte)((1 - m) * k);    // R <--> B
								np[j + 2] = (byte)((1 - y) * k);
							}
							palette = np;
						}
						png.WritePalette(palette);
					}
					png.WriteData(PdfReader.GetStreamBytes(prs), (width * bpc + 7) / 8);
					png.WriteEnd();
					wi = Image.FromStream(ms);
				}
			}
			int Format32bppCmyk = 8207;                 // handle CMYK images
			if ((int)wi.PixelFormat == Format32bppCmyk && ImageFormat.Jpeg.Equals(wi.RawFormat)) {
				System.Windows.Int32Rect rect = new System.Windows.Int32Rect(0, 0, width, height);
				byte[] pd = new byte[height * width * 4];
				var decoder = new JpegBitmapDecoder(
					new MemoryStream(PdfReader.GetStreamBytesRaw(prs)),
					BitmapCreateOptions.PreservePixelFormat, BitmapCacheOption.Default);
				decoder.Frames[0].CopyPixels(rect, pd, wi.Width * 4, 0);
				for (int i = 0; i < pd.Length; i += 4)
					Utils.Swap(ref pd[i], ref pd[i + 2]);
				Bitmap bmp = new Bitmap(width, height);
				var data = bmp.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
				Marshal.Copy(pd, 0, data.Scan0, pd.Length);
				bmp.UnlockBits(data);
				wi = ScaleImage(bmp, width, height, Color.White);       // 32bpp -> 24bpp
			}
			/* not sure it's width%8 or /K != -1
			 * For following no need to redo image???
			 * Width=2550; BitsPerComponent=1; ColorSpace=/DeviceGray; Decode=[0, 1]; Filter=[/CCITTFaxDecode]; 
			 *		/DecodeParms[] : K=-1
			 * Need conv for:
			 * Filter=/CCITTFaxDecode; BitsPerComponent=1; ColorSpace=/DeviceGray; Width=1668
			 *		/DecodeParms : K=0; BlackIs1=true
			 */
			if (wi.PixelFormat == PixelFormat.Format1bppIndexed && 0 != (wi.Width % 8)
			&& fltrArr.Any(x => x.Equals(PdfName.CCITTFAXDECODE))
			&& decPrm != null && decPrm.GetAsNumber(PdfName.K).GetInt() == 0) {
				var prmK = decPrm.GetAsNumber(PdfName.K).GetInt();
				var prmBlk1 = (decPrm.GetAsBoolean(PdfName.BLACKIS1) ?? PdfBoolean.PDFFALSE).BooleanValue;
				List<byte> raw = new List<byte>(PdfReader.GetStreamBytesRaw(prs));
				if ((raw.Count % 2) != 0) raw.Add(0);
				MemoryStream ms = new MemoryStream();
				ms.WriteBytes(new byte[] { 0x49, 0x49, 0x2a, 0 },
							BitConverter.GetBytes(raw.Count + 16),
							new byte[] { 0, 0, 0, 0, 0, 0, 0, 0 }, raw, new byte[] { 7, 0 });
				int[] ctrl = new int[] {
						0x40100,1,wi.Width, 0x40101,1,wi.Height, 0x40117,1,raw.Count,
						0x40103,1,prmK == -1 ? 4 : 3,		// COMPRESSION
						0x40106,1,prmBlk1 ? 1 : 0,			// PHOTOMETRIC
						0x40102,1,1, 0x40111,1,16			// BITS/STRIPOFFSETS
					};
				ctrl.ForEach(x => ms.WriteBytes(BitConverter.GetBytes(x)));
				wi = Bitmap.FromStream(ms);
			}
			if (fAlpha && mask != null && wi != mask) {
				if (clrSpc != null && clrSpc.Count() == 4 && PdfName.INDEXED.Equals(clrSpc[0])
				&& PdfReader.GetPdfObject(clrSpc[1]) is PdfArray cc1    // ??????????? 
				&& cc1.Count() > 1 && cc1[0].Equals(PdfName.ICCBASED)
				&& GetDict0(dSMask, PdfName.DECODEPARMS)?.Get(PdfName.COLORS).GetInt() == 1) {
					mask = mask.PixFrmt(PixelFormat.Format1bppIndexed).Invert();
					wi = wi.Invert();
				}
				wi = ApplyMask(wi, mask, dSMask == null);
			}
			if (fAlpha && mask == null && dict.Get(PdfName.IMAGEMASK).GetString() == "true")
				wi = ApplyMask(wi, wi, true);
			return wi;
		}
		static Image ScaleImage(Image s, int w, int h, Color c) {
			var rsi = new Bitmap(w, h, PixelFormat.Format24bppRgb);
			using (Graphics g = Graphics.FromImage(rsi)) {
				g.Clear(c);
				g.DrawImage(s, 0, 0, w, h);
			}
			return rsi;
		}
		static Image ApplyMask(Image wi, Image mask, bool inv) {
			Bitmap bmp;
			int width = wi.Width, height = wi.Height;
			(width, height) = (Math.Max(width, mask.Width), Math.Max(height, mask.Height));
			if (wi.Width < width || wi.Height < height)     // resize mask to be same size
				wi = ScaleImage(wi, width, height, Color.Black);
			if (mask.Width < width || mask.Height < height) // resize mask to be same size
				mask = ScaleImage(mask, width, height, Color.Black);
			//Console.WriteLine(dMask == null ? "smask" : "mask");
			//Console.WriteLine(DumpPdfObj(dict));
			// Still work todo....
			//if (dSMask != null) mask = mask.Invert();
			bmp = new Bitmap(wi).Clone(new Rectangle(0, 0, width, height), PixelFormat.Format32bppArgb);
			var rb = bmp.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
			var ra = new byte[rb.Height * rb.Stride]; Marshal.Copy(rb.Scan0, ra, 0, ra.Length);
			Bitmap fm = new Bitmap(mask);
			var mb = fm.LockBits(new Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
			var ma = new byte[mb.Height * mb.Stride]; Marshal.Copy(mb.Scan0, ma, 0, ma.Length);
			bool trans = false;
			for (int y = 0; y < height - 1; y++)
				for (int x = 0, sa = y * rb.Stride, sm = y * mb.Stride; x < width; x++, sa += 4, sm += 3) {
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
			wi = bmp;
			if (!trans)
				wi = ScaleImage(wi, width, height, Color.White);
			return wi;
		}

		class PgRenderListener : IExtRenderListener { //  IRenderListener 
			internal List<String> errs = null;
			internal Graphics g;
			internal Bitmap bmp;
			//internal PdfDictionary	clrs;
			internal iTextSharp.text.Rectangle sz;
			internal bool fTxt;
			internal int imgCnt = 0;
			internal PdfDictionary res;
			Dictionary<string, Tuple<string, FontStyle>> fntMap = new Dictionary<string, Tuple<string, FontStyle>>();
			iTextSharp.text.pdf.parser.Path path = new iTextSharp.text.pdf.parser.Path();
			public void BeginTextBlock() { }
			public void EndTextBlock() { }
			Tuple<string, FontStyle> MapFont(string nmFnt) {
				if (fntMap.TryGetValue(nmFnt, out Tuple<string, FontStyle> ret))
					return ret;
				var fonts = res.GetAsDict(PdfName.FONT);
				var dct = fonts?.Keys.Select(x => fonts.GetAsDict(x))
					.FirstOrDefault(x => x.GetAsName(PdfName.BASEFONT).GetString() == "/" + nmFnt);
				dct = GetDict0(dct, PdfName.DESCENDANTFONTS) ?? dct;
				PdfDictionary dsc = dct?.GetAsDict(PdfName.FONTDESCRIPTOR);
				int flg = (dsc == null) ? 0 : dsc.GetAsNumber(PdfName.FLAGS).GetInt();
				FontStyle sf = FontStyle.Regular;
				if ((flg & 0x00040) != 0 || nmFnt.Contains("Italic")) sf |= FontStyle.Italic;
				if ((flg & 0x40000) != 0 || nmFnt.Contains("Bold")) sf |= FontStyle.Bold;
				string nm = "Arial";
				if ((flg & 1) != 0 || nmFnt.Contains("Courier")) nm = "Courier New";
				if ((flg & 2) != 0 || nmFnt.Contains("Times")) nm = "Times New Roman";
				if (nmFnt.Contains("Trebuchet")) nm = "Trebuchet MS";
				if (nmFnt.Contains("Wingdings")) nm = "Wingdings";
				if (nmFnt.Contains("Calibri")) nm = "Calibri";
				if (nmFnt.Contains("Tahoma")) nm = "Tahoma";
				if (nmFnt.Contains("Impact")) nm = "Impact";
				ret = new Tuple<string, FontStyle>(nm, sf);
				return fntMap[nmFnt] = ret;
			}
			public void RenderText(TextRenderInfo ri) {
				string txt = ri.GetText();
				if (!fTxt || ri == null || txt.Trim().Length == 0)
					return;
				try {
					var bLine = ri.GetBaseline();
					var vec = bLine.GetEndPoint().Subtract(bLine.GetStartPoint());
					if (vec.Length == 0) vec = new Vector(1f, 0f, 0f);
					int a = (int)(Math.Atan2(vec[1], vec[0]) * 180 / Math.PI);
					bool hor = ((a % 180) == 0);
					RectangleJ bl = ri.GetBaseline().GetBoundingRectange(),
							   al = ri.GetAscentLine().GetBoundingRectange();
					var bf = ri.GetFont();
					float size = Math.Abs(hor ? al.Y - bl.Y : al.X - bl.X),
						tw = bf.GetWidth(txt) * 0.001f, span = (hor ? bl.Width : bl.Height);
					if (tw * size > span) size = span / tw; // find right font size
					if (size < 1) return;
					var fnm = MapFont(bf.PostscriptFontName);
					var fnt = new Font(fnm.Item1, size, fnm.Item2);
					var gs = Utils.GetPrivVal<GraphicsState>(ri, "gs");
					var clr = new SolidBrush(Color.FromArgb(gs.StrokeColor?.RGB
										?? gs.FillColor?.RGB ?? Color.Black.ToArgb()));
					var sm = g.Transform;
					ri.GetCharacterRenderInfos().ForEach(x => {
						var xbl = x.GetBaseline().GetBoundingRectange();
						float sx = xbl.X, sy = (xbl.Y > 0 ? sz.Height : 0) - xbl.Y;
						sy = hor ? (sy - size - 2) : (sy + size);
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
			public void RenderImage(ImageRenderInfo ri) {
				try {
					var dict = PdfReader.GetPdfObject(ri.GetRef()) as PdfDictionary
						?? Utils.GetPrivVal<InlineImageInfo>(ri, "inlineImageInfo")?.ImageDictionary;
					var wi = Dict2Img(dict);
					if (wi == null) return;
					var ctm = ri.GetImageCTM();
					//i0 and i3: Scaling in the x and y directions
					//i1 and i2: Rotation and skewing
					//i4 and i5: Translation(movement) in x and y directions                   
					Vector tl = new Vector(0, 0, 1).Cross(ctm), br = new Vector(1, 1, 1).Cross(ctm);
					float x = Math.Min(tl[0], br[0]), w = Math.Max(tl[0], br[0]) - x,
							y = sz.Height - Math.Max(tl[1], br[1]),
							h = sz.Height - Math.Min(tl[1], br[1]) - y;
					if (tl[0] < br[0] && tl[1] < br[1]) {; }
					else if (tl[0] < br[0] && tl[1] > br[1])
						wi.RotateFlip(RotateFlipType.Rotate270FlipXY);
					else if (tl[0] > br[0] && tl[1] < br[1])
						wi.RotateFlip(RotateFlipType.Rotate90FlipXY);
					else
						wi.RotateFlip(RotateFlipType.Rotate180FlipNone);
					g.DrawImage(wi, x, y, w, h);
					imgCnt++;
				}
				catch (Exception ex) {
					if (!(ex is MessageException))
						if (errs != null) errs.Add(ex.Message);
						else Utils.Log(ex);
				}
			}
			public void ModifyPath(PathConstructionRenderInfo ri) {
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
			public iTextSharp.text.pdf.parser.Path RenderPath(PathPaintingRenderInfo ri) {
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
			public void ClipPath(int rule) {
				// TODO : g.Clip = ...
				path.Subpaths.Clear();
			}
		}
		static PdfObject GetProcRes(PdfContentStreamProcessor proc, PdfName sub, PdfName nm) {
			return PdfReader.GetPdfObject(Utils.GetPrivVal<PdfDictionary>(proc, "resources")?
									.GetAsDict(sub)?.Get(nm));
		}
		static String DumpPdfObj(PdfObject obj) {
			if (obj == null) return "<null>";
			if (obj is PdfArray a)
				return "PdfArray(" + a.Size + ") ["
					+ a.GetString(x => DumpPdfObj(PdfReader.GetPdfObject(x)), "; ") + "], ";
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
				bool stroke = char.IsUpper(oper.ToString()[0]);
				var gs = proc.Gs();
				var csn = stroke ? gs.ColorSpaceStroke : gs.ColorSpaceFill;
				var cs = GetProcRes(proc, PdfName.COLORSPACE, csn) as PdfArray ?? new PdfArray();
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
				if (dev == PdfName.DEVICEGRAY) (nop, bpc) = ("g", 1);
				else if (dev == PdfName.DEVICERGB) (nop, bpc) = ("rg", 3);
				else if (dev == PdfName.DEVICECMYK) (nop, bpc) = ("k", 4);
				if (bpc == 0 || plt == null || plt.Length < idx * bpc + bpc) { // oopsie....
					orig.Invoke(proc, oper, prms);
					return;
				}
				var nprm = new List<PdfObject>();               // let default impl 
				for (int j = 0; j < bpc; j++)                   // for right oper handle clr set
					nprm.Add(new PdfNumber(plt[idx * bpc + j]));// populate color info
				if (stroke) nop = nop.ToUpper();                // fill -0 low case, stroke - upr
				var oo = proc.RegisterContentOperator(nop, null);// get original operator
				proc.RegisterContentOperator(nop, oo);          // and restore oper tbl
				oo.Invoke(proc, null, nprm);                    // let orig do the work
			}
		}

		public static Image GetPageImage(PdfReader rdr, int pg, float zoom,
				bool fTxt = false, List<string> err = null) {
			var sz = rdr.GetPageSize(pg);
			Bitmap bmp = new Bitmap((int)(zoom * sz.Width), (int)(zoom * sz.Height), PixelFormat.Format24bppRgb);
			PdfDictionary res = rdr.GetPageN(pg).GetAsDict(PdfName.RESOURCES) ?? new PdfDictionary();
			var lstnr = new PgRenderListener { sz = sz, fTxt = fTxt, errs = err, res = res, bmp = bmp };
			PdfContentStreamProcessor proc = new PdfContentStreamProcessor(lstnr);
			foreach (var o in new string[] { "scn", "SCN" }) {
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
					byte[] bytes = (i as PdfString).GetBytes();
					string str = proc.Gs().Font.Decode(bytes, 0, bytes.Length);
					str = new string(str.ToCharArray()
						.Select(x => specChars.ContainsKey(x) ? specChars[x] : x).ToArray());
					//str.Where(x => x > 255).ForEach(x => Console.WriteLine("{0} {1}", (int)x, x));
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
			class ImgRendListener : IRenderListener {
				internal Dictionary<PdfDictionary, int> szImgs = null;
				internal PdfContentByte Canvas = null;
				public void BeginTextBlock() { }
				public void EndTextBlock() { }
				public void RenderText(TextRenderInfo ri) { }
				public void RenderImage(ImageRenderInfo ri) {
					try {
						//PdfImageObject tries to load image itself - might be very slow...
						//var key = ri.GetImage?().GetDictionary();	
						var key = PdfReader.GetPdfObject(ri.GetRef()) as PdfDictionary;
						// ?? Utils.GetPrivVal<InlineImageInfo>(ri, "inlineImageInfo")?.ImageDictionary;
						if (key == null)
							return;
						var w = szImgs.ContainsKey(key) ? szImgs[key] : 0;
						var ctm = ri.GetImageCTM();             // save max width
						szImgs[key] = Math.Max(w, (int)ctm[0]);
					}
					catch {
					};
				}
			}
			public Stream Shrink(Stream src) {
				MemoryStream so = new MemoryStream(), si = new MemoryStream();
				src.CopyTo(si);
				si.Position = src.Position = 0;
				bool ch = true;
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
						AdjustFonts(stmp);
						//stmp.FormFlattening = stmp.FreeTextFlattening = true;
						for (int i = rdr.XrefSize; i > 0; i--) {
							PdfDictionary obj = rdr.GetPdfObject(i) as PdfDictionary;
							if (obj == null) continue;
							if (obj.IsFont())
								ch = ChkFont(obj, null, i) || ch;
							else if (obj.IsStream() && obj.Get(PdfName.SUBTYPE) != null)
								ch = ChkImage(obj, pgw, i) || ch;
						}
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
			void AdjustFonts(PdfStamper stmp) {
				ImgRendListener lstnr = new ImgRendListener { szImgs = imgRefs };
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
				for (int pg = 1; pg <= rdr.NumberOfPages; pg++) {
					var page = rdr.GetPageN(pg);
					var res = page.GetAsDict(PdfName.RESOURCES);
					string[] strms = getObjList(res, PdfName.XOBJECT)
						.Select(x => x.Get(PdfName.SUBTYPE).GetString()).ToArray();
					bool fFrm = strms.Any(x => x == "/Form"), fImg = strms.Any(x => x == "/Image");
					bool fFnt = getObjList(res, PdfName.FONT)
						.Any(x => x.GetAsName(PdfName.SUBTYPE) != PdfName.TRUETYPE
								|| x.GetAsDict(PdfName.ENCODING) != null);
					var ctx = ContentByteUtils.GetContentBytesForPage(rdr, pg);
					if (fFrm || (fImg && !fFnt))                    // there might be images inside the form
						imgProc.ProcessContent(ctx, res);           // this one doesn't change the page
					if (fFnt) {                                     // if non trueType fonts need to re-incode strings
						var sz = rdr.GetPageSizeWithRotation(page);
						var rot = rdr.GetPageRotation(pg);
						lstnr.Canvas = stmp.GetUnderContent(pg);    // save page contents and create a canvas
						page.Remove(PdfName.CONTENTS);              // clear page	
																	// stamper wraps everything in Rotate upon close. Un-rotate.
						lstnr.Canvas.InternalBuffer.Append(
							string.Format(sRot[rot / 90], sz.Width, sz.Height));
						txtProc.ProcessContent(ctx, res);           // txtProc.CtxOperWrap - rebuilds the page
					}
				}
			}
			//https://github.com/QuestPDF/QuestPDF/issues/31
			PdfName MapFontName(PdfDictionary dct, PdfName fnt) {		    // map font name to one of the internals
				string nmFnt = fnt.ToString();
				if (iTextSharp.text.FontFactory.IsRegistered(nmFnt))
					return null;
				var parts = nmFnt.Split(new char[] { '+' });                // we only remove TTF fonts
				if (dct.GetAsName(PdfName.SUBTYPE) == PdfName.TRUETYPE
				&& dct.GetAsName(PdfName.ENCODING) != null
				&& parts.Length > 1 && parts[0].Length > 5 && parts[1].Length > 4)
					return new PdfName(parts[1]);
				PdfDictionary dsc = dct.GetAsDict(PdfName.FONTDESCRIPTOR);
				int flg = (dsc == null) ? 0 : dsc.GetAsNumber(PdfName.FLAGS).GetInt();
				bool fs = (flg & 0x00002) != 0 || nmFnt.Contains("Times"),  // sarif
					ff = (flg & 0x00001) != 0 || nmFnt.Contains("Courier"), // fixed
					fi = (flg & 0x00040) != 0 || nmFnt.Contains("Italic"),  // italic
					fb = (flg & 0x40000) != 0 || nmFnt.Contains("Bold");    // bold
				if (fi && !ff && fs)                                        // sarif & italic & not fixed
					return new PdfName(fb ? BaseFont.TIMES_BOLDITALIC : BaseFont.TIMES_ITALIC);
				if (fb)                                                     // bold
					return new PdfName(ff ? BaseFont.COURIER_BOLD : (fs ? BaseFont.TIMES_BOLD : BaseFont.HELVETICA_BOLD));
				return new PdfName(ff ? BaseFont.COURIER : (fs ? BaseFont.TIMES_ROMAN : BaseFont.HELVETICA));
			}
			bool ChkFont(PdfDictionary dct, PdfDictionary par, int refIdx) {
				if (dct == null || !dct.IsFont()) return false;
				PdfDictionary dsc = dct.GetAsDict(PdfName.FONTDESCRIPTOR);
				PdfDictionary ch0 = GetDict0(dct, PdfName.DESCENDANTFONTS);
				if (ch0 == null && (dsc == null || dsc.Get(PdfName.FONTFILE2) == null))
					return false;
				string sType = dct.GetAsName(PdfName.SUBTYPE).GetString();
				PdfName font = dct.GetAsName(PdfName.BASEFONT),
						mFnt = MapFontName(ch0 ?? dct, font);
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
					dct.Remove(PdfName.WIDTHS);
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
			bool ChkImage(PdfDictionary dict, int pgWidth, int refIdx) {
				// if img is not in the imgRefs - it's somebody's MASK/SMASK or inline img
				if (dict == null || !imgRefs.ContainsKey(dict)
				|| !dict.Get(PdfName.SUBTYPE).Equals(PdfName.IMAGE))
					return false;
				PdfObject fltr = dict.Get(PdfName.FILTER);
				if (fltr == null) return false;
				PdfArray fa = (fltr as PdfArray) ?? new PdfArray(fltr);
				// need to handle PdfName.JPXDECODE -> JPEG 2000 files!!!!
				if (!fa.OfType<PdfObject>().Any(x => x.Equals(PdfName.DCTDECODE)
				|| x.Equals(PdfName.CCITTFAXDECODE) || x.Equals(PdfName.FLATEDECODE)))
					return false;
				var msk = PdfReader.GetPdfObject(dict.Get(PdfName.SMASK)) as PRStream
					   ?? PdfReader.GetPdfObject(dict.Get(PdfName.MASK)) as PRStream;
				bool isMask = dict.Get(PdfName.IMAGEMASK).GetString() == "true";
				//if (isMask)		return false;
				//if (msk != null)	return false;           
				pgWidth = Math.Min(pgWidth, 792);   // 11" at 72pdi = 792
				int mxw = (imgRefs.ContainsKey(dict)) ? imgRefs[dict] : pgWidth;
				if (mxw == 0) mxw = pgWidth;
				int oldLen = dict.Get(PdfName.LENGTH).GetInt();
				decimal oiw = dict.Get(PdfName.WIDTH).GetNum(), oih = dict.Get(PdfName.HEIGHT).GetNum();
				decimal scale = oiw / mxw,                  // ratio width of img over viewport
						rZip = oldLen * 100m / oiw / oih;   // ratio of arr width over num of pxls
				if (scale * rZip < 0.65m || oldLen < 1000)
					return false;
				try {
					Image wi = Dict2Img(dict, !isMask);
					byte[] newBytes = null;
					if (dict.Get(PdfName.BITSPERCOMPONENT).GetInt() != 1) {
						wi = wi.ScaleDown(mxw * 2, 0);
						if (Image.IsAlphaPixelFormat(wi.PixelFormat))
							newBytes = wi.GetBytes(ImageFormat.Png);
						else if (Math.Min(wi.Width, mxw) >= pgWidth)
							newBytes = wi.BgClean().GetBytes(ImageFormat.Png);
						else
							newBytes = wi.PixFrmt(PixelFormat.Format24bppRgb).GetBytes(ImageFormat.Jpeg);
					}
					else {                                  // scaledown converts 1bpp to full color. 
						if (wi.Width >= mxw * 2)            // Converting back to 1bpp looses resolution. 
							wi = wi.ScaleDown(mxw * 2, 0).PixFrmt(PixelFormat.Format4bppIndexed);
						newBytes = wi.GetBytes(ImageFormat.Png);    // Keep it as 4bpp.
					}
					if (newBytes == null || newBytes.Length > oldLen * 0.8)
						return false;
					var imgZip = iTextSharp.text.Image.GetInstance(newBytes);
					if (isMask) imgZip.MakeMask();
					PdfImage nImg = new PdfImage(imgZip, null, null);
					if (nImg.GetBytes() == null)            // why ????
						return false;
					dict.Keys.ToArray().ForEach(x => dict.Remove(x));
					nImg.Keys.ForEach(x => dict.Put(x, nImg.Get(x)));
					(dict as PRStream).SetDataRaw(nImg.GetBytes());
					return true;
				}
				catch (Exception ex) {
					Utils.Log(ex);
					return false;
				}
			}
		}
	}
	internal class Program {
		static void Main(string[] args) {
			using (Stream fin = new FileStream(args[0], FileMode.Open, FileAccess.Read, FileShare.Read))
				File.WriteAllBytes(args[1],
					PdfUtils.Compress(new MemoryStream(fin.GetBytes())).GetBytes());
		}
	}
}
