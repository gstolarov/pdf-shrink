using iTextSharp.xmp.impl;
using System;
using System.Collections.Generic;
using System.Drawing.Imaging;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

namespace WebLib {
	public class MessageException : Exception {
		public MessageException(string msg) : base(msg) {  }
	}
	public static class Utils {
		public static void Log(Exception ex) {
			Console.WriteLine(ex.ToString());
		}
		public static void Log(string msg) {
			Console.WriteLine(msg);
		}
		public static T GetPrivVal<T>(object obj, string nm) {
			FieldInfo fld = null;
			for (var t = obj.GetType(); t != null && fld == null; t = t.BaseType)
				fld = t.GetField(nm, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
			return (T)fld?.GetValue(obj);
		}
		public static byte[] GetBytes(this Stream s, bool curPos = false) {
			byte[] ret;
			if (s is MemoryStream _tms
			&& (ret = Utils.GetPrivVal<byte[]>(_tms, "_buffer")).Length == _tms.Length)
				return ret;
			MemoryStream ms = new MemoryStream();
			if (!curPos && s.CanSeek && s.Position != 0) {
				long p = s.Position;
				s.Position = 0;
				s.CopyTo(ms);
				s.Position = p;
			}
			else
				s.CopyTo(ms);
			ms.Position = 0;
			return ms.ToArray();
		}
		public static string reReplace(this string s, string ptrn, string rs, bool noCase = true) {
			return Regex.Replace(s, ptrn, rs,
				(noCase ? RegexOptions.IgnoreCase : 0) | RegexOptions.Singleline);
		}
		public static IEnumerable<T> ForEach<T>(this IEnumerable<T> arr, Action<T> exp) {
			foreach (T o in arr)
				exp(o);
			return arr;
		}
		public static string GetString<T>(this IEnumerable<T> arr, Func<T, string> exp, string sep) {
			StringBuilder ret = new StringBuilder();
			foreach (T o in arr) {
				if (ret.Length > 0) ret.Append(sep);
				ret.Append(exp == null ? o.GetString() : exp(o));
			}
			return ret.ToString();
		}
		public static string GetString(this object o) {
			if (o == null || Convert.IsDBNull(o))
				return "";
			if (o.GetType() == typeof(DateTime)) {
				DateTime tm = (DateTime)o;
				if (tm.TimeOfDay.TotalMinutes == 0)
					return tm.ToString("MM/dd/yyyy");
			}
			if (o.GetType() == typeof(decimal)) {
				decimal d = (decimal)o;
				if (d < Int64.MaxValue && (d - (long)d) == 0L)
					return ((long)d).ToString();
			}
			return o.ToString();
		}
		public static Decimal GetNum(this object o) {
			if (o == null)
				return 0;
			if (o is int || o is decimal || o is long || o is float || o is double || o is short)
				return Convert.ToDecimal(o);
			Decimal r = 0;
			int s = 1, d = 1, f = 0, op = 0;
			foreach (char c in GetString(o)) {
				if (c == '-') s = -s;
				else if (c == '.') f = 1;
				else if (c == '(') op = c;
				else if (c == ')')
					if (op != 0) s = -1;
					else break;
				else if (c >= '0' && c <= '9') {
					r = r * 10 + (c - '0');
					if (f == 1) {
						d *= 10;
						if (d >= 1000000000)
							break;
					}
				}
				else if (c != ':' && c != '$' && c != ',' && c != '+')
					break;
			}
			return s * r / d;
		}
		public static int GetInt(this object o) {
			decimal r = o.GetNum();
			return (r > int.MinValue && r < int.MaxValue) ? (int)r : 0;
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
	}
}
