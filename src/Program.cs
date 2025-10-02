using System.IO;
using WebLib;

namespace PdfShrink {
	internal class Program {
		static void Main(string[] args) {
			using (Stream fin = new FileStream(args[0], FileMode.Open, FileAccess.Read, FileShare.Read))
				File.WriteAllBytes(args[1],
					PdfUtils.Compress(new MemoryStream(fin.GetBytes())).GetBytes());
		}
	}
}
