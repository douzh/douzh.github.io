在Java中，String.getBytes(String decode)方法会根据指定的decode编码返回某字符串在该编码下的byte数组表示，如

	byte[] b_gbk = "中".getBytes("GBK");
	byte[] b_utf8 = "中".getBytes("UTF-8");
	byte[] b_iso88591 = "中".getBytes("ISO8859-1");

将分别返回“中”这个汉字在GBK、UTF-8和ISO8859-1编码下的byte数组表示，此时b_gbk的长度为2，b_utf8的长度为3，b_iso88591的长度为1。

而与getBytes相对的，可以通过new String(byte[], decode)的方式来还原这个“中”字时，这个new String(byte[], decode)实际是使用decode指定的编码来将byte[]解析成字符串。

	String s_gbk = new String(b_gbk,"GBK");
	String s_utf8 = new String(b_utf8,"UTF-8");
	String s_iso88591 = new String(b_iso88591,"ISO8859-1");

通过打印s_gbk、s_utf8和s_iso88591，会发现，s_gbk和s_utf8都是“中”，而只有s_iso88591是一个不认识 的字符，为什么使用ISO8859-1编码再组合之后，无法还原“中”字呢，其实原因很简单，因为ISO8859-1编码的编码表中，根本就没有包含汉字 字符，当然也就无法通过"中".getBytes("ISO8859-1");来得到正确的“中”字在ISO8859-1中的编码值了，所以再通过new String()来还原就无从谈起了。

因此，通过String.getBytes(String decode)方法来得到byte[]时，一定要确定decode的编码表中确实存在String表示的码值，这样得到的byte[]数组才能正确被还原。

有时候，为了让中文字符适应某些特殊要求（如http header头要求其内容必须为iso8859-1编码），可能会通过将中文字符按照字节方式来编码的情况，如

	String s_iso88591 = new String("中".getBytes("UTF-8"),"ISO8859-1")

这样得到的s_iso8859-1字符串实际是三个在 ISO8859-1中的字符，在将这些字符传递到目的地后，目的地程序再通过相反的方式

	String s_utf8 = new String(s_iso88591.getBytes("ISO8859-1"),"UTF-8")

来得到正确的中文汉字“中”。这样就既保证了遵守协 议规定、也支持中文。