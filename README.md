## Xpath.scala

*Xpath.scala* is a simple tool to make dealing with Xpath in scala little bit
nicer and more typed.

### Example usage

```scala
// 1) somehow create org.w3c.dom.Document instance

val xmlString = """<?xml version="1.0" encoding="utf-8"?>
<channel>
	<title>k47.cz</title>
	<item><title>title 1</title><guid isPermaLink="true">https://k47.cz/article-1.html</guid><pubDate>Sun, 26 Aug 2018 00:00:00 +0200</pubDate></item>
	<item><title>title 2</title><guid isPermaLink="true">https://k47.cz/article-2.html</guid><pubDate>Sun, 12 Jan 2018 01:00:00 +0200</pubDate></item>
</channel>"""

val builder = javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder
val doc = builder.parse(new java.io.ByteArrayInputStream(xmlString.getBytes("utf-8")))


case class Item(title: String, url: String)


import Xpath.xpath
import org.w3c.dom._

// 2) prepare nested xpath queries

val readTitle = xpath[String]("title")
val readUrl   = xpath[String]("guid")

val f = xpath[Seq[Node]]("//item[guid[@isPermaLink]]").andThen { nodes =>
	nodes.map { n =>
		Item(readTitle(n), readUrl(n))
	}
}

// 3) perform query

f(doc) foreach println
```
