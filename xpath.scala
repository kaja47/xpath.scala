object Xpath {
  import javax.xml.xpath.XPathFactory
  import javax.xml.xpath.XPathConstants._
  import javax.xml.namespace.QName
  import org.w3c.dom.{ Node, NodeList }

  trait CanXpath[T] {
    /** argument for method xpath.evaluate */
    def xpathConst: QName
    /** cast result of xpath.evaluate (which has proper dynamic type, but static type is AnyRef) to appropriate type */
    def cast(x: AnyRef): T = x.asInstanceOf[T]
  }

  trait ConvertNode[T] {
    /** convert Node into desired type */
    def convert(n: Node): T
  }

  def ConvertNode[T](f: Node => T) = new ConvertNode[T] { def convert(n: Node) = f(n) }

  trait CanXpathAndConvert[T] extends CanXpath[T] with ConvertNode[T] { self =>
    def andThen[U](f: T => U): CanXpathAndConvert[U] =
      new CanXpathAndConvert[U] {
        def xpathConst = self.xpathConst
        override def cast(a: AnyRef)  = f(self cast a)
        def convert(n: Node) = f(self convert n)
      }
  }

  implicit object CanXpathBoolean extends CanXpathAndConvert[Boolean] {
    def xpathConst = BOOLEAN
    def convert(n: Node) = n.getTextContent.toBoolean
  }
  implicit object CanXpathString extends CanXpathAndConvert[String] {
    def xpathConst = STRING
    def convert(n: Node) = n.getTextContent
  }
  implicit object CanXpathDouble extends CanXpathAndConvert[Double] {
    def xpathConst = NUMBER
    def convert(n: Node) = n.getTextContent.toDouble
  }
  implicit val CanXpathInt:    CanXpathAndConvert[Int]    = CanXpathDouble andThen (_.toInt)
  implicit val CanXpathLong:   CanXpathAndConvert[Long]   = CanXpathString andThen (_.toLong)
  implicit val CanXpathBigInt: CanXpathAndConvert[BigInt] = CanXpathString andThen BigInt.apply

  implicit object CanXpathNode extends CanXpathAndConvert[Node] {
    def xpathConst = NODE
    def convert(n: Node) = n
  }
  implicit object CanXpathNodeSet extends CanXpath[NodeList] {
    def xpathConst = NODESET
  }

  implicit def canXpathSeq[T](implicit t: ConvertNode[T]): CanXpath[Seq[T]] =
    new CanXpath[Seq[T]] {
      def xpathConst = NODESET
      override def cast(x: AnyRef) = x.asInstanceOf[NodeList] map t.convert
    }

  implicit def enhanceNodeList1(nl: NodeList): Seq[Node] = (0 until nl.getLength map (nl item _))
  implicit def enhanceNodeList2(nl: NodeList) = new {
    def getTextContent = enhanceNodeList1(nl) map (_.getTextContent) mkString
  }

  /** When this method is invoked, it compiles xpath query and returns another function that do matching against this xpath.
   *  That way, you can match against it several times, but xpath is compiled only once. */
  def xpath[T](path: String)(implicit ev: CanXpath[T]): AnyRef => T = {
    val expr = XPathFactory.newInstance.newXPath.compile(path)
    (doc) => ev.cast(expr.evaluate(doc, ev.xpathConst))
  }

  def xpath[T](path: String, doc: AnyRef)(implicit ev: CanXpath[T]): T = xpath[T](path)(ev)(doc)
}
