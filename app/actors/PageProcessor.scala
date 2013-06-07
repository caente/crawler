
import akka.actor.{ActorLogging, Actor}
import java.io.{File, FileOutputStream}
import java.net.URL
import java.util.regex.Pattern
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element}
import org.jsoup.select.{Elements, Evaluator, Collector}
import scala.collection.JavaConverters._


/**
 * Created with IntelliJ IDEA.
 * User: MIglesias
 * Date: 6/3/13
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
case class Item(price: String, description: String, link: String, img: String) {
  override def toString = "price:\t" + price + "\ndesc:\t" + description + "\nlink:\t" + link + "\nimg:\t" + img
}

object PageProcessor {

  /** If there is an prefix is added to the start of the result. Also checks if the text should be retrieved from an attribute or the actual content of the tag
   *
   * @param prefix
   * @param attr
   * @param element
   * @return
   */
  def process(prefix: Option[String])(attr: Option[String])(element: Element): String = {
    val pr = prefix match {
      case Some(p: String) => p
      case None => ""
    }
    pr + (attr match {
      case Some(at: String) => element.attr(at)
      case None => element.text
    })
  }

  /** Makes an css query to find a list of elements. This elements will the ones that will "mark" the items we want to register.
   *
   * @param path
   * @param element
   * @return
   */
  def selectByPath(path: String)(element: Element): Elements = element.select(path)

  /**  Combines the processUP and the processByPath. The processUp finds the nth parent, then the processByPath finds the elements using css query
   *
   * @param prefix
   * @param levelsUp
   * @param path
   * @param index
   * @param attr
   * @return
   */
  def generalProcessor(prefix: String, levelsUp: Int, path: String, index: Int, attr: String): Element => String = processUP(levelsUp)(processByPath(path)(index)(process(Option(prefix))(Option(attr))))(_)

  /** Inspects all the elements that the selector found, and for each one an Item is created, using the processors
   *
   * @param selector
   * @param element
   * @param processPrice
   * @param processDescription
   * @param processLink
   * @param processImg
   * @return
   */
  def inspect(selector: Element => Elements)(element: Element, processPrice: Element => String, processDescription: Element => String, processLink: Element => String, processImg: Element => String): List[Item] = {
    val elements = selector(element)
    elements.asScala.foldLeft(List[Item]()) {
      (items, el) =>
        new Item(processPrice(el), processDescription(el), processLink(el), processImg(el)) :: items
    }
  }

  /** Uses a selector from a css query
   *
   * @param path
   * @return
   */
  def inspectByPath(path: String) = inspect(selectByPath(path))(_, _, _, _, _)


  /** Goes "up" parents
   *
   * @param up
   * @param p
   * @param element
   * @return
   */
  def processUP(up: Int)(p: Element => String)(element: Element): String = if (up == 0) p(element) else processUP(up - 1)(p)(element.parent())

  /** Finds the "pos" element of the list returned by the css query
   *
   * @param path
   * @param pos
   * @param p
   * @param element
   * @return
   */
  def processByPath(path: String)(pos: Int = 0)(p: Element => String)(element: Element): String = p(selectByPath(path)(element).get(pos))//p(element.select(path).get(pos))
}

/** This evaluator can be used in the inspect function, like this:  Collector.collect(new Eval(element.hasClass("prices")), element)
 *
 * @param evaler
 */
class Eval(evaler: Element => Boolean) extends Evaluator {

  def matches(root: Element, element: Element): Boolean = evaler(element)

}

/** Eventually will be the actor who makes this operations posible
 *
 */
class PageProcessor extends Actor with ActorLogging {

  def receive = {
    case _ => println
  }

}


object UglyTester extends App {

  import PageProcessor._


  case class Site(address: String)

  def fetch(site: Site): List[Item] = {

    //    def processDescriptionMacys(element: Element): String = element.parent.getElementsByClass("shortDescription").first().text()
    //
    //    def processPriceMacys(element: Element): String = element.getElementsByAttributeValue("itemprop", "price").first().attr("content")
    //
    //    def processImgMacys(element: Element) = element.parent().select("a").first().select("input").last().attr("value")
    //
    //    def processLinkMacys(element: Element) = "http://macys.com" + element.parent().select("a").first().attr("href")
    //
    //    def checkPrice(element: Element) = element.hasClass("prices")
    //
    //    def selectByClass(element: Element) = Collector.collect(new Eval(checkPrice), element)

    val url = new URL(site.address)
    val doc = Jsoup.parse(url, 3000)
    val body = doc.body()



    def description = generalProcessor(null, 1, "div.shortDescription", 0, null)
    def price = generalProcessor(null, 0, "*[itemprop=price]", 0, "content")
    def link = generalProcessor("http://macys.com", 1, "a.imageLink", 0, "href")
    def img = generalProcessor(null, 1, "input:last-of-type", 0, "value")

    inspectByPath("div.prices")(body, price, description, link, img)
  }


  val site = new Site("http://www1.macys.com/shop/juniors-clothing/juniors-dresses?id=18109")
  val newDoc = fetch(site)
  val file = new File("result.txt")
  if (file.exists()) file.delete
  val output = new FileOutputStream(file)
  output.write(newDoc.mkString("\n" + ">" * 30 + "\n").getBytes())
  output.close()


}
