
import akka.actor.{ActorLogging, Actor}
import java.io.{File, FileOutputStream}
import java.net.URL
import java.util.regex.Pattern
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element}
import org.jsoup.select.{Evaluator, Collector}
import scala.collection.JavaConverters._


/**
 * Created with IntelliJ IDEA.
 * User: MIglesias
 * Date: 6/3/13
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
case class Item(price: String, description: String, link: String, img: String)

object PageProcessor {

  def process(element: Element): String = element.parent().html()

  lazy val pricePattern = Pattern.compile( """\$\d{1,5}.\d{1,4}""")

  def selectByPricePattern(element: Element) = pricePattern.matcher(element.text()).matches()

  def inspect(element: Element, selector: Element => Boolean, processPrice: Element => String, processDescription: Element => String, processImg: Element => String, processLink: Element => String): List[Item] = {
    val elements = Collector.collect(new Eval(selector), element)
    elements.asScala.foldLeft(List[Item]()) {
      (items, el) =>
        new Item(processPrice(el), processDescription(el), processImg(el), processLink(el)) :: items
    }
  }


}

class Eval(evaler: Element => Boolean) extends Evaluator {

  def matches(root: Element, element: Element): Boolean = evaler(element)

}

class PageProcessor extends Actor with ActorLogging {

  def receive = {
    case _ => println
  }

}


object UglyTester extends App {


  case class Site(address: String)

  def fetch(site: Site): List[Item] = {

    def processDescriptionMacys(element: Element): String = element.parent.getElementsByClass("shortDescription").first().text()

    def processPriceMacys(element: Element): String = element.getElementsByAttributeValue("itemprop", "price").first().attr("content")

    def processImgMacys(element: Element) = element.parent().select("a").first().select("input").last().attr("value")

    def processLinkMacys(element: Element) = "http://macys.com" + element.parent().select("a").first().attr("href")

    def selectByClass(element: Element) = element.hasClass("prices")

    val url = new URL(site.address)
    val doc = Jsoup.parse(url, 3000)
    val body = doc.body()
    PageProcessor.inspect(body, selectByClass, processPriceMacys, processDescriptionMacys, processImgMacys, processLinkMacys)
  }


  val site = new Site("http://www1.macys.com/shop/juniors-clothing/juniors-dresses?id=18109")
  val newDoc = fetch(site)
  val file = new File("result.xml")
  if (file.exists()) file.delete
  val output = new FileOutputStream(file)
  output.write(newDoc.mkString("\n" + ">" * 30 + "\n").getBytes())
  output.close()


}
