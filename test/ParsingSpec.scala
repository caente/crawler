import UglyTester.Site
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: MIglesias
 * Date: 6/3/13
 * Time: 4:05 PM
 * To change this template use File | Settings | File Templates.
 */
class ParsingSpec extends FunSuite{
   test("checking the HTML"){
     UglyTester.fetch(new Site("http://www1.macys.com/shop/womens-clothing/dresses?id=5449&edge=hybrid&cm_sp=us_catsplash_women-_-row5-_-Dresses"))
   }
}
