package controllers

import play.api.mvc.{Action, Controller}

/**
 * Created with IntelliJ IDEA.
 * User: MIglesias
 * Date: 6/6/13
 * Time: 4:59 PM
 * To change this template use File | Settings | File Templates.
 */
object Retriever extends Controller {

  def load() = Action {
    Redirect("http://www1.macys.com/shop/juniors-clothing/juniors-dresses?id=18109")
  }

}
