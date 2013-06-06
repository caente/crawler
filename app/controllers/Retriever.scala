package controllers

import play.api.mvc.{Controller,Action}
import play.api.libs.json.Json
import scala.io.Source
import play.api.libs.iteratee.{Iteratee, Enumerator}

/**
 * Created with IntelliJ IDEA.
 * User: MIglesias
 * Date: 6/6/13
 * Time: 12:52 PM
 * To change this template use File | Settings | File Templates.
 */
object Retriever extends Controller{
    lazy val url = "http://www1.macys.com/shop/juniors-clothing/juniors-dresses?id=18109"
  val reader = Iteratee.fold[Array[Byte],String](""){(text,chunk)=>
     text+new String(chunk)
  }

  def load= Action {
  //  Enumerator(Source.fromURL(url).getLines())(Iteratee.fold(""){_+_})
    Ok(Enumerator(Source.fromURL(url).)(reader))
  }

}
