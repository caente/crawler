import play.api._
import play.api.mvc._
import play.api.http.HeaderNames._

/**
 * Global application settings.
 */
object Global extends GlobalSettings {

  /**
   * Global action composition.
   */
  override def doFilter(action: EssentialAction): EssentialAction = EssentialAction { request =>
    action.apply(request).map(_.withHeaders(
      ACCESS_CONTROL_ALLOW_ORIGIN -> "*"
    ))
  }
}