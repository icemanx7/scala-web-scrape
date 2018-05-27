import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import org.w3c.dom.DocumentType

import scala.util.Try

object ParserPage {
  val browser: Browser = JsoupBrowser()

  def main(args: Array[String]): Unit = {
    println(genres("https://www.imdb.com/title/tt0184894/?ref_=fn_al_tt_1"))
  }

  def genres(url: String): List[String] = {
    getPageHtml(url).map(x => x >> elementList("a"))
      .get
      .filter(isGenreRef)
      .filter(doesContainSpan)
      .flatMap(getGenreList)
  }

  def isGenreRef(elem: Element): Boolean = {
    Try(elem.attr("href").contains("genre")).getOrElse(false)
  }

  def getGenreList(elem: Element): List[String] = {
    (elem >> elementList("span"))
      .map(x => x.text)
  }

  def doesContainSpan(elem: Element): Boolean = {
    !(elem >> elementList("span")).isEmpty
  }

  /*
   * TODO: Handle the error
   */
  def getPageHtml(page: String): Try[browser.DocumentType] =  {
    Try(browser.get(page))
  }
}
