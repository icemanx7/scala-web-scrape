import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import org.w3c.dom.DocumentType

import scala.annotation.tailrec
import scala.util.Try

object GetLinks {
  val browser: Browser = JsoupBrowser()

  def main(args: Array[String]): Unit = {
    println(genres("http://ebooks.bharathuniv.ac.in/gdlc1/gdlc1/"))
  }

  def genres(url: String) = {
    val initalList = getListofLinks(url)
    @tailrec
    def reduceLinks(links: List[String]): List[String] = {
      reduceLinks(makeLinks(links, List()))
    }
    reduceLinks(initalList)
  }

  @tailrec
  def makeLinks(links: List[String], acc: List[String]): List[String] = links match {
    case Nil => acc
    case x :: rest => {
      val ll = getListofLinks(x)
      makeLinks(rest, ll ++ acc)
    }
  }

  def getListofLinks(url: String): List[String]= {
    getPageHtml(url).map(x => x >> elementList("a"))
      .getOrElse(List())
      .map(getHref)
      .map(x => appendPath(x, url))
  }


  def appendPath(path: String, base: String) = {
    s"${base}${path}"
  }

  def getHref(elem: Element) = {
    Try(elem.attr("href")).getOrElse("")
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
  def getPageHtml(page: String) =  {
    Try(browser.get(page))
  }
}
