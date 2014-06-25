package lc.fn.sketchy

import org.scalatra._
import scalate.ScalateSupport
import scala.collection.immutable.Map
import scala.io.Source
import scala.util.Random
import com.github.tototoshi.base62.Base62
import com.netaporter.uri.dsl._
import com.netaporter.uri.Uri.parse
import com.redis._

class Sketchy extends SketchyUrlShortenerStack with ScalateSupport {
  val r = new RedisClient("localhost", 6379)
  val base62 = new Base62
  val files = new java.io.File("words").listFiles
  val rand = new Random
  var endings = List("mkv", "mp4", "dll", "exe", "so", "out", "kmod", "msi", "msp", "jar", "bat", "cmd", "py", "sh", "vbs", "pdf", "rb", "jpg.exe", "COM")
  val words: Map[String, List[String]] = {
    def getWordsFromFile(file: java.io.File): List[String] = {
      Source.fromFile(file).getLines
        .filter(line => line.length > 0 && line.head != '#')
        .flatMap(_.split(","))
        .map(_.trim)
        .toList
    }
    files.map(file => (file.getName, getWordsFromFile(file))).toMap
  }

  get("/:url") {
    val url = params("url")
    val to = r.get("sketchy:url:"+url)
    def formatUrl(url: String) = {
      def uri = parse(url)
      uri.protocol match {
        case Some => url
        case None => "http://" + url
      }
    }
    to match {
      case Some(s) => {
        halt(status = 301, headers = Map("Location" -> formatUrl(s)))
      }
      case None => halt(404, <h1>Url Not Found</h1>)
    }
  }
  get("/") {
    contentType = "text/html"
    ssp("/WEB-INF/views/index.jade", "lists" -> words)
  }
  get("/short") {
    contentType = "text/html"
    ssp("/WEB-INF/views/index.jade", "lists" -> words)
  }
  post("/new") {
    contentType = "text/html"
    val url = params("url")
    val hidden = params("hidden")
    val lists = request.getParameterValues("lists").toList
    var word_count = params("words").toInt
    if(word_count > 100){
      word_count = 100
    }
    if(hidden.length > 0){
      halt(403, "NOPE")
    }
    var short = ""
    params.get("base62") match {
      case Some(s) => {
        val num = r.incr("sketchy:latestid")
        short = base62.encode(num.last)
      }
      case None => {
        short = getUrl(lists, word_count)
      }
    }
    r.set("sketchy:url:" + short, url)
    ssp("/WEB_INF/views/new.jade", "url" -> url, "short" -> ("http://fn.lc/"+short))
  }
  get("/test") {
    getUrl(words.keys.toList, 8)
  }
  def randWord(lists: List[String]): String = {
    val tmp_words = lists.flatMap(words(_)).toArray
    tmp_words(rand.nextInt(tmp_words.length))
  }
  def messCase(word: String): String = {
    rand.nextInt(3) match {
      case 0 => word.toLowerCase
      case 1 => word.toLowerCase.split(" ").map(_.capitalize).mkString(" ")
      case 2 => word
    }
  }
  def getUrl(lists: List[String], count: Int): String = {
    def wordList:List[String] = (0 to rand.nextInt(count)).map(i => messCase(randWord(lists)).replaceAll(" ", "_")).toList
    val num = r.incr("sketchy:latestid")
    val parts = insertAt(base62.encode(num.last), rand.nextInt(wordList.length + 1), wordList)
    val url = parts.mkString("_")
    url + "." + endings(rand.nextInt(endings.length))
  }
  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
    val(pre, post) = ls.splitAt(n)
    pre ::: e :: post
  }
}
