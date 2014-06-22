package lc.fn.sketchy

import org.scalatra._
import scalate.ScalateSupport
import scala.io.Source
import scala.util.Random
import com.github.tototoshi.base62.Base62
import com.redis._

class Sketchy extends SketchyUrlShortenerStack with ScalateSupport {
  val r = new RedisClient("localhost", 6379)
  val base62 = new Base62
  val files = new java.io.File("words").listFiles
  val rand = new Random
  var words:Map[String,List[String]] = Map()
  var endings = List("mkv", "mp4", "dll", "exe", "so", "out", "kmod", "msi", "msp", "jar", "bat", "cmd", "py", "sh", "vbs", "pdf", "rb")
  for(file <- files) {
    val name = file.getName
    var tmp_words:List[String] = List()
    for(line <- Source.fromFile(file).getLines) {
      if(line.length > 0 && line.head != '#') {
        tmp_words = line.split(",").map(x => x.trim).toList ::: tmp_words
      }
    }
    words += (name -> tmp_words)
  }
  get("/:url") {
    val url = params("url")
    val to = r.get("sketchy_url:"+url)
    to match {
      case Some(s) => redirect(s)
      case None => halt(404, <h1>Url Not Found</h1>)
    }
  }
  get("/") {
    contentType = "text/html"
    ssp("/WEB-INF/views/index.jade", "lists" -> words.keys.toList)
  }
  get("/short") {
    contentType = "text/html"
    ssp("/WEB-INF/views/index.jade", "lists" -> words.keys.toList)
  }
  post("/new") {
    contentType = "text/html"
    val url = params("url")
    val hidden = params("hidden")
    val lists = params("lists")
    println(params.keys.mkString("[", ", ","]"))
    if(hidden.length > 0){
      halt(403, "NOPE")
    }
    var short = ""
    params.get("base62") match {
      case Some(s) => {
        val num = r.incr("sketch_latestid")
        short = base62.encode(num.last)
      }
      case None => {
        short = getUrl(words.keys.toList)
      }
    }
    r.set("sketchy_url:"+short, url)
    ssp("/WEB_INF/views/new.jade", "url" -> url, "short" -> ("http://fn.lc/"+short))
  }
  get("/test") {
    getUrl(words.keys.toList)
  }
  def randWord(lists: List[String]): String = {
    var tmp_words:List[String] = List()
    for(list <- lists){
      tmp_words = words(list) ::: tmp_words
    }
    tmp_words(rand.nextInt(tmp_words.length))
  }
  def getUrl(lists: List[String]): String = {
    var url = randWord(lists)
    for(v <- 1 to rand.nextInt(4) ) {
      url += "_"+randWord(lists)
    }
    val num = r.incr("sketch_latestid")
    url += "_" + base62.encode(num.last)
    for(v <- 1 to rand.nextInt(4) ) {
      url += "_"+randWord(lists)
    }
    url += "."+endings(rand.nextInt(endings.length))
    url = url.replaceAll(" ", "_")//.toLowerCase
    url
  }
}
