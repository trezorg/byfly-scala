package by.trezor.byfly

import java.io.File
import java.util.logging._
import scalaj.http._
import scala.language.reflectiveCalls
import scala.util.{Success, Try, Failure}
import org.rogach.scallop._
import org.fusesource.jansi.AnsiConsole
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.streum.configrity._
import org.fusesource.jansi.Ansi._


object ByflyOutput {

  def out(op: String => Unit)(text: String, color: Option[Color] = None): ByflyOutput.type = {
    color match {
      case Some(cl) =>
        AnsiConsole.systemInstall()
        op(ansi().fg(cl).a(text).reset().toString)
      case _ => op(text)
    }
    this
  }
  def outPrintln = out(println) _
  def outPrint = out(print) _
  def prnln(text: String, color: Option[Color] = None): ByflyOutput.type = outPrintln(text, color)
  def prn(text: String, color: Option[Color] = None): ByflyOutput.type = outPrint(text, color)
  def apply(text: String, color: Option[Color] = None) = prnln(text, color)

}

object ByflyParser {

  private val TariffText = "Тарифный план на услуги"
  private val StatusText = "Статус блокировки"
  private val RegexpBalance = "(.*?)(-?\\d+)(.*)".r
  private val imageSrc = "/data/img/liber/coins.png"

  def getBalance(doc: Document) = {
    val selector = "img[src=%s]" format imageSrc
    val balance = doc.select(selector).first().siblingElements().select("b").text().replaceAll("\\s*", "")
    balance match {
      case RegexpBalance(_, b, _) => b
      case _ => ""
    }
  }

  def getTariff(doc: Document) = {
    val selector = "td:containsOwn(%s)" format TariffText
    doc.select(selector).first().siblingElements().first().text()
  }

  def getStatus(doc: Document) = {
    val selector = "span:containsOwn(%s)" format StatusText
    doc.select(selector).first().parent().siblingElements().first().text()
  }

  def parsePage(html: String) = {
    val doc = Jsoup.parse(html)
    (getBalance(doc), getTariff(doc), getStatus(doc))
  }

}

class ByflyOptions(arguments: Seq[String]) extends ScallopConf(arguments) {

  banner("""Usage: byfly [OPTIONS] [classname]
           |This program allows to know byfly account
           |Options:
           |""".stripMargin)
  version("byfly balance. 0.0.2")
  val login = opt[String](required = false, descr = "set login")
  val password = opt[String](required = false, descr = "set password")
  val filename = opt[String](required = false, descr = "set configuration file")
  mutuallyExclusive(login, filename)
  mutuallyExclusive(password, filename)
  codependent(login, password)
}

object Byfly {
  val LOG                         = Logger.getLogger("byfly")
  private val UrlLogin            = "https://issa.beltelecom.by/main.html"
  private val PageCodePage        = "utf-8"
  private val UserAgentHeaders    = List(
    "User-Agent" -> ("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 " +
      "(KHTML, like Gecko) Chrome/34.0.1847.116 Safari/537.36")
  )
  private val SessionCookieName   = "PHPSESSID"
  private val PasswordFieldName   = "passwd"
  private val LoginFieldName      = "oper_user"
  private val RedirectFieldName   = "redirect"
  private val RedirectFieldValue  = "/main.html"
  val ConfigErrorText             = "Задайте конфигурационный файл или логин и пароль"
  val MainErrorText               = "Введен неверный логин или пароль"
  private val ConnectionTimeout   = 3600 * 1000

  val byflyHttpOptions = List(HttpOptions.connTimeout(ConnectionTimeout),
    HttpOptions.readTimeout(ConnectionTimeout))

  def cookiesToMap(st: String) =
    st.split("; ").map(_.split("=", 2)).collect { case Array(a, b) => a -> b }.toMap

  def parseCookies(cookie: Option[List[String]]) = cookie match {
    case Some(list: List[String]) =>
      (for (
        map <- list.map(cookiesToMap);
        (name, value) <- map
        if name == SessionCookieName
      ) yield value).mkString
    case _ => ""
  }

  def apply(login: String, password: String): Unit = new Byfly(login, password).request()

}

class Byfly(login: String, password: String) {

  import Byfly._

  def getLoginParams = List(
    LoginFieldName -> login,
    PasswordFieldName -> password,
    RedirectFieldName -> RedirectFieldValue)

  lazy val sessionHeaders: Option[List[(String, String)]] = {
    val cookies = getSessionCookie
    if (!cookies.isEmpty)
      Some(List(("Cookie", SessionCookieName + "=" + cookies)) ::: UserAgentHeaders)
    else None
  }

  def getSessionCookie = {
    val post = Http.post(UrlLogin).
      headers(UserAgentHeaders).
      options(byflyHttpOptions).
      option(_.setInstanceFollowRedirects(false)).
      params(getLoginParams)
    parseCookies(post.asCodeHeaders._2.get("Set-Cookie"))
  }

  def getBalancePage = for (headers <- sessionHeaders) yield {
    Http(UrlLogin).headers(headers).
      options(byflyHttpOptions).charset(PageCodePage).asString
  }

  def getData = Try(for (page <- getBalancePage) yield ByflyParser.parsePage(page))

  def request(): Unit = {
    getData match {
      case Success(Some((balance, tariff, status))) =>
        val fields = List("Логин", "Баланс", "Тариф", "Статус")
        val maxFieldLength = fields.view.map(_.length).max
        fields.map {
          x => "%s%s=> ".format (x, " " * (maxFieldLength - x.length + 1))
        }.zip(List(login, balance, tariff, status)).foreach { case (field, value) =>
          ByflyOutput.prn(field, Some(Color.GREEN))(value)
        }
      case Success(res) =>
        LOG.log(Level.SEVERE, "Something wrong. %s" format res)
      case Failure(ex) =>
        LOG.log(Level.SEVERE, "%s: %s" format (MainErrorText, ex.getMessage))
    }
  }
}

object ByflyConfig {

  val defaultConfigFile = "byfly.conf"
  def checkFileExists(file: File): Boolean = file.exists && file.isFile

  def getConfigs(filename: Option[String]): List[String] = {
    val list = List(
      new File(new File(".").getAbsolutePath, defaultConfigFile),
      new File(new File(System.getProperty("user.home")).getAbsolutePath, defaultConfigFile))
    (if (filename.isDefined) new File(filename.get) :: list else list).
      filter(checkFileExists).map(_.getCanonicalPath)
  }

  def readConfigs(filename: Option[String]): (Option[String], Option[String]) = {
    val data = mergeConfigs(getConfigs(filename) map parseConfig)
    data(0) -> data(1)
  }

  def mergeConfigs(list: List[(Option[String], Option[String])]): List[Option[String]] = {
    val (a, b) = list.unzip
    List(a, b).map(_.collect {
      case Some(x) => x }
    ).map(x => if (x.isEmpty) None else Some(x(0)))
  }

  def parseConfig(filename: String): (Option[String], Option[String]) = {
    val config = Configuration.load(filename)
    (config.get[String]("login"), config.get[String]("password"))
  }

}

object Main {

  import Byfly._

  def getCredentials(conf: ByflyOptions, fileName: Option[String]) = {
    if (conf.login.isEmpty || conf.password.isEmpty) {
      // we should read conf file
      ByflyConfig.readConfigs(fileName)
    } else (Some(conf.login()), Some(conf.password()))
  }

  def main(args: Array[String]): Unit =  {
    val conf = new ByflyOptions(args)
    val fileName   = if (conf.filename.isEmpty) None else Some(conf.filename())
    getCredentials(conf, fileName) match {
      case (Some(login), Some(password)) => Byfly(login, password)
      case _ =>
        LOG.info(ConfigErrorText)
        conf.printHelp()
    }
  }
}
