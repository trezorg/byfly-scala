package by.trezor.byfly

import java.io.File
import java.util.logging._
import scalaj.http._
import scala.language.reflectiveCalls
import org.rogach.scallop._
import org.fusesource.jansi.AnsiConsole
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.streum.configrity._
import org.fusesource.jansi.Ansi._


object ByflyOutput {

  def out(op: String => Unit)(text: String, color: Option[Color] = None): ByflyOutput.type = {
    color match {
      case Some(cl) => {
        AnsiConsole.systemInstall()
        op(ansi().fg(cl).a(text).reset().toString)
      }
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

  private val BalanceText = "Актуальный баланс"
  private val TariffText = "Тарифный план"
  private val RegexpBalance = "-?\\d+".r

  def getBalance(doc: Document): String = {
    val selector = "td.dark:containsOwn(%s)" format BalanceText
    val balance = doc.select(selector).first().nextElementSibling().text()
    RegexpBalance findFirstIn balance match {
      case Some(bal) => bal
      case _ => ""
    }
  }

  def getTariff(doc: Document): String = {
    val selector = "img[alt=%s]" format TariffText
    doc.select(selector).parents().first().siblingElements().select("b.body").text()
  }

  def parsePage(html: String): (String, String) = {
    val doc = Jsoup.parse(html)
    (getBalance(doc), getTariff(doc))
  }

}

class ByflyOptions(arguments: Seq[String]) extends ScallopConf(arguments) {

  banner("""Usage: byfly [OPTIONS] [classname]
           |This program allows to know byfly account
           |Options:
           |""".stripMargin)
  val login = opt[String](required = false, descr = "set login")
  val password = opt[String](required = false, descr = "set password")
  val filename = opt[String](required = false, descr = "set configuration file")
  mutuallyExclusive(login, filename)
  mutuallyExclusive(password, filename)
  codependent(login, password)
}

object Byfly {

  private val UrlLogin            = "https://issa.beltelecom.by/cgi-bin/cgi.exe?function=is_login"
  private val UrlAccount          = "https://issa.beltelecom.by/cgi-bin/cgi.exe?function=is_account"
  private val PageCodePage        = "windows-1251"
  private val Headers             = List("User-Agent" -> "Mozilla/5.0 (X11; U; Linux x86_64; ru; rv:1.8.1.12) Gecko/20080129")
  val LOG                         = Logger.getLogger("byfly")
  private val SessionCookieName   = "session_id"
  private val PasswordFieldName   = "Password"
  private val LoginFieldName      = "mobnum"
  val ConfigErrorText             = "Задайте конфигурационный файл или логин и пароль"
  val MainErrorText               = "Введен неверный логин или пароль"
  val ErrorText                   = "Введен неверный пароль, либо этот номер заблокирован"
  private val ConnectionTimeout   = 3600 * 1000

}

class Byfly(login: String, password: String) {

  val byflyHttpOptions: List[HttpOptions.HttpOption] =
    List(HttpOptions.connTimeout(Byfly.ConnectionTimeout), HttpOptions.readTimeout(Byfly.ConnectionTimeout))

  def cookiesToMap(st: String): Map[String, String] = {
    st.split("; ").map(_.split("=", 2)).foldLeft(Map[String, String]())((m, s) => m + (
      s match {
        case Array(a, b) => a -> b
        case _ => "" -> ""
      }))
  }

  def getLoginParams: List[(String, String)] =
    List((Byfly.LoginFieldName, login), (Byfly.PasswordFieldName, password))

  lazy val sessionHeaders: List[(String, String)] =
    List(("Cookie", Byfly.SessionCookieName + "=" + getSessionCookie)) ::: Byfly.Headers

  def getSessionCookie: String = {
    val data = Http.post(Byfly.UrlLogin).headers(Byfly.Headers).
      options(byflyHttpOptions).
      params(getLoginParams).
      asCodeHeaders
    data._2.get("Set-cookie") match {
      case Some(list: List[String]) => {
        for (
          map <- list.map(cookiesToMap);
          (name, value) <- map
          if name == Byfly.SessionCookieName
        ) yield value}.mkString
      case _ => ""
    }
  }

  def getBalancePage: String = Http(Byfly.UrlAccount).headers(sessionHeaders).
    options(byflyHttpOptions).charset(Byfly.PageCodePage).asString

  def getData: Option[(String, String)] = {
    val html = getBalancePage
    if (html contains Byfly.ErrorText) {
      Byfly.LOG.log(Level.SEVERE, Byfly.ErrorText)
      None
    } else {
      Some(ByflyParser.parsePage(html))
    }
  }

  def apply() {
    try {
      getData match {
        case Some((balance, tariff)) => {
          ByflyOutput.prn("Баланс: ", Some(Color.GREEN))(balance)
          ByflyOutput.prn("Тариф: ", Some(Color.GREEN))(tariff)
        }
        case _ =>
      }
    } catch {
      case scalaj.http.HttpException(403, message, _, _) => Byfly.LOG.log(Level.SEVERE, "%s: %s" format (Byfly.MainErrorText, message))
      case ex: Exception => Byfly.LOG.log(Level.SEVERE, ex.getMessage)
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
    (if (filename.isDefined) new File(filename.get) :: list else list).filter(checkFileExists).map(_.getCanonicalPath)
  }

  def readConfigs(filename: Option[String]): (Option[String], Option[String]) = {
    val data = mergeConfigs(getConfigs(filename) map parseConfig)
    (data(0), data(1))
  }

  def mergeConfigs(list: List[(Option[String], Option[String])]): List[Option[String]] = {
    val (a, b) = list.unzip
    List(a, b).map(_.collect { case Some(x) => x }).map(x => if (x.isEmpty) None else Some(x(0)))
  }

  def parseConfig(filename: String): (Option[String], Option[String]) = {
    val config = Configuration.load(filename)
    (config.get[String]("login"), config.get[String]("password"))
  }

}

object Main {

  def main(args: Array[String]) {

    val conf = new ByflyOptions(args)

    val fileName   = if (conf.filename.isEmpty) None else Some(conf.filename())

    def getCredentials(conf: ByflyOptions): (Option[String], Option[String]) = {
      if (conf.login.isEmpty || conf.password.isEmpty) {
        // we should read conf file
        ByflyConfig.readConfigs(fileName)
      } else {
        (Some(conf.login()), Some(conf.password()))
      }
    }

    getCredentials(conf) match {
      case (Some(login), Some(password)) => {
        new Byfly(login, password)()
      }
      case _ => {
        Byfly.LOG.info(Byfly.ConfigErrorText)
        conf.printHelp()
        sys.exit(1)
      }
    }
  }
}
