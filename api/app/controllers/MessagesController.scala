package controllers

import akka.actor.ActorSystem
import javax.inject._
import play.api._
import play.api.libs.json.JsValue
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.ws.WSClient
import play.api.mvc._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import play.api.libs.ws._
import play.api.libs.json._

/**
 * Created by nfaure on 5/17/16.
 */
@Singleton
class MessagesController @Inject()(implicit ws: WSClient, context: ExecutionContext) extends Controller {

  val ACCESS_TOKEN = "EAAQPssZBjhH0BAIwjpSFlwebqQT9Q8ShPF2nMf1Os1QbZBx5dPm1IsVtt0KrykwbZC632UsOYbxMoFV5HcYV6lYSK7H4DgqnVbQEhRdHQ79ro1vqmI99E22qxinS4IQe2kTLXZCNDBWo6QqBYqIKCdOVRCoqnf08OUWPZBlKYIAZDZD"
  val SEARCH_URL = "https://api.mercadolibre.com/sites/MLA/search"

  def receive = Action { request =>
    Ok(request.getQueryString("hub.challenge").mkString)
  }

  def buildResponse(q: String, json: JsValue) = {
    ws
      .url(SEARCH_URL)
      .withQueryString("q" -> q)
      .get()
      .map { response =>
      println("Volvi de search")

      val searchJson = response.json
      val results = (searchJson \ "results").as[List[JsValue]].take(10)

      val r = results.map { result =>

        val button = Json.obj(
          "type" -> "web_url",
          "url" -> (result \ "permalink").get,
          "title" -> "Comprar"
        )
        val buttons = Json.arr(button)

        val responseElement = Json.obj(
          "title" -> (result \ "title").get,
          "subtitle" -> (result \ "price").get,
          "image_url" -> (result \ "thumbnail").get,
          "buttons" -> buttons
        )

        responseElement
      }

      println(r)
      val responseElements = JsArray(r)

      val payload = Json.obj(
        "template_type" -> "generic",
        "elements" -> r
      )

      val attachment = Json.obj(
        "type" -> "template",
        "payload" -> payload
      )
      val messageResponse = Json.obj(
        "attachment" -> attachment
      )
      val entry = (json \ "entry")(0)
      val message = (entry \ "messaging")(0)
      val sender = (message \ "sender")


      val data = Json.obj(
        "message" -> messageResponse,
        "recipient" -> sender.get
      )

      ws
        .url("https://graph.facebook.com/v2.6/me/messages")
        .withQueryString("access_token" -> ACCESS_TOKEN.toString)
        .post(data).map { response =>
        println("Response was sent to FB with response" + response.body)
      }

    }

  }

  def receiveNewMessage = Action { request =>
    val jsonBody: Option[JsValue] = request.body.asJson
    jsonBody.map { json =>
      println(json.toString)
      val entry = (json \ "entry")(0)
      val message = (entry \ "messaging")(0)
      val msg = (message \ "message")
      if (msg.isInstanceOf[JsDefined]) {
        buildResponse((msg \ "text").get.toString(), json)
      }
      Ok("")
    }.getOrElse {
      BadRequest("")
    }
  }

}
