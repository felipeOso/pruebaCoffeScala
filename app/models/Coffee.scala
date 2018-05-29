package models

import play.api.libs.json.{JsPath, Reads, Writes, Format}
import play.api.libs.functional.syntax._

case class Coffee(name: String, price: Double, farm: String)

object Coffee {
 // implicit val cofffeFormat = Json.format[Coffee]

  implicit val readsCoffee: Reads[Coffee] = (
    (JsPath \ "nombre").read[String] and
      (JsPath \ "precio").read[Double] and
      (JsPath \ "granja").read[String]
    ).apply(Coffee.apply _)


  implicit val coffeeFormat: Format[Coffee] = (
    (JsPath \ "nombre").format[String] and
      (JsPath \ "precio").format[Double] and
      (JsPath \ "granja").format[String]
    ).apply(Coffee.apply _, Coffee.unapply(_).get)


implicit val writesCoffee: Writes[Coffee] = (
  (JsPath \ "nombre").write[String] and
    (JsPath \ "nombre").write[Double] and
    (JsPath \ "nombre").write[String]
  ).apply(unlift(Coffee.unapply))
}