package controllers

import javax.inject._
import models.Coffee
import persistence.CoffeeRepository
import play.api.libs.json.{JsError, JsResult, JsValue, Json}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class HomeController @Inject()(cc: ControllerComponents, coffeeRepository: CoffeeRepository)(implicit executionContext: ExecutionContext) extends AbstractController(cc) {

  populateDate()


  def index() = Action.async { implicit request: Request[AnyContent] =>

    val fCoffees: Future[Seq[Coffee]] = coffeeRepository.all()

    fCoffees.map(s => Ok(Json.toJson(s)))
  }

  def lower(limit: Int) = Action.async { implicit request: Request[AnyContent] =>

    val fCoffees: Future[Seq[Coffee]] = coffeeRepository.lower(limit)

    fCoffees.map(s => Ok(Json.toJson(s)))
  }

  def add() = Action.async(parse.json[Coffee]) { request =>
    insertCoffee(request.body)
  }

  private def insertCoffee(coffee: Coffee): Future[Result] = {
    coffeeRepository.insert(coffee)
      .map(s => Ok(Json.toJson(s)))
      .recoverWith {
        case _: Exception => Future.successful(InternalServerError("No pudo guardarse el registro"))
      }
  }

  //puedo obtener todos los valores que vienen en la etiqueta nombre al enviar varios objetos
  def addFruta() = Action { request =>
    val body: AnyContent = request.body
    val bodyParser: Option[JsValue] = body.asJson
    bodyParser.map { json =>
      //con el doble \\ se extrae del JsValue que es el tipo de variable json unan nueva lista con todos los nombres de todos los elementos
      Ok("prueba " + (json \\ "name"))
    }.getOrElse {
      BadRequest("Expecting application/json request body")
    }
  }

  def addFrutaJson() = Action { request =>
    val body: Option[JsValue] = (request.body).asJson
    Ok(body.get)
  }

  //busca el contenido de la eqtiqueta json que viene del frontend ,me retorna solo el nombre y en insomnia
  def buscarFruta() = Action { request =>
    val bodyParser: Option[JsValue] = (request.body).asJson
    bodyParser.map {
      json =>
        Ok((json \ "nombre").get)
    }.getOrElse {
      BadRequest("Expecting application/json request body")
    }
  }

  //otra forma de buscar elementos del json que viene del frontend
  def buscarFruta1() = Action { request =>
    val bodyParser: Option[JsValue] = (request.body).asJson
    bodyParser.map {
      json =>
        Ok((json("nombre")))
    }.getOrElse {
      BadRequest("Expecting application/json request body")
    }
  }

  val converterJsValue: JsValue = Json.parse(
    """
  {
    "name" : "Felipe Osorio",
    "location" : {
      "lat" : 51.235685,
      "long" : -1.309197
    },
    "residents" : [ {
      "name" : "Chucho",
      "age" : 4,
      "role" : null
    }, {
      "name" : "Bigwig",
      "age" : 6,
      "role" : "Owsla"
    } ]
  }
  """)

  def validateInformation()= Action{ request =>
    val bodyParser: Option[JsValue] =request.body.asJson

    val nameResult = ( bodyParser.map(json=>json("nombre")))
    nameResult match {
      case s => Ok("el nombre es "+ s.get)
      case e => BadRequest("el nombre qeu envio no es un String")
    }
  }

  def converterToJsValue(): Action[AnyContent] = Action { request =>
    Ok(converterJsValue)
  }

  def converterToStringOfJsValue = Action { request =>
    val convertirToString = Json.stringify(converterJsValue)
    //prettyprint me lo manda con formato pero sigue siendo string
    val convertirToPrettyString = Json.prettyPrint(converterJsValue)
    Ok("conversor" + convertirToString + "pretty" + convertirToPrettyString)
  }


  private def populateDate() {
    insertCoffee(new Coffee("Expresso", 1200, ""))
    insertCoffee(new Coffee("Tostado", 1500, ""))
    insertCoffee(new Coffee("Negro", 2000, ""))
  }

}
