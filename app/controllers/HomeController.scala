package controllers

import javax.inject._
import models.Coffee
import persistence.CoffeeRepository
import play.api.Logger
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

//con esto creo un action que extiende  de actionbuilder y lo hago sobreescribiendo el metodo invokeBlock
//tener en cuenta que debo declarar implicitamente el executionContext, lo hagod e forma global
class LoggingAction @Inject() (parser: BodyParsers.Default) extends ActionBuilderImpl(parser) {
  override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
    Logger.info("Calling action")
    block(request)
  }
}



@Singleton
class HomeController @Inject()(cc: ControllerComponents, coffeeRepository: CoffeeRepository, loggingAction: LoggingAction)(implicit executionContext: ExecutionContext) extends AbstractController(cc) {

  populateDate()


  def index() = Action.async { implicit request: Request[AnyContent] =>

    val fCoffees: Future[Seq[Coffee]] = coffeeRepository.all()

    fCoffees.map(s => Ok(Json.toJson(s)))
  }
  def actionComposition() = loggingAction {
    Ok("Hello Felipe Osorio")
  }
  def actionCompositionBodyParser(): Action[String] = loggingAction(parse.text) { request =>
    val x: Int = request.body.length
    Ok("Got a body " + s"$x" +"bytes long")
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
        Ok((json \ "nombre").get).withHeaders(
          CACHE_CONTROL -> "max age=100",
          ETAG -> "xx"
        )
    }.getOrElse {
      BadRequest("Expecting application/json request body")
    }
  }

  def buscarFruta11() = Action { request =>
    val bodyParser: Option[JsValue] = (request.body).asJson
    bodyParser.map {
      json =>
        Ok((json \ "nombre").get).withCookies(Cookie("theme", "blue")).bakeCookies()
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

  def validateInformation() = Action { request =>
    val nombre = request.body.asJson.map(s => s("nombre")).get.validate[String]
    //otra forma de validar si es correcto obtiene le valor de nombre si no desplega le mensaje de esta indefinido
    val xorelse = nombre.getOrElse("esta indefinido")
    Logger.debug("la salida de validate info es " + xorelse)
    //esto me devolvera un JsSucces o un JsError
    val nameUpperCase = nombre.map(_.toUpperCase())
    Logger.info("la validacion con map es " + nameUpperCase)

    nombre match {
      case s: JsSuccess[String] => Ok("el nombre es " + s.get)
      case e: JsError => BadRequest("el nombre qeu envio no es un String" + JsError.toJson(e).toString())
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
