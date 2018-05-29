package persistence

import javax.inject.{Inject, Singleton}

import models.Coffee
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CoffeeRepository @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext) extends HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  private val Coffees = TableQuery[CoffeesTable]

  def all() : Future[Seq[Coffee]] = db.run(Coffees.result)

  def insert(coffee: Coffee): Future[Coffee] = db.run(Coffees += coffee).map { _ => coffee }

  def lower(limit: Int): Future[Seq[Coffee]] = db.run(Coffees.filter(_.price < limit.toDouble).result)


  private class CoffeesTable(tag: Tag) extends Table[Coffee](tag, "COFFEE") {

    def name = column[String]("NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def farm = column[String]("FARM")

    def * = (name, price, farm) <> ( (Coffee.apply _).tupled, Coffee.unapply)
  }

}
