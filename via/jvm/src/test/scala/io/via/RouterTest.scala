package io.via

import org.scalatest.funsuite.AnyFunSuite
import via.*
import io.via.core.{RequestBuilder, Router}
import io.via.types.{Method, Params, Query, RouteInfo, RouteMatcher}

type Headers = Map[String, String]

case class Auth(username: String, token: String)

case class Request(
    method: Method,
    target: String,
    params: Params,
    query: Query,
    matcher: RouteMatcher,
    body: Option[String] = None,
    headers: Headers = Map(),
    auth: Option[Auth] = None
)

case class NativeRequest(body: Option[String] = None, headers: Headers = Map())

trait ResponseBase

case class Response[T](
    status: Int,
    body: Option[T] = None,
    contentType: Option[String] = None
) extends ResponseBase

type ResponseText = Response[String]

object Response:
  def notFound: ResponseText = Response(404)
  def serverError: ResponseText = Response(500)
  def badRequest: ResponseText = Response(400)

  def unauthorized: ResponseText = Response(401)
  def ok: ResponseText = Response(200)

  def apply(status: Int, body: String): ResponseText =
    Response(status, Some(body), None)

  def apply[T](status: Int, body: T): Response[T] =
    Response(status, Some(body), None)

  def apply(status: Int, body: String, contentType: String): ResponseText =
    Response(status, Some(body), Some(contentType))

  def apply[T](status: Int, body: T, contentType: String): Response[T] =
    Response(status, Some(body), Some(contentType))

// sbt testOnly *RouterTest
class RouterTest extends AnyFunSuite:

  given RequestBuilder[Request, NativeRequest]:
    override def build(
        routeInfo: RouteInfo,
        nativeRequest: Option[NativeRequest]
    ): Request =
      Request(
        routeInfo.method,
        routeInfo.target,
        routeInfo.params,
        routeInfo.query,
        routeInfo.matcher,
        body = nativeRequest.flatMap(_.body),
        headers = nativeRequest.map(_.headers).getOrElse(Map())
      )

  val users = Map(
    "123456" -> "jonh@gmail.com"
  )

  test("router GET /") {

    val entry = route(GET, root) { (req: Request) =>
      Response(200, "OK", "text/plain")
    }

    val router = Router[Request, ResponseText, NativeRequest](entry)

    router.dispatch(GET, "/") match
      case Some(resp) =>
        assert(resp.body.contains("OK"), "expected response OK")
      case None => fail("resp can't be none")
  }

  test("router GET and POST /") {

    val entry = route(verbs(GET, POST), root) { (req: Request) =>
      Response(200, s"OK ${req.method.verb}", "text/plain")
    }

    val router = Router[Request, ResponseText, NativeRequest](entry)

    router.dispatch(GET, "/") match
      case Some(resp) =>
        assert(resp.body.contains("OK GET"), "expected response OK GET")
      case None => fail("resp can't be none")

    router.dispatch(POST, "/") match
      case Some(resp) =>
        assert(resp.body.contains("OK POST"), "expected response OK POST")
      case None => fail("resp can't be none")
  }

  test("router GET  with auth middleware") {

    val index = route(GET, root) { (req: Request) =>
      Response(200, s"hello ${req.auth.get.username}", "text/plain")
    }

    val auth = enter { (req: Request) =>
      req.headers.get("Authorization") match
        case Some(token) =>
          users.get(token) match
            case Some(username) =>
              val auth = Auth(username, token) |> Some.apply
              req.copy(auth = auth)
            case _ => Response.unauthorized
        case _ => Response.unauthorized
    }

    val authIndex = auth ++ index

    val router = Router[Request, ResponseText, NativeRequest](authIndex)
    val nativeRequest =
      NativeRequest(headers = Map("Authorization" -> "123456"))
    router.dispatch(GET, "/", nativeRequest) match
      case Some(resp) =>
        assert(
          resp.body.contains("hello jonh@gmail.com"),
          "expected response hello jonh@gmail.com"
        )
      case None => fail("resp can't be none")

    router.dispatch(GET, "/") match
      case Some(resp) =>
        assert(
          resp.status == 401,
          "expected response status 401"
        )
      case None => fail("resp can't be none")

  }

  test("router GET  with auth and validation middleware") {

    val index = route(GET, root) { (req: Request) =>
      Response(200, s"${req.body.get} ${req.auth.get.username}", "text/plain")
    }

    val auth = enter { (req: Request) =>
      req.headers.get("Authorization") match
        case Some(token) =>
          users.get(token) match
            case Some(username) =>
              val auth = Auth(username, token) |> Some.apply
              req.copy(auth = auth)
            case _ => Response.unauthorized
        case _ => Response.unauthorized
    }

    val validation = enter { (req: Request) =>
      req.body match
        case None => Response.badRequest
        case _    => req
    }

    val authIndex = auth ++ validation ++ index

    val router = Router[Request, ResponseText, NativeRequest](authIndex)
    val nativeRequest = NativeRequest(
      body = Some("hello"),
      headers = Map("Authorization" -> "123456")
    )
    router.dispatch(GET, "/", nativeRequest) match
      case Some(resp) =>
        assert(
          resp.body.contains("hello jonh@gmail.com"),
          "expected response hello jonh@gmail.com"
        )
      case None => fail("resp can't be none")

    router.dispatch(GET, "/", nativeRequest.copy(body = None)) match
      case Some(resp) =>
        assert(
          resp.status == 400,
          "expected response status 400"
        )
      case None => fail("resp can't be none")
  }

  test("router GET  with json middleware") {

    val index = route[Request, ResponseBase](GET, root) { (req: Request) =>
      Response(200, Map("name" -> "ricardo"))
    }

    val json = leave { (_: Request, resp: ResponseBase) =>
      resp match
        case r: Response[Map[String, String]] =>
          val fields = r.body.map(_.map((k, v) => s"\"$k\": \"$v\""))
          val jsonStr = s"{${fields.get.head}}"
          Response[String](
            status = r.status,
            body = jsonStr,
            contentType = "application/json"
          )
        case _ => resp
    }

    val authIndex = index ++ json

    val router = Router[Request, ResponseBase, NativeRequest](authIndex)

    router.dispatch(GET, "/") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("{\"name\": \"ricardo\"}"),
          "expected response hello jonh@gmail.com"
        )
      case None => fail("resp can't be none")

  }

  test("router GET  with string route path") {

    val index = route[Request, ResponseBase](GET, "/") { (req: Request) =>
      Response(200, "index")
    }

    val users = route[Request, ResponseBase](GET, "/user") { (req: Request) =>
      Response(200, "users")
    }

    val userGet = route[Request, ResponseBase](GET, "/user/:id(int)") {
      (req: Request) =>
        Response(200, s"user/${req.params.int("id").get}")
    }

    val peopleName = route[Request, ResponseBase](GET, "/people/:name") {
      (req: Request) =>
        Response(200, s"people/${req.params.str("name").get}")
    }

    val tail = route[Request, ResponseBase](GET, "/head/*") { (req: Request) =>
      Response(200, s"head/${req.params.tail("paths").mkString(",")}")
    }

    val router = Router[Request, ResponseBase, NativeRequest](
      index,
      users,
      userGet,
      peopleName,
      tail
    )

    router.dispatch(GET, "/") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("index"),
          "expected response index"
        )
      case _ => fail("resp can't be none")

    router.dispatch(GET, "/user") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("users"),
          "expected response users"
        )
      case _ => fail("resp can't be none")

    router.dispatch(GET, "/user/55") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("user/55"),
          "expected response user/55"
        )
      case _ => fail("resp can't be none")

    router.dispatch(GET, "/people/ricardo") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("people/ricardo"),
          "expected response people/ricardo"
        )
      case _ => fail("resp can't be none")

    router.dispatch(GET, "/head/a/b/c") match
      case Some(resp: ResponseText) =>
        assert(
          resp.body.contains("head/a,b,c"),
          "expected response head/a,b,c"
        )
      case _ => fail("resp can't be none")

  }
