package io.via.core

import io.via.types.Query.*
import io.via.types.Query.QueryType.*
import io.via.types.*
import io.via.types.RegexType.*
import io.via.types.RouteQuery.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

case class RouteException(message: String) extends Exception(message)

object RouteMacher:

  case class RouteData(
      route: Route,
      matcher: RouteMatcher,
      params: Params = Params(),
      query: Query = Query()
  )
  def matchRoute(
      method: Method,
      target: String,
      routes: Seq[Route]
  ): Option[RouteData] =

    val urlParts = target.split('?').toList
    val uri = urlParts.head
    val uriQuery = urlParts.lastOption

    @tailrec
    def find(rts: List[Route]): Option[RouteData] =
      rts match
        case x :: xs =>
          val regex = Regex(x.pattern)
          val results = regex.findAllIn(uri)

          if results.isEmpty
          then find(xs)
          else
            val allowedMethod =
              method == Method.ANY || x.methods.exists(m =>
                m == Method.ANY || m == method
              )

            if !allowedMethod
            then find(xs)
            else
              val queries = parseQueries(x.path, getQuery(uriQuery))
              val query = queries |> mkQuery
              x.params match
                case Nil =>
                  Some(
                    RouteData(
                      x,
                      mkRouteMatcher(x, Nil),
                      Params(Nil),
                      query
                    )
                  )
                case pathParams =>
                  val params = mutable.ListBuffer[Param]()
                  for i <- 1 to results.groupCount do
                    val paramVal = results.group(i)
                    val param = pathParams(i - 1)
                    (param.regex.typ match
                      case RegexStr =>
                        ParamStr(param.name, paramVal)
                      case RegexInt =>
                        ParamInt(param.name, paramVal.toInt)
                      case RegexLong =>
                        ParamLong(param.name, paramVal.toLong)
                      case RegexPaths =>
                        ParamPaths(param.name, paramVal.split("/").toList)
                    ) |> params.append

                  val pars = params.toSeq
                  val matcher = mkRouteMatcher(x, pars)
                  Some(RouteData(x, matcher, Params(pars), query))

        case _ => None

    find(routes.toList)

  private def parseQueries(
      path: Path,
      targetQuery: Map[String, String]
  ): List[QueryParam] =
    path.query match
      case Nil =>
        targetQuery.map { case (k, v) => QueryParam(k, QueryStr(v)) }.toList
      case dslQueries =>
        dslQueries.map {
          case RouteQueryInt(name) =>
            QueryParam(
              name,
              toQueryTypeOrError(
                findQueryValue(targetQuery, name),
                toQueryIntOpt
              )
            )
          case RouteQueryLong(name) =>
            QueryParam(
              name,
              toQueryTypeOrError(
                findQueryValue(targetQuery, name),
                toQueryLongOpt
              )
            )
          case RouteQueryStr(name) =>
            QueryParam(
              name,
              toQueryTypeOrError(
                findQueryValue(targetQuery, name),
                toQueryStrOpt
              )
            )
          case RouteQueryBool(name) =>
            QueryParam(
              name,
              toQueryTypeOrError(
                findQueryValue(targetQuery, name),
                toQueryBoolOpt
              )
            )
          case RouteQueryOpt(typ) =>
            typ match
              case RouteQueryList(lstType) =>
                QueryParam(getQueryName(lstType), toQueryOpt(targetQuery, typ))
              case _ =>
                QueryParam(getQueryName(typ), toQueryOpt(targetQuery, typ))
          case RouteQueryList(typ) =>
            (getQueryName(typ), toQueryList(targetQuery, typ)) match
              case (name, QueryList(Nil)) => QueryParam(name, QueryInvalid())
              case (name, ql)             => QueryParam(name, ql)
          case typ => throw RouteException(s"invalid route query type: $typ")
        }

  private def toQueryIntOpt(v: String): Option[QueryInt] =
    v.toIntOption.map(QueryInt.apply)

  private def toQueryLongOpt(v: String): Option[QueryLong] =
    v.toLongOption.map(QueryLong.apply)

  private def toQueryStrOpt(v: String): Option[QueryStr] =
    Option.when(v.nonEmpty)(v).map(QueryStr.apply)

  private def toQueryRegexOpt(pattern: String)(v: String): Option[QueryStr] =
    if Regex(pattern).matches(v) then Some(QueryStr(v)) else None

  private def toQueryBoolOpt(v: String): Option[QueryBool] =
    v.toBooleanOption.map(QueryBool.apply)

  private def toQueryList[T](
      v: Option[String],
      f: String => Option[T]
  ): List[Option[T]] = v match
    case Some(s) => s.split(",").toList.map(f)
    case None    => Nil

  private def toQueryTypeOrError[T <: QueryType](
      v: Option[String],
      f: String => Option[T]
  ): QueryType = v match
    case Some(s) => f(s).getOrElse(QueryInvalid())
    case None    => QueryInvalid()

  private def unwrapRouteTypeOpt[T <: QueryType](v: Option[T]): T |
    QueryInvalid = v match
    case Some(r) => r
    case None    => QueryInvalid()

  private def toQueryList(
      qs: Map[String, String],
      typ: RouteQueryListType
  ): QueryType = typ match
    case RouteQueryInt(name) =>
      toQueryList(findQueryValue(qs, name), toQueryIntOpt).map(
        unwrapRouteTypeOpt
      ) |> QueryList.apply
    case RouteQueryLong(name) =>
      toQueryList(findQueryValue(qs, name), toQueryLongOpt).map(
        unwrapRouteTypeOpt
      ) |> QueryList.apply
    case RouteQueryStr(name) =>
      toQueryList(findQueryValue(qs, name), toQueryStrOpt).map(
        unwrapRouteTypeOpt
      ) |> QueryList.apply
    case RouteQueryRegex(name, pattern) =>
      toQueryList(findQueryValue(qs, name), toQueryRegexOpt(pattern)).map(
        unwrapRouteTypeOpt
      ) |> QueryList.apply
    case _ => QueryInvalid()

  private def toQueryOpt(
      qs: Map[String, String],
      typ: RouteQueryOptType
  ): QueryOption = typ match
    case RouteQueryInt(name) =>
      findQueryValue(qs, name).flatMap(toQueryIntOpt) |> QueryOption.apply
    case RouteQueryStr(name) =>
      findQueryValue(qs, name).flatMap(toQueryStrOpt) |> QueryOption.apply
    case RouteQueryRegex(name, pattern) =>
      findQueryValue(qs, name).flatMap(
        toQueryRegexOpt(pattern)
      ) |> QueryOption.apply
    case RouteQueryLong(name) =>
      findQueryValue(qs, name).flatMap(toQueryLongOpt) |> QueryOption.apply
    case RouteQueryBool(name) =>
      findQueryValue(qs, name).flatMap(toQueryBoolOpt) |> QueryOption.apply
    case RouteQueryList(typ) =>
      toQueryList(qs, typ) match
        case ql @ QueryList(l) if l.nonEmpty => Some(ql) |> QueryOption.apply
        case _                               => None |> QueryOption.apply

  private def getQueryName(
      typ: RouteQueryListType | RouteQueryOptType
  ): String = typ match
    case RouteQueryInt(name)      => name
    case RouteQueryLong(name)     => name
    case RouteQueryStr(name)      => name
    case RouteQueryBool(name)     => name
    case RouteQueryRegex(name, _) => name
    case typ =>
      throw RouteException(
        s"invalid route query type to get query name: ${typ}"
      )

  private def findQueryValue(
      qs: Map[String, String],
      name: String
  ): Option[String] =
    qs.find((k, _) => k == name).map(_._2)

  private def getQuery(query: Option[String]): Map[String, String] =
    query match
      case Some(queries) =>
        queries
          .split("&")
          .foldRight(Map[String, String]()) { (value, acc) =>
            value.split("=").toList match
              case k :: v :: Nil =>
                acc + (k -> v)
              case _ => acc
          }
      case None => Map()

  private def mkQuery(queries: List[QueryParam]): Query =
    def extract(v: QueryType): Matchable = v match
      case QueryInt(v)  => v
      case QueryLong(v) => v
      case QueryStr(v)  => v
      case QueryBool(v) => v
      case QueryList(v) => v.map(extract)
      case QueryOption(v) =>
        v match
          case Some(s) => Some(s |> extract)
          case None    => None
      case typ =>
        throw RouteException(s"invalid route query type to extract: ${typ}")

    val matcher =
      queries.map:
        case QueryParam(name, value) =>
          extract(value).asInstanceOf[QueryMatcherType]

    val tuple: Seq[(String, Matchable)] =
      queries.map:
        case QueryParam(name, value) =>
          (name, extract(value))

    Query(queries, matcher, tuple)

  private def mkRouteMatcher(route: Route, params: Seq[Param]): RouteMatcher =
    val matcher: RouteMatcher =
      route.path.parts.map:
        case p: PathParam =>
          params.find(_.name == p.name) match
            case Some(ParamInt(_, v))      => v
            case Some(ParamLong(_, v))     => v
            case Some(ParamStr(_, v))      => v
            case Some(ParamPaths(_, tail)) => tail.head
            case _ =>
              throw RouteException(
                s"param ${p.name} not found to route match creator"
              )
        case p: PathRoot => p
        case p: PathEnd  => p
        case PathPart(v) => v
        case p =>
          throw RouteException(s"wrong path part $p to route match creator")
    route.path.parts.lastOption match
      case Some(ParamPaths(_, tail)) => matcher ::: tail.tail
      case _                         => matcher
