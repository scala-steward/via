package io.via.types

import scala.collection.mutable

case class Route(
    path: Path,
    pattern: String = "",
    pure: Boolean = false, // if has not params and query
    params: List[PathParam] = Nil,
    methods: Seq[Method] = Nil,
    tag: Option[Any] = None
):

  infix def /(p: Path): Path =
    path.copy(parts = path.parts ::: p.parts)

  def compile: Route =

    val params = mutable.ListBuffer[PathParam]()

    val routeParts =
      path.parts
        .map[String]:
          case _: PathRoot => ""
          case _: PathEnd  => "/$"
          case _: PathAny  => "/(.+)$"
          case p: PathPart => s"/${p.path}"
          case p: PathParam =>
            params.append(p)
            p.regex match
              case PathParamRegex(_, pattern, _) => s"/($pattern)" // (group)
        .filterNot(_.isEmpty)

    val pattern =
      routeParts match
        case Nil => "^/$"
        case _ =>
          val str = routeParts.mkString
          if str.endsWith("$")
          then s"^$str"
          else s"^$str$$"

    val pars = params.toList
    copy(pattern = pattern, params = pars, pure = pars.isEmpty)
/*
object Route:

  def route(path: Path): Route =
    Route(path).compile

  /** Analyze route path
 * @param path
 *   E.g. / /user/:id -> any value (string param) /user/:id([0-9]) -> int
 *   value (int param) /user/:id(int) -> int value (int param) /user/\* ->
 *   tail paths (seq param) /user/:id([0-9]+) -> regrex value (string param)
 *
 * @return
 */
  def route(path: String): Route =
    if path == "/"
    then route(root)
    else
      val parts =
        path.split("/").foldLeft(root) { (acc: Path, part: String) =>
          if part.isEmpty
          then acc
          else
            val re = "^:(\\w+)(\\(.*\\))?".r
            re.findFirstMatchIn(part) match
              case Some(matcher) =>
                val varName = matcher.group(1)
                matcher.group(2) match
                  case null                             => acc / param(varName)
                  case "([0-9])" | "([0-9]+)" | "(int)" => acc / int(varName)
                  case value => acc / regex(varName, value)
              case _ =>
                part.charAt(0) match
                  case '*' =>
                    acc / tail("paths")
                  case _ =>
                    acc / part
        }
      route(parts)
 */
