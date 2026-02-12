package io.via.core

import io.via.types.Query.*
import QueryType.*
import io.via.types.*

object RouteChain:

  sealed trait Issue
  case class QueryIssue(name: String) extends Issue

  sealed trait RouteResult
  case class RouteNotFound() extends RouteResult
  case class RouteFound(
      route: Route,
      matcher: RouteMatcher,
      params: Params,
      query: Query,
      issues: List[Issue] = Nil
  ) extends RouteResult

  def chain(target: String, routes: Seq[Route]): RouteResult =
    chain(Method.ANY, target, routes)

  def chain(method: Method, target: String, routes: Seq[Route]): RouteResult =
    RouteMacher.matchRoute(method, target, routes) match
      case Some(info) =>
        val issues =
          info.query.raw
            .filter {
              case QueryParam(name, QueryInvalid()) => true
              case QueryParam(name, QueryList(l)) =>
                l.exists {
                  case QueryInvalid() => true
                  case _              => false
                }
              case _ => false
            }
            .map(s => QueryIssue(s.name))

        RouteFound(info.route, info.matcher, info.params, info.query, issues)

      case None => RouteNotFound()
