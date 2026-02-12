package io.via.types

case class RouteInfo(
    method: Method,
    target: String,
    matcher: RouteMatcher,
    params: Params,
    query: Query
)
