import io.via.core.{RequestBuilder, Router}
import io.via.types.*

package object via:

  export io.via.types.{
    Params,
    Query,
    RouteMatcher,
    // Route,
    Param,
    ParamInt,
    ParamStr,
    ParamLong,
    ParamPaths,
    // RouteEntry,
    RouteInfo,
    // Enter,
    // Leave,
    Method,
    // Controller,
    // Handler,
    // Dispatcher,
    // MiddlewareEnter,
    // MiddlewareLeave,
    |>
  }

  object types:
    export io.via.types.{
      Route,
      RouteEntry,
      Enter,
      Leave,
      Controller,
      HttpHandler,
      Dispatcher,
      MiddlewareEnter,
      MiddlewareLeave
    }

  export Path.*
  export Router.{leave, enter, route, verbs}
  export io.via.core.{RequestBuilder, Router}
  export Method.{
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
    ANY
  }
  export io.via.core.RouteChain
  export RouteChain.{RouteFound, RouteNotFound}
  export RouteQuery.*
