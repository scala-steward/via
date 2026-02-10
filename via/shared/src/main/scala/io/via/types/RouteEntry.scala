package io.via.types

type Dispatcher[Req, Resp] = Req => Resp

trait RouteEntry[Req, Resp]:
  val leave: Option[Leave[Req, Resp]] = None
  val enter: Option[Enter[Req, Resp]] = None
  val methods: Seq[Method]
  val route: Route

  def copyWithLeave(m: Leave[Req, Resp]): RouteEntry[Req, Resp]
  def copyWithEnter(m: Enter[Req, Resp]): RouteEntry[Req, Resp]

  def ++(m: Leave[Req, Resp]): RouteEntry[Req, Resp] =
    copyWithLeave(m)

case class RouteEntryHandler[Req, Resp](
    override val methods: Seq[Method],
    override val route: Route,
    override val leave: Option[Leave[Req, Resp]] = None,
    override val enter: Option[Enter[Req, Resp]] = None,
    handler: HttpHandler[Req, Resp]
) extends RouteEntry[Req, Resp]:

  override def copyWithLeave(m: Leave[Req, Resp]): RouteEntry[Req, Resp] =
    copy(leave = Some(m))

  override def copyWithEnter(m: Enter[Req, Resp]): RouteEntry[Req, Resp] =
    copy(enter = Some(m))

case class RouteEntryController[Req, Resp](
    override val methods: Seq[Method],
    override val route: Route,
    override val leave: Option[Leave[Req, Resp]] = None,
    override val enter: Option[Enter[Req, Resp]] = None,
    controller: Controller[Req, Resp]
) extends RouteEntry[Req, Resp]:

  override def copyWithLeave(m: Leave[Req, Resp]): RouteEntry[Req, Resp] =
    copy(leave = Some(m))

  override def copyWithEnter(m: Enter[Req, Resp]): RouteEntry[Req, Resp] =
    copy(enter = Some(m))

case class RouteEntryDispatcher[Req, Resp](
    override val methods: Seq[Method],
    override val route: Route,
    override val leave: Option[Leave[Req, Resp]] = None,
    override val enter: Option[Enter[Req, Resp]] = None,
    dispatcher: Dispatcher[Req, Resp]
) extends RouteEntry[Req, Resp]:

  override def copyWithLeave(m: Leave[Req, Resp]): RouteEntry[Req, Resp] =
    copy(leave = Some(m))

  override def copyWithEnter(m: Enter[Req, Resp]): RouteEntry[Req, Resp] =
    copy(enter = Some(m))
