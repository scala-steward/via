package io.via.types

/** Router controller
  * @tparam Req
  *   Request type
  * @tparam Resp
  *   Response type
  */
trait Controller[Req, Resp]:

  private type Maybe = Resp | Unit

  /** Handle http GET method
    * @param req
    *   The request
    * @return
    *   The response
    */
  def get(req: Req): Maybe = ()

  /** Handle http POST method
    * @param req
    *   The request
    * @return
    *   The response
    */
  def post(req: Req): Maybe = ()

  /** Handle http PUT method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def put(req: Req): Maybe = ()

  /** Handle http DELETE method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def delete(req: Req): Maybe = ()

  /** Handle http HEAD method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def head(req: Req): Maybe = ()

  /** Handle http OPTIONS method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def options(req: Req): Maybe = ()

  /** Handle http PATCH method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def patch(req: Req): Maybe = ()

  /** Handle http TRACE method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def trace(req: Req): Maybe = ()

  /** Handle http CONNECT method
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def connect(req: Req): Maybe = ()

  /** Handle all http methods
    *
    * @param req
    *   The request
    * @return
    *   The response
    */
  def handle(req: Req): Maybe = ()

/** Router action
  * @tparam Req
  *   Request type
  * @tparam Resp
  *   Response type
  */
trait HttpHandler[Req, Resp]:

  /** Handle all http methods
    * @param req
    *   The request
    * @return
    *   The response
    */
  def handle(req: Req): Resp
