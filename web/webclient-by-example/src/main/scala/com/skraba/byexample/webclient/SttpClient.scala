package com.skraba.byexample.webclient

import com.skraba.byexample.webclient.WebClientGo.SimpleResponse
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}

object SttpClient extends WebClientGo.SimpleClient {

  private def toSimple(r: Response[String]): SimpleResponse = SimpleResponse(r.code.code, r.body)

  /** Make a GET request to the server. */
  override def get(path: String): SimpleResponse = toSimple(quickRequest.get(uri"$path").send(DefaultSyncBackend()))

  /** Make a POST request to the server. */
  override def post(path: String, payload: String): SimpleResponse = toSimple(
    quickRequest.post(uri"$path").body(payload).send(DefaultSyncBackend())
  )

  /** Make a PUT request to the server. */
  override def put(path: String, payload: String): SimpleResponse = toSimple(
    quickRequest.put(uri"$path").body(payload).send(DefaultSyncBackend())
  )

  /** Make a DELETE request to the server. */
  override def delete(path: String): SimpleResponse = toSimple(
    quickRequest.delete(uri"$path").send(DefaultSyncBackend())
  )
}
