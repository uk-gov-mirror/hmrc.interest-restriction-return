/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package audit

import play.api.Logging
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.RequestHeader
import v1.connectors.HttpHelper.SubmissionResponse

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class InterestRestrictionReturnAuditService extends Logging {

  def sendInterestRestrictionReturnEvent(actionPerformed: String, payload: JsValue)(sendEvent: InterestRestrictionReturnAuditEvent => Unit)
                                        (implicit rh: RequestHeader, ec: ExecutionContext): PartialFunction[Try[SubmissionResponse], Unit] = {
    case Success(Right(successFulResponse)) =>
      sendEvent(
        InterestRestrictionReturnAuditEvent(
          action = actionPerformed,
          status = Status.CREATED,
          payload = Some(eventPayload(Json.toJson(successFulResponse),payload))))
    case Success(Left(e)) =>
      sendEvent(
        InterestRestrictionReturnAuditEvent(
          action = actionPerformed,
          status = e.status,
          payload =Some(eventPayload(Json.toJson(e.body),payload))))
    case Failure(t) =>
      logger.error("Error in sending audit event", t)
  }

  private def eventPayload(response: JsValue, payload: JsValue): JsValue = {
    Json.obj("response" -> Json.toJson(response), "payload" -> payload)
  }
}
