/*
 * Copyright 2019 HM Revenue & Customs
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

package controllers

import play.api.libs.json._
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.play.bootstrap.controller.BackendBaseController
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


case class ValidationErrorResponse(field:JsPath, errors:Seq[JsonValidationError])

object ValidationErrorResponse {
  implicit val writes:Writes[ValidationErrorResponse] = Writes{models =>
    Json.obj("field" -> models.field.toString(), "errors" -> models.errors.map(_.message))
  }
}

trait BaseController extends BackendBaseController {

  implicit val ec: ExecutionContext = controllerComponents.executionContext

  override def withJsonBody[T](
                                 f: (T) => Future[Result])(implicit request: Request[JsValue], m: Manifest[T], reads: Reads[T]): Future[Result] =
    Try(request.body.validate[T]) match {
      case Success(JsSuccess(payload, _)) => f(payload)
      case Success(JsError(errs)) =>
            Future.successful(BadRequest(Json.toJson(errs.map{
              case (field, errors) => ValidationErrorResponse(field, errors)
            })))
      case Failure(e) => Future.successful(BadRequest(s"Could not parse body due to ${e.getMessage}"))
    }
}
