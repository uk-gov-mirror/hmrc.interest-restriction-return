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

package v1.models.errors

import play.api.libs.json.{Json, Writes}

case class ErrorResponseModel(code: String, message: String)

object ErrorResponseModel {
  implicit val writes: Writes[ErrorResponseModel] = Json.writes[ErrorResponseModel]
}

//Standard Errors
object NotFoundError extends ErrorResponseModel("MATCHING_RESOURCE_NOT_FOUND", "Matching resource not found")
object DownstreamError extends ErrorResponseModel("INTERNAL_SERVER_ERROR", "An internal server error occurred")
object BadRequestError extends ErrorResponseModel("INVALID_REQUEST", "Invalid request")
object InvalidBodyTypeError extends ErrorResponseModel("INVALID_BODY_TYPE", "Expecting text/json or application/json body")

//Authorisation Errors
object UnauthorisedError extends ErrorResponseModel("CLIENT_OR_AGENT_NOT_AUTHORISED", "The client and/or agent is not authorised")

// Accept header Errors
object InvalidAcceptHeaderError extends ErrorResponseModel("ACCEPT_HEADER_INVALID", "The accept header is missing or invalid")
object UnsupportedVersionError extends ErrorResponseModel("NOT_FOUND", "The requested resource could not be found")




