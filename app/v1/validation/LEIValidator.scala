/*
 * Copyright 2020 HM Revenue & Customs
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

package v1.validation

import play.api.libs.json.{JsPath, Json}
import v1.models.Validation.ValidationResult
import v1.models.{LEIModel, Validation}
import config.Constants.legalEntityIdentifierRegex

trait LEIValidator extends BaseValidation {

  import cats.implicits._

  val leiModel: LEIModel

  def validate(implicit path: JsPath): ValidationResult[LEIModel] = {

    val isValidIdentifier = leiModel.lei matches legalEntityIdentifierRegex

    if (isValidIdentifier) {
      leiModel.validNec
    } else {
      LEIFormatError(leiModel).invalidNec
    }
  }
}

case class LEIFormatError(leiValue: LEIModel)(implicit val path: JsPath) extends Validation {
  val errorMessage: String = "Legal Entity Identifier does not satisfy the correct regex format"
  val value = Json.toJson(leiValue)
}
