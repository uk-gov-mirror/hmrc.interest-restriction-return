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

package validation

import models.Validation.ValidationResult
import models.{NonConsolidatedInvestmentModel, Validation}
import play.api.libs.json.{JsPath, Json}

trait NonConsolidatedInvestmentValidator extends BaseValidation {

  import cats.implicits._

  val nonConsolidatedInvestmentModel: NonConsolidatedInvestmentModel

  private def validateInvestmentName(implicit path: JsPath): ValidationResult[String] = {
    if (nonConsolidatedInvestmentModel.nonConsolidatedInvestment.length >= 1 && nonConsolidatedInvestmentModel.nonConsolidatedInvestment.length <= 32767) {
      nonConsolidatedInvestmentModel.nonConsolidatedInvestment.validNec
    } else {
      NonConsolidatedInvestmentNameError(nonConsolidatedInvestmentModel.nonConsolidatedInvestment).invalidNec
    }
  }

  def validate(implicit path: JsPath): ValidationResult[NonConsolidatedInvestmentModel] = {
    validateInvestmentName.map(_ => nonConsolidatedInvestmentModel)
  }
}

case class NonConsolidatedInvestmentNameError(nonConsolidatedInvestment: String)(implicit val topPath: JsPath) extends Validation {
  val errorMessage: String = "NonConsolidatedInvestment names must not be more than 32767"
  val path = topPath \ "investorGroups"
  val value = Json.toJson(nonConsolidatedInvestment)
}





