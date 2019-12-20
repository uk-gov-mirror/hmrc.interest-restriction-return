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
import models.{GroupRatioBlendedModel, InvestorGroupModel, Validation}
import play.api.libs.json.{JsPath, Json}

trait GroupRatioBlendedValidator extends BaseValidation {

  import cats.implicits._

  val groupRatioBlendedModel: GroupRatioBlendedModel

  private def validateGroupRatioBlended(implicit path: JsPath): ValidationResult[GroupRatioBlendedModel] = {
    (groupRatioBlendedModel.isElected,  groupRatioBlendedModel.investorGroups.isDefined) match {
      case (false, true) => GroupRatioBlendedNotElectedError(groupRatioBlendedModel).invalidNec
      case _ => groupRatioBlendedModel.validNec
    }
  }

  def validate(implicit path: JsPath): ValidationResult[GroupRatioBlendedModel] = {

    val investorGroupsValidation: ValidationResult[Option[InvestorGroupModel]] = optionValidations(groupRatioBlendedModel.investorGroups.map(investors =>
      combineValidations(investors.zipWithIndex.map {
        case (a, i) => a.validate(JsPath \ s"investorGroups[$i]")
      }:_*)
    ))

    (validateGroupRatioBlended,
      investorGroupsValidation).mapN((_,_) => groupRatioBlendedModel)
  }
}


case class GroupRatioBlendedNotElectedError(groupRatioBlended: GroupRatioBlendedModel)(implicit val topPath: JsPath) extends Validation {
  val errorMessage: String = "Group Ratio Blended is not elected, unable to supply investor groups"
  val path = topPath \ "groupRatioBlended"
  val value = Json.toJson(groupRatioBlended)
}





