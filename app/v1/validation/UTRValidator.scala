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

package v1.validation

import play.api.libs.json.{JsPath, Json}
import v1.models.Validation.ValidationResult
import v1.models.{UTRModel, Validation}

trait UTRValidator extends BaseValidation {

  import cats.implicits._

  val utrModel: UTRModel

  def validate(implicit path: JsPath): ValidationResult[UTRModel] = {

    val utrInts = utrModel.utr.map(_.asDigit)

    def checkSum = {

      val utrSum = (utrInts(1) * 6) + (utrInts(2) * 7) + (utrInts(3) * 8) + (utrInts(4) * 9) + (utrInts(5) * 10) + (utrInts(6) * 5) + (utrInts(7) * 4) + (utrInts(8) * 3) + (utrInts(9) * 2)

      val utrCalc = 11 - (utrSum % 11)

      if (utrCalc > 9) utrCalc - 9 else utrCalc
    }

    if(utrModel.utr.length != 10) {
      UTRLengthError(utrModel).invalidNec
    } else {
      if (checkSum == utrInts(0)) {
        utrModel.validNec
      } else {
        UTRChecksumError(utrModel).invalidNec
      }
    }
  }
}

case class UTRChecksumError(utrValue: UTRModel)(implicit val path: JsPath) extends Validation {
  val errorMessage: String = "UTR Check Sum does not satisfy the check sum"
  val value = Json.toJson(utrValue)
}

case class UTRLengthError(utrValue: UTRModel)(implicit val path: JsPath) extends Validation {
  val errorMessage: String = s"UTR is ${utrValue.utr.length} character${if (utrValue.utr.length != 1) "s" else ""} long and should be 10"
  val value = Json.toJson(utrValue)
}