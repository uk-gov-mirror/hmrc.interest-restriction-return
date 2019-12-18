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

package validation.abbreviatedReturn

import assets.abbreviatedReturn.UkCompanyConstants._
import play.api.libs.json.JsPath
import validation.{BaseValidationSpec, UTRChecksumError}

class UkCompanyValidatorSpec extends BaseValidationSpec {

  implicit val path = JsPath \ "some" \ "path"

  "Reporting Company Validation" should {

    "Return valid" when {

      "a valid Reporting Company model is validated" in {
        rightSide(ukCompanyModel.validate) shouldBe ukCompanyModel
      }
    }

    "Return invalid" when {

      "CTUTR is invalid" in {
        leftSideError(ukCompanyModel.copy(ctutr = invalidUtr).validate).errorMessage shouldBe UTRChecksumError(invalidUtr).errorMessage
      }
    }
  }
}
