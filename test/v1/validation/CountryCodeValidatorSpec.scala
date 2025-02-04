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

import play.api.libs.json.JsPath
import utils.BaseSpec
import v1.models.CountryCodeModel

class CountryCodeValidatorSpec extends BaseSpec {

  implicit val path = JsPath \ "some" \ "path"

  "Country Code Validation" when {

    "Country Code is supplied and is not the correct length" in {
      val model  = CountryCodeModel("A")
      leftSideError(model.validate).errorMessage shouldBe
        errorMessages(CountryCodeLengthError(model).errorMessage, CountryCodeValueError(model).errorMessage)
    }

    "Invalid Country Code supplied" in {
      leftSideError(invalidCountryCode.validate).errorMessage shouldBe CountryCodeValueError(invalidCountryCode).errorMessage
    }

    "Valid Country Code supplied" in {
      rightSide(nonUkCountryCode.validate) shouldBe nonUkCountryCode
    }
  }
}
