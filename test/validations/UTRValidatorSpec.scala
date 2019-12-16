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

package validations

import models.UTRModel
import org.scalatest.{Matchers, WordSpec}
import validation._

class UTRValidatorSpec extends WordSpec with Matchers{

  "UTR Validation" when {

    "UTR is supplied and fails Check Sum" in {
      val model  = UTRModel("1234567890")
      model.validate.toEither.left.get.head.errorMessages shouldBe UTRError.errorMessages
    }

    "UTR is supplied and passes Check Sum" in {
      val model  = UTRModel("9534668155")
      model.validate.toEither.right.get shouldBe model
    }
  }
}
