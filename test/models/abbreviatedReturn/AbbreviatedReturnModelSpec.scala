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

package models.abbreviatedReturn

import assets.BaseConstants
import assets.abbreviatedReturn.AbbreviatedReturnConstants._
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

class AbbreviatedReturnModelSpec extends WordSpec with Matchers with BaseConstants {

  "AbbreviatedReturnModel" must {

    "correctly write to json" when {

      "max values given" in {

        val expectedValue = abbreviatedReturnUltimateParentJson
        val actualValue = Json.toJson(abbreviatedReturnUltimateParentModel)

        actualValue shouldBe expectedValue
      }

      "min values given" in {

        val expectedValue = abbreviatedReturnJsonMin
        val actualValue = Json.toJson(abbreviatedReturnModelMin)

        actualValue shouldBe expectedValue
      }
    }

    "correctly read from Json" when {

      "max values given" in {

        val expectedValue = abbreviatedReturnUltimateParentModel
        val actualValue = abbreviatedReturnUltimateParentJson.as[AbbreviatedReturnModel]

        actualValue shouldBe expectedValue
      }

      "min values given" in {

        val expectedValue = abbreviatedReturnModelMin
        val actualValue = abbreviatedReturnJsonMin.as[AbbreviatedReturnModel]

        actualValue shouldBe expectedValue
      }
    }

    "correctly collect all of the ukCrns" when {

      "ultimate parent crn is given" in {

        val expectedValue = Seq(
          AbbreviatedReturnModel.ultimateParentCrnPath -> crnLetters,
          AbbreviatedReturnModel.reportingCompanyCrnPath -> crn
        )
        val actualValue = abbreviatedReturnUltimateParentModel.ukCrns

        actualValue shouldBe expectedValue
      }

      "deemed parent crn is given" in {

        val expectedValue = Seq(
          AbbreviatedReturnModel.deemedParentCrnPath(0) -> crn,
          AbbreviatedReturnModel.deemedParentCrnPath(1) -> crn,
          AbbreviatedReturnModel.deemedParentCrnPath(2) -> crn,
          AbbreviatedReturnModel.reportingCompanyCrnPath -> crn
        )
        val actualValue = abbreviatedReturnDeemedParentModel.ukCrns

        actualValue shouldBe expectedValue
      }

      "min crns given" in {

        val expectedValue = Seq(
          AbbreviatedReturnModel.reportingCompanyCrnPath -> crn
        )
        val actualValue = abbreviatedReturnModelMin.ukCrns

        actualValue shouldBe expectedValue
      }
    }
  }
}
