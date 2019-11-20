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

package schemas.subSchemas

import play.api.libs.json.{JsValue, Json}
import schemas.BaseSchemaSpec
import schemas.helpers.fullReturn.AllocatedRestrictions

class AllocatedRestrictions extends BaseSchemaSpec {

  def validate(json: JsValue): Boolean = validateJson("subSchemas/allocatedRestrictions.json", json)

  "allocatedRestrictions" should {

    "Return valid" when {

      "valid JSON is received" in {
        validate(Json.toJson(AllocatedRestrictions())) shouldBe true
      }
    }

    "Return invalid" when {

      "ap1End" when {

      }

      "ap2End" when {

      }

      "ap3End" when {

      }

      "disallowanceAp1" when {

      }

      "disallowanceAp2" when {

      }

      "disallowanceAp3" when {

      }

    }
  }
}
