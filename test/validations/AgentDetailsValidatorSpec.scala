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

import models.AgentDetailsModel
import org.scalatest.{Matchers, WordSpec}
import validation._

class AgentDetailsValidatorSpec extends WordSpec with Matchers {

  "Agent Details Validation" when {
    "passed false and No name should succeed" in {
      val model = AgentDetailsModel(false, None)
      model.validate.toEither.right.get shouldBe model
    }

    "passed true and Some name (with correct name length) should succeed" in {
      val model = AgentDetailsModel(true, Some("AgentYang"))
      model.validate.toEither.right.get shouldBe model
    }

    "passed true and Some name (with incorrect name length) should not succeed" in {
      val model = AgentDetailsModel(true, Some(""))
      model.validate.toEither.left.get.head.errorMessages shouldBe AgentNameLengthError.errorMessages
    }

    "passed false and Some name (with correct name length) should not succeed" in {
      val model = AgentDetailsModel(false, Some("Yangksy"))
      model.validate.toEither.left.get.head.errorMessages shouldBe AgentNameSuppliedError.errorMessages

    }

    "passed false and Some name (with incorrect name length) should not succeed with 2 errors" in {
      val model = AgentDetailsModel(false , Some(""))
      model.validate.toEither.left.get.toChain.get(0).get.errorMessages shouldBe AgentNameLengthError.errorMessages
      model.validate.toEither.left.get.toChain.get(1).get.errorMessages shouldBe AgentNameSuppliedError.errorMessages
    }

    "passed true and None should not succeed" in {
      val model = AgentDetailsModel(true, None)
      model.validate.toEither.left.get.head.errorMessages shouldBe AgentNameNotSuppliedError.errorMessages
    }

    "passed true and Some name (with incorrect name length > maxLength) should not succeed" in {
      val model = AgentDetailsModel(true, Some("a" * 180))
      model.validate.toEither.left.get.head.errorMessages shouldBe AgentNameLengthError.errorMessages
    }

  }
}
