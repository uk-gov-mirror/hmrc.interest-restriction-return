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

package assets

import models.abbreviatedReturn.GroupRatioBlendedModel
import play.api.libs.json.Json

object GroupRatioBlendedConstants {

  val investorGroups = Seq("investment1", "investment2", "investment3")

  val groupRatioBlendedModel = GroupRatioBlendedModel(
    election = "elect",
    investorGroups = investorGroups
  )

  val groupRatioBlendedJson = Json.obj(
    "election" -> "elect",
    "investorGroups" -> investorGroups
  )
}
