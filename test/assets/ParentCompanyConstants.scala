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

import assets.UltimateParentConstants._
import assets.DeemedParentConstants._
import models.{ParentCompanyModel}
import play.api.libs.json.Json

object ParentCompanyConstants {

  val parentCompanyJsonMax = Json.obj(
    "ultimateParent" -> ultimateParentJsonMax,
    "deemedParent" -> Seq(deemedParentJsonMax)
  )

  val parentCompanyModelMax = ParentCompanyModel(
    ultimateParent = Some(ultimateParentModelMax),
    deemedParent = Some(Seq(deemedParentModelMax))
  )

  val parentCompanyJsonMin = Json.obj(
  )

  val parentCompanyModelMin = ParentCompanyModel(
    ultimateParent = None,
    deemedParent = None
  )

  val parentCompanyModelUlt = ParentCompanyModel(
    ultimateParent = Some(ultimateParentModelMax),
    deemedParent = None
  )

  val parentCompanyModelDee = ParentCompanyModel(
    ultimateParent = None,
    deemedParent = Some(Seq(deemedParentModelMax)
  )
  )
}


