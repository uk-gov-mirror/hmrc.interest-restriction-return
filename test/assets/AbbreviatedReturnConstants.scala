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

import models.AgentDetailsModel
import models.abbreviatedReturn.AbbreviatedReturnModel
import play.api.libs.json.Json
import assets.AgentDetailsConstants._
import assets.ParentCompanyConstants._
import assets.GroupCompanyDetailsConstants._
import assets.GroupLevelElectionsConstants._
import assets.UkCompanyConstants._

object AbbreviatedReturnConstants {

  val abbreviatedReturnJson = Json.obj(
    "agentDetails" -> agentDetailsJson,
    "isReportingCompanyUltimateParent" -> true,
    "parentCompany" -> parentCompanyJson,
    "groupCompanyDetails" -> groupCompanyDetailsJson,
    "submissionType" -> "submission type",
    "revisedReturnDetails" -> "revised details",
    "groupLevelElections" -> groupLevelElectionsJson,
    "ukCompanies" -> Seq(ukCompanyJson)
  )

  val abbreviatedReturnModel = AbbreviatedReturnModel(
    agentDetails = agentDetailsModel,
    isReportingCompanyUltimateParent = true,
    parentCompany = parentCompanyModel,
    groupCompanyDetails = groupCompanyDetailsModel,
    submissionType = "submission type",
    revisedReturnDetails = Some("revised details"),
    groupLevelElections = Some(groupLevelElectionsModel),
    ukCompanies = Seq(ukCompanyModel)
  )

}
