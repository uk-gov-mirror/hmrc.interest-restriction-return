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

package assets

import assets.AgentDetailsITConstants._
import assets.GroupCompanyDetailsITConstants.groupCompanyDetailsJson
import assets.GroupLevelElectionsITConstants.groupLevelElectionsJson
import assets.ReportingCompanyITConstants._
import assets.UkCompanyITConstants.ukCompanyJson
import play.api.libs.json.Json
import v1.models.Revised

object AbbreviatedReturnITConstants {

  val ackRef = "ackRef"

  val abbreviatedReturnDesSuccessJson = Json.obj(
    "acknowledgementReference" -> ackRef
  )

  val abbreviatedReturnJson = Json.obj(
    "appointedReportingCompany" -> true,
    "agentDetails" -> agentDetailsJson,
    "reportingCompany" -> reportingCompanyJson,
    "publicInfrastructure" -> true,
    "groupCompanyDetails" -> groupCompanyDetailsJson,
    "submissionType" -> Revised,
    "angie" -> 1.1,
    "revisedReturnDetails" -> "revised details",
    "groupLevelElections" -> groupLevelElectionsJson,
    "ukCompanies" -> Seq(ukCompanyJson)
  )
}
