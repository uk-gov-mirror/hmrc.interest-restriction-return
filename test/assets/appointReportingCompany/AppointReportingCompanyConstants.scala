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

package assets.appointReportingCompany

import assets.AgentDetailsConstants._
import assets.ReportingCompanyConstants._
import assets.AuthorisingCompanyConstants._
import assets.UltimateParentConstants._
import assets.IdentityOfAppointingCompanyConstants._
import play.api.libs.json.Json
import assets.AccountingPeriodConstants._
import models.IdentityOfAppointingCompanyModel
import models.appointReportingCompany.AppointReportingCompanyModel

object AppointReportingCompanyConstants {

  val ackRef = "ackRef"

  val appointReportingCompanyJson = Json.obj(
    "agentDetails" -> agentDetailsJsonMax,
    "reportingCompany" -> reportingCompanyJsonMax,
    "authorisingCompanies" -> Json.arr(
      authorisingCompanyJson
    ),
    "identityOfAppointingCompany" -> identityOfAppointingCompanyJsonMax,
    "ultimateParentCompany" -> optionalUkParentJsonMax,
    "accountingPeriod" -> accountingPeriodJson,
    "declaration" -> true
  )

  val appointReportingCompanyModel = AppointReportingCompanyModel(
    agentDetails = agentDetailsModelMax,
    reportingCompany = reportingCompanyModelMax,
    authorisingCompanies = Seq(authorisingCompanyModel),
    identityOfAppointingCompany = identityOfAppointingCompanyModelMax,
    ultimateParentCompany = optionalUkParent,
    accountingPeriod = accountingPeriodModel,
    declaration = true
  )
}
