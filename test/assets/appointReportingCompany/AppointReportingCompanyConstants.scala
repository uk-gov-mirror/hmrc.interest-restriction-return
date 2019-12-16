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
import assets.IdentityOfCompanySubmittingConstants._
import play.api.libs.json.Json
import assets.AccountingPeriodConstants._
import models.appointReportingCompany.AppointReportingCompanyModel

object AppointReportingCompanyConstants {

  val ackRef = "ackRef"

  val appointReportingCompanyJsonMax = Json.obj(
    "agentDetails" -> agentDetailsJsonMax,
    "reportingCompany" -> reportingCompanyJsonMax,
    "authorisingCompanies" -> Json.arr(
      authorisingCompanyJson
    ),
    "isReportingCompanyAppointingItself" -> true,
    "identityOfAppointingCompany" -> identityOfCompanySubmittingJsonMax,
    "ultimateParentCompany" -> ultimateParentJsonMax,
    "accountingPeriod" -> accountingPeriodJson,
    "declaration" -> true
  )

  val appointReportingCompanyModelMax = AppointReportingCompanyModel(
    agentDetails = agentDetailsModelMax,
    reportingCompany = reportingCompanyModelMax,
    authorisingCompanies = Seq(authorisingCompanyModel),
    isReportingCompanyAppointingItself = true,
    identityOfAppointingCompany = Some(identityOfCompanySubmittingModelMax),
    ultimateParentCompany = Some(ultimateParentModelMax),
    accountingPeriod = accountingPeriodModel,
    declaration = true
  )
  val appointReportingCompanyJsonMin = Json.obj(
    "agentDetails" -> agentDetailsJsonMax,
    "reportingCompany" -> reportingCompanyJsonMax,
    "authorisingCompanies" -> Json.arr(
      authorisingCompanyJson
    ),
    "isReportingCompanyAppointingItself" -> true,
    "accountingPeriod" -> accountingPeriodJson,
    "declaration" -> true
  )

  val appointReportingCompanyModelMin = AppointReportingCompanyModel(
    agentDetails = agentDetailsModelMax,
    reportingCompany = reportingCompanyModelMax,
    authorisingCompanies = Seq(authorisingCompanyModel),
    isReportingCompanyAppointingItself = true,
    identityOfAppointingCompany = None,
    ultimateParentCompany = None,
    accountingPeriod = accountingPeriodModel,
    declaration = true
  )

}
