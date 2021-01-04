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

package assets.revokeReportingCompany

import assets.AccountingPeriodConstants._
import assets.AgentDetailsConstants._
import assets.AuthorisingCompanyConstants._
import assets.ReportingCompanyConstants._
import assets.UltimateParentConstants._
import play.api.libs.json.Json
import v1.models.revokeReportingCompany.RevokeReportingCompanyModel

object RevokeReportingCompanyConstants {

  val ackRef = "ackRef"

  val revokeReportingCompanyModelMax = RevokeReportingCompanyModel(
    agentDetails = agentDetailsModelMax,
    reportingCompany = reportingCompanyModel,
    isReportingCompanyRevokingItself = true,
    companyMakingRevocation = None,
    ultimateParentCompany = Some(ultimateParentModelUkCompany),
    accountingPeriod = accountingPeriodModel,
    authorisingCompanies = Seq(authorisingCompanyModel),
    declaration = true
  )

  val revokeReportingCompanyJsonMax = Json.obj(
    "agentDetails" -> agentDetailsJsonMax,
    "reportingCompany" -> reportingCompanyJson,
    "isReportingCompanyRevokingItself" -> true,
    "ultimateParentCompany" -> ultimateParentJsonUkCompany,
    "accountingPeriod" -> accountingPeriodJson,
    "authorisingCompanies" -> Seq(authorisingCompanyJson),
    "declaration" -> true
  )

  val revokeReportingCompanyModelMin = RevokeReportingCompanyModel(
    agentDetails = agentDetailsModelMax,
    reportingCompany = reportingCompanyModel,
    isReportingCompanyRevokingItself = true,
    companyMakingRevocation = None,
    ultimateParentCompany = None,
    accountingPeriod = accountingPeriodModel,
    authorisingCompanies = Seq(authorisingCompanyModel),
    declaration = true
  )

  val revokeReportingCompanyJsonMin = Json.obj(
    "agentDetails" -> agentDetailsJsonMax,
    "reportingCompany" -> reportingCompanyJson,
    "isReportingCompanyRevokingItself" -> true,
    "accountingPeriod" -> accountingPeriodJson,
    "authorisingCompanies" -> Seq(authorisingCompanyJson),
    "declaration" -> true
  )
}
