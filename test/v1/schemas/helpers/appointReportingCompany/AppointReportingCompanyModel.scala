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

package v1.schemas.helpers.appointReportingCompany

import play.api.libs.json.Json
import v1.schemas.helpers._

case class AppointReportingCompanyModel(agentDetails: Option[AgentDetails] = Some(AgentDetails()),
                                        reportingCompany: Option[ReportingCompany] = Some(ReportingCompany()),
                                        authorisingCompanies: Option[Seq[AuthorisingCompanies]] = Some(Seq(AuthorisingCompanies())),
                                        isReportingCompanyAppointingItself: Option[Boolean] = Some(true),
                                        identityOfAppointingCompany: Option[IdentityOfCompanySubmitting] = Some(IdentityOfCompanySubmitting()),
                                        ultimateParentCompany: Option[UltimateParent] = Some(UltimateParent(sautr = None, countryOfIncorporation = None)),
                                        accountingPeriod: Option[AccountingPeriod] = Some(AccountingPeriod()),
                                        declaration: Option[Boolean] = Some(true)
                                       )

object AppointReportingCompanyModel {
  implicit val writes = Json.writes[AppointReportingCompanyModel]
}