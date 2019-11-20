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

package schemas

import play.api.libs.json.Json

trait AbbreviatedBaseSchemaSpec extends BaseSchemaSpec {

  case class UKCompanies(companyName: Option[String] = Some("name"), utr: Option[String] = Some("1111111111"))

  object UKCompanies {
    implicit val writes = Json.writes[UKCompanies]
  }

  case class AccountingPeriod(startDate: Option[String] = Some("1111-11-11"),
                              endDate: Option[String] = Some("1111-11-11"))

  object AccountingPeriod {
    implicit val writes = Json.writes[AccountingPeriod]
  }

  case class AbbreviatedReturnModel(agentDetails: Option[AgentDetails] = Some(AgentDetails()),
                                    reportingCompany: Option[ReportingCompany] = Some(ReportingCompany()),
                                    parentCompany: Option[ParentCompany] = Some(ParentCompany()),
                                    publicInfrastructure: Option[Boolean] = Some(false),
                                    groupCompanyDetails: Option[GroupCompanyDetails] = Some(GroupCompanyDetails()),
                                    submissionType: Option[String] = Some("original"),
                                    revisedReturnDetails: Option[String] = Some("asdfghj"),
                                    groupLevelElections: Option[GroupLevelElections] = Some(GroupLevelElections()),
                                    ukCompanies: Option[Seq[UKCompanies]] = Some(Seq(UKCompanies())))

  object AbbreviatedReturnModel {
    implicit val writes = Json.writes[AbbreviatedReturnModel]
  }
}
