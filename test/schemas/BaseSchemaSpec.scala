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

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.{Json, Writes}
import utils.SchemaValidation

trait BaseSchemaSpec extends WordSpec with Matchers with GuiceOneAppPerSuite with SchemaValidation {

  val maxAgentNameLength = 160
  val maxCompanyNameLength = 160
  val utrLength = 10
  val crnLength = 8
  val electString = "elect"
  val revokeString = "revoke"

  case class AgentDetails(agentActingOnBehalfOfCompany: Option[Boolean] = Some(true),
                          agentName: Option[String] = Some("Agent Name"))

  object AgentDetails {
    implicit val writes = Json.writes[AgentDetails]
  }

  sealed trait UltimateParent

  case class UkCompany(registeredCompanyName: Option[String] = Some("cde ltd"),
                       knownAs: Option[String] = Some("efg"),
                       utr: Option[String] = Some("1234567890"),
                       crn: Option[String] = Some("AB123456"),
                       otherUkTaxReference: Option[String] = Some("1234567890")
                      ) extends UltimateParent

  object UkCompany {
    implicit val writes = Json.writes[UkCompany]
  }

  case class NonUkCompany(registeredCompanyName: Option[String] = Some("cde ltd"),
                          knownAs: Option[String] = Some("efg"),
                          countryOfIncorporation: Option[String] = Some("US"),
                          crn: Option[String] = Some("AB123456")
                         ) extends UltimateParent

  object NonUkCompany {
    implicit val writes = Json.writes[NonUkCompany]
  }

  object UltimateParent {
    implicit def writes: Writes[UltimateParent] = Writes {
      case x: UkCompany => Json.toJson(x)(UkCompany.writes)
      case x: NonUkCompany => Json.toJson(x)(NonUkCompany.writes)
    }
  }

  case class DeemedParent(companyName: Option[String] = Some("name"), utr: Option[String] = Some("1111111111"))

  object DeemedParent {
    implicit val writes = Json.writes[DeemedParent]
  }

  case class ParentCompany(ultimateParent: Option[UltimateParent] = Some(UkCompany()), deemedParent: Option[Seq[DeemedParent]] = None)

  object ParentCompany {
    implicit val writes = Json.writes[ParentCompany]
  }

  case class UKCompanies(companyName: Option[String] = Some("name"), utr: Option[String] = Some("1111111111"))

  object UKCompanies {
    implicit val writes = Json.writes[UKCompanies]
  }

  case class AccountingPeriod(startDate: Option[String] = Some("1111-11-11"),
                              endDate: Option[String] = Some("1111-11-11"))

  object AccountingPeriod {
    implicit val writes = Json.writes[AccountingPeriod]
  }

  case class GroupCompanyDetails(totalCompanies: Option[Int] = Some(1),
                                 inclusionOfNonConsentingCompanies: Option[Boolean] = Some(true),
                                 accountingPeriod: Option[AccountingPeriod] = Some(AccountingPeriod()))

  object GroupCompanyDetails {
    implicit val writes = Json.writes[GroupCompanyDetails]
  }

  case class InvestorGroup(groupName: Option[String] = Some("Group"))

  object InvestorGroup {
    implicit val writes = Json.writes[InvestorGroup]
  }

  case class GroupRatioBlended(election: Option[String] = Some(electString),
                               investorGroups: Option[Seq[InvestorGroup]] = Some(Seq(InvestorGroup())))

  object GroupRatioBlended {
    implicit val writes = Json.writes[GroupRatioBlended]
  }

  case class Investment(investmentName: Option[String] = Some("Name"))

  object Investment {
    implicit val writes = Json.writes[Investment]
  }

  case class InterestAllowanceNonConsolidatedInvestment(election: Option[String] = Some(electString),
                                                        nonConsolidatedInvestments: Option[Seq[Investment]] = Some(Seq(Investment())))

  object InterestAllowanceNonConsolidatedInvestment {
    implicit val writes = Json.writes[InterestAllowanceNonConsolidatedInvestment]
  }

  case class Partnership(partnershipName: Option[String] = Some("Name"))

  object Partnership {
    implicit val writes = Json.writes[Partnership]
  }

  case class InterestAllowanceConsolidatedPartnership(election: Option[Boolean] = Some(true),
                                                      consolidatedPartnerships: Option[Seq[Partnership]] = Some(Seq(Partnership())))

  object InterestAllowanceConsolidatedPartnership {
    implicit val writes = Json.writes[InterestAllowanceConsolidatedPartnership]
  }

  case class GroupLevelElections(groupRatioElection: Option[String] = Some(electString),
                                 groupRatioBlended: Option[GroupRatioBlended] = Some(GroupRatioBlended()),
                                 groupEBITDAChargeableGains: Option[Boolean] = Some(true),
                                 interestAllowanceAlternativeCalculation: Option[Boolean] = Some(true),
                                 interestAllowanceNonConsolidatedInvestment: Option[InterestAllowanceNonConsolidatedInvestment] = Some(InterestAllowanceNonConsolidatedInvestment()),
                                 interestAllowanceConsolidatedPartnership: Option[InterestAllowanceConsolidatedPartnership] = Some(InterestAllowanceConsolidatedPartnership())
                                )

  object GroupLevelElections {
    implicit val writes = Json.writes[GroupLevelElections]
  }

  case class AbbreviatedReturnModel(agentDetails: Option[AgentDetails] = Some(AgentDetails()),
                                    isReportingCompanyUltimateParent: Option[Boolean] = Some(true),
                                    parentCompany: Option[ParentCompany] = Some(ParentCompany()),
                                    groupCompanyDetails: Option[GroupCompanyDetails] = Some(GroupCompanyDetails()),
                                    submissionType: Option[String] = Some("original"),
                                    revisedReturnDetails: Option[String] = Some("asdfghj"),
                                    groupLevelElections: Option[GroupLevelElections] = Some(GroupLevelElections()),
                                    ukCompanies: Option[Seq[UKCompanies]] = Some(Seq(UKCompanies())))

  object AbbreviatedReturnModel {
    implicit val writes = Json.writes[AbbreviatedReturnModel]
  }

  case class ReportingCompany(companyName: Option[String] = Some("MIB Ltd"),
                              utr: Option[String] = Some("1234567890"),
                              crn: Option[String] = Some("12345678"),
                              sameAsUltimateParent: Option[Boolean] = Some(false),
                              reportingCompanyDeemed: Option[Boolean] = Some(true))

  object ReportingCompany {
    implicit val writes = Json.writes[ReportingCompany]
  }

  case class AuthorisingCompanyModel(companyName: Option[String] = Some("cde ltd"),
                                     utr: Option[String] = Some("1234567890"))

  object AuthorisingCompanyModel {
    implicit val writes = Json.writes[AuthorisingCompanyModel]
  }

  case class AppointReportingCompanyModel(agentDetails: Option[AgentDetails] = Some(AgentDetails()),
                                          reportingCompany: Option[ReportingCompany] = Some(ReportingCompany()),
                                          authorisingCompanies: Option[Seq[AuthorisingCompanyModel]] = Some(Seq(AuthorisingCompanyModel())),
                                          declaration: Boolean = true)

  object AppointReportingCompanyModel {
    implicit val writes = Json.writes[AppointReportingCompanyModel]
  }
}
