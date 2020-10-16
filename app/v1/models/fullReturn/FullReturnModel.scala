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

package v1.models.fullReturn

import play.api.libs.json._
import v1.models._
import v1.validation.fullReturn.FullReturnValidator

case class FullReturnModel(appointedReportingCompany: Boolean,
                           agentDetails: AgentDetailsModel,
                           reportingCompany: ReportingCompanyModel,
                           parentCompany: Option[ParentCompanyModel],
                           publicInfrastructure: Boolean,
                           groupCompanyDetails: GroupCompanyDetailsModel,
                           submissionType: SubmissionType,
                           revisedReturnDetails: Option[String],
                           groupLevelElections: GroupLevelElectionsModel,
                           ukCompanies: Seq[UkCompanyModel],
                           angie: Option[BigDecimal],
                           returnContainsEstimates: Boolean,
                           groupEstimateReason: Option[String],
                           companiesEstimateReason: Option[String],
                           groupSubjectToInterestRestrictions: Boolean,
                           groupSubjectToInterestReactivation: Boolean,
                           totalReactivation: BigDecimal,
                           totalRestrictions: BigDecimal,
                           groupLevelAmount: GroupLevelAmountModel,
                           adjustedGroupInterest: Option[AdjustedGroupInterestModel]) extends FullReturnValidator {

  override val fullReturnModel: FullReturnModel = this

  private val totalTaxInterestIncome: BigDecimal = ukCompanies.map(_.netTaxInterestIncome).sum
  private val totalTaxInterestExpense: BigDecimal = ukCompanies.map(_.netTaxInterestExpense).sum

  private val oSum: Seq[BigDecimal] => Option[BigDecimal] = {
    case x if x.isEmpty => None
    case x => Some(x.sum)
  }

  val numberOfUkCompanies: Int = ukCompanies.length
  val aggregateNetTaxInterest: BigDecimal = totalTaxInterestIncome - totalTaxInterestExpense
  val aggregateTaxEBITDA: BigDecimal = ukCompanies.map(_.taxEBITDA).sum
  val aggregateAllocatedRestrictions: Option[BigDecimal] = oSum(ukCompanies.flatMap(_.allocatedRestrictions.flatMap(_.totalDisallowances)))
  val aggregateAllocatedReactivations: Option[BigDecimal] = oSum(ukCompanies.flatMap(_.allocatedReactivations.map(_.currentPeriodReactivation)))
}

object FullReturnModel {

  val writes: Writes[FullReturnModel] = Writes { models =>

    JsObject(Json.obj(
      "appointedReportingCompany" -> models.appointedReportingCompany,
      "agentDetails" -> models.agentDetails,
      "reportingCompany" -> models.reportingCompany,
      "parentCompany" -> models.parentCompany,
      "publicInfrastructure" -> models.publicInfrastructure,
      "groupCompanyDetails" -> models.groupCompanyDetails,
      "submissionType" -> models.submissionType,
      "revisedReturnDetails" -> models.revisedReturnDetails,
      "groupLevelElections" -> models.groupLevelElections,
      "ukCompanies" -> models.ukCompanies,
      "numberOfUkCompanies" -> models.numberOfUkCompanies,
      (if(models.aggregateNetTaxInterest >= 0) "aggregateNetTaxInterestIncome" else "aggregateNetTaxInterestExpense") -> models.aggregateNetTaxInterest.abs,
      "aggregateTaxEBITDA" -> models.aggregateTaxEBITDA,
      "aggregateAllocatedRestrictions" -> models.aggregateAllocatedRestrictions,
      "aggregateAllocatedReactivations" -> models.aggregateAllocatedReactivations,
      "angie" -> models.angie,
      "returnContainsEstimates" -> models.returnContainsEstimates,
      "groupEstimateReason" -> models.groupEstimateReason,
      "companiesEstimateReason" -> models.companiesEstimateReason,
      "groupSubjectToInterestRestrictions" -> models.groupSubjectToInterestRestrictions,
      "groupSubjectToInterestReactivation" -> models.groupSubjectToInterestReactivation,
      "totalReactivation" -> models.totalReactivation,
      "totalRestrictions" -> models.totalRestrictions,
      "groupLevelAmount" -> models.groupLevelAmount,
      "adjustedGroupInterest" -> models.adjustedGroupInterest
    ).fields.filterNot(_._2 == JsNull))
  }

  implicit val format: Format[FullReturnModel] = Format[FullReturnModel](Json.reads[FullReturnModel], writes)
}