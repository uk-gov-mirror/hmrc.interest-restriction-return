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

package v1.schemas.helpers

import play.api.libs.json.Json

case class GroupLevelElections(groupRatio: Option[GroupRatio] = Some(GroupRatio()),
                               interestAllowanceAlternativeCalculation: Option[Boolean] = Some(true),
                               interestAllowanceNonConsolidatedInvestment: Option[InterestAllowanceNonConsolidatedInvestment] = Some(InterestAllowanceNonConsolidatedInvestment()),
                               interestAllowanceConsolidatedPartnership: Option[InterestAllowanceConsolidatedPartnership] = Some(InterestAllowanceConsolidatedPartnership())
                              )

object GroupLevelElections {
  implicit val writes = Json.writes[GroupLevelElections]
}