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

package schemas.helpers.fullReturn

import play.api.libs.json.Json

case class UkCompanyFull(companyName: Option[String] = Some("ABC Ltd"),
                         utr: Option[String] = Some("1234567890"),
                         consenting: Option[Boolean] = Some(true),
                         netTaxInterestExpense: Option[BigDecimal] = Some(56000),
                         netTaxInterestIncome: Option[BigDecimal] = Some(0),
                         taxEBITDA: Option[BigDecimal] = Some(26500),
                         allocatedRestrictions: Option[AllocatedRestrictions] = Some(AllocatedRestrictions()),
                         allocatedReactivations: Option[AllocatedReactivations] = Some(AllocatedReactivations()))

object UkCompanyFull {
  implicit val format = Json.format[UkCompanyFull]
}