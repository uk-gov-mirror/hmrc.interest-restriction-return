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

package v1.validation.fullReturn

import assets.fullReturn.AllocatedRestrictionsConstants._
import play.api.libs.json.JsPath
import utils.BaseSpec
import v1.models.AccountingPeriodModel
import v1.models.fullReturn.AllocatedRestrictionsModel

class AllocatedRestrictionsValidatorSpec extends BaseSpec {

  implicit val path = JsPath \ "some" \ "path"

  val groupAccountingPeriod = AccountingPeriodModel(
    startDate = ap1EndDate.minusDays(1),
    endDate = ap3EndDate
  )

  val restrictionModel = AllocatedRestrictionsModel(
    ap1EndDate = None,
    disallowanceAp1 = None,
    ap2EndDate = None,
    disallowanceAp2 = None,
    ap3EndDate = None,
    disallowanceAp3 = None,
    totalDisallowances = None
  )

  "AllocatedRestrictionsValidator" should {

    "Return valid" when {

      "Ap1 supplied with the total disallowed amount" in {

        val model = restrictionModel.copy(
          ap1EndDate = Some(ap1EndDate),
          disallowanceAp1 = Some(disallowanceAp1),
          totalDisallowances = Some(disallowanceAp1)
        )

        rightSide(model.validate(groupAccountingPeriod)) shouldBe model
      }

      "Ap1 and Ap2 supplied with the total disallowed amount" in {

        val model = restrictionModel.copy(
          ap1EndDate = Some(ap1EndDate),
          disallowanceAp1 = Some(disallowanceAp1),
          ap2EndDate = Some(ap2EndDate),
          disallowanceAp2 = Some(disallowanceAp2),
          totalDisallowances = Some(disallowanceAp1 + disallowanceAp2)
        )

        rightSide(model.validate(groupAccountingPeriod)) shouldBe model
      }

      "Ap1, Ap2 and Ap3 supplied with the total disallowed amount" in {

        val model = restrictionModel.copy(
          ap1EndDate = Some(ap1EndDate),
          disallowanceAp1 = Some(disallowanceAp1),
          ap2EndDate = Some(ap2EndDate),
          disallowanceAp2 = Some(disallowanceAp2),
          ap3EndDate = Some(ap3EndDate),
          disallowanceAp3 = Some(disallowanceAp3),
          totalDisallowances = Some(totalDisallowances)
        )

        rightSide(model.validate(groupAccountingPeriod)) shouldBe model
      }

    }

    "Return invalid" when {

      "Ap1" when {

        "is supplied with no amount" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNotSupplied(1).errorMessage
        }

        "is supplied with no date" in {

          val model = restrictionModel.copy(
            disallowanceAp1 = Some(disallowanceAp1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionSupplied(1).errorMessage
        }

        "is supplied with negative amount" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(-1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNegative(1, -1).errorMessage
        }

        "is supplied with a date that is equal to Group Accounting Period start date" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(groupAccountingPeriod.startDate),
            disallowanceAp1 = Some(disallowanceAp1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe
            Ap1NotAfterGroupStartDate(groupAccountingPeriod.startDate, groupAccountingPeriod.startDate).errorMessage
        }

        "is supplied with a date that is less than Group Accounting Period start date" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(groupAccountingPeriod.startDate.minusDays(1)),
            disallowanceAp1 = Some(disallowanceAp1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe
            Ap1NotAfterGroupStartDate(groupAccountingPeriod.startDate.minusDays(1), groupAccountingPeriod.startDate).errorMessage
        }
      }

      "Ap2" when {

        "is supplied with no amount" in {

          val model = restrictionModel.copy(
            ap2EndDate = Some(ap2EndDate),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNotSupplied(2).errorMessage
        }

        "is supplied with no date" in {

          val model = restrictionModel.copy(
            disallowanceAp2 = Some(disallowanceAp2),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionSupplied(2).errorMessage
        }

        "is supplied with negative amount" in {

          val model = restrictionModel.copy(
            ap2EndDate = Some(ap2EndDate),
            disallowanceAp2 = Some(-1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNegative(2, -1).errorMessage
        }

        "is supplied without Ap1" in {

          val model = restrictionModel.copy(
            ap2EndDate = Some(ap2EndDate),
            disallowanceAp2 = Some(disallowanceAp2),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionLaterPeriodSupplied(2).errorMessage
        }

        "is supplied with a date equal to Ap1" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap1EndDate),
            disallowanceAp2 = Some(disallowanceAp2),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionDateBeforePrevious(2).errorMessage
        }

        "is supplied with a date less than Ap1" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap1EndDate.minusDays(1)),
            disallowanceAp2 = Some(disallowanceAp2),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionDateBeforePrevious(2).errorMessage
        }
      }

      "Ap3" when {

        "is supplied with no amount" in {

          val model = restrictionModel.copy(
            ap3EndDate = Some(ap3EndDate),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNotSupplied(3).errorMessage
        }

        "is supplied with no date" in {

          val model = restrictionModel.copy(
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionSupplied(3).errorMessage
        }

        "is supplied with negative amount" in {

          val model = restrictionModel.copy(
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(-1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionNegative(3, -1).errorMessage
        }

        "is supplied without Ap1 and Ap2" in {

          val model = restrictionModel.copy(
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionLaterPeriodSupplied(3).errorMessage
        }

        "is supplied without Ap2" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionLaterPeriodSupplied(3).errorMessage
        }

        "is supplied with a date equal to Ap2" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap3EndDate),
            disallowanceAp2 = Some(disallowanceAp2),
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionDateBeforePrevious(3).errorMessage
        }

        "is supplied with a date less than Ap2" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap3EndDate.plusDays(1)),
            disallowanceAp2 = Some(disallowanceAp2),
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionDateBeforePrevious(3).errorMessage
        }

        "is supplied with a date less than Group Accounting Period end" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap2EndDate),
            disallowanceAp2 = Some(disallowanceAp2),
            ap3EndDate = Some(groupAccountingPeriod.endDate.minusDays(1)),
            disallowanceAp3 = Some(disallowanceAp3),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe
            Ap3BeforeGroupEndDate(groupAccountingPeriod.endDate.minusDays(1), groupAccountingPeriod.endDate).errorMessage
        }
      }

      "totalDisallowances" when {

        "is not supplied when Ap1 is" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionTotalNotSupplied().errorMessage
        }

        "is not supplied when Ap1 & Ap2 is" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap2EndDate),
            disallowanceAp2 = Some(disallowanceAp2)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionTotalNotSupplied().errorMessage
        }

        "is not supplied when Ap1, Ap2 & Ap3 is" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            ap2EndDate = Some(ap2EndDate),
            disallowanceAp2 = Some(disallowanceAp2),
            ap3EndDate = Some(ap3EndDate),
            disallowanceAp3 = Some(disallowanceAp3)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionTotalNotSupplied().errorMessage
        }

        "is negative" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            totalDisallowances = Some(-1)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionTotalNegative(-1).errorMessage
        }

        "does not match the calculated total" in {

          val model = restrictionModel.copy(
            ap1EndDate = Some(ap1EndDate),
            disallowanceAp1 = Some(disallowanceAp1),
            totalDisallowances = Some(totalDisallowances)
          )

          leftSideError(model.validate(groupAccountingPeriod)).errorMessage shouldBe AllocatedRestrictionTotalDoesNotMatch(totalDisallowances, disallowanceAp1).errorMessage
        }
      }
    }
  }
}
