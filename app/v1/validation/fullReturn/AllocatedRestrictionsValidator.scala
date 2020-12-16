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

import java.time.LocalDate

import play.api.libs.json.{JsPath, Json}
import v1.models.Validation.ValidationResult
import v1.models.fullReturn.AllocatedRestrictionsModel
import v1.models.{AccountingPeriodModel, Validation}
import v1.validation.BaseValidation

trait AllocatedRestrictionsValidator extends BaseValidation {

  import cats.implicits._

  val allocatedRestrictionsModel: AllocatedRestrictionsModel

  private def apRestrictionValidator(endDate: Option[LocalDate],
                                     amount: Option[BigDecimal],
                                     i: Int)(implicit path: JsPath): ValidationResult[(Option[LocalDate], Option[BigDecimal])] = {
    val period = (endDate, amount)
    period match {
      case (Some(_), None) => AllocatedRestrictionNotSupplied(i).invalidNec
      case (None, Some(_)) => AllocatedRestrictionSupplied(i).invalidNec
      case (Some(_), Some(amt)) if amt < 0 => AllocatedRestrictionNegative(i, amt).invalidNec
      case (Some(_), Some(amt)) if amt % 0.01 != 0 => AllocatedRestrictionDecimalError(i, amt).invalidNec
      case _ => period.validNec
    }
  }

  def validateAp1(groupStartDate: LocalDate)(implicit topPath: JsPath): ValidationResult[Option[LocalDate]] =
    allocatedRestrictionsModel.ap1EndDate.fold[ValidationResult[Option[LocalDate]]](None.validNec)(ap1 =>
      if(ap1.isAfter(groupStartDate)) Some(ap1).validNec else Ap1NotAfterGroupStartDate(ap1, groupStartDate).invalidNec
    )

  def validateAp2(implicit topPath: JsPath): ValidationResult[Option[LocalDate]] =
    (allocatedRestrictionsModel.ap1EndDate, allocatedRestrictionsModel.ap2EndDate) match {
      case (optAp1, Some(ap2)) =>
        optAp1.fold[ValidationResult[Option[LocalDate]]](AllocatedRestrictionLaterPeriodSupplied(2).invalidNec)(ap1 =>
          if (!ap2.isAfter(ap1)) AllocatedRestrictionDateBeforePrevious(2).invalidNec else Some(ap2).validNec
        )
      case _ => None.validNec
    }

  def validateAp3(groupEndDate: LocalDate)(implicit topPath: JsPath): ValidationResult[Option[LocalDate]] =
    (allocatedRestrictionsModel.ap1EndDate, allocatedRestrictionsModel.ap2EndDate, allocatedRestrictionsModel.ap3EndDate) match {
      case (None, _, Some(_)) | (_, None, Some(_)) => AllocatedRestrictionLaterPeriodSupplied(3).invalidNec
      case (_, Some(ap2), Some(ap3)) => combineValidationsForField(
        if (!ap3.isAfter(ap2)) AllocatedRestrictionDateBeforePrevious(3).invalidNec else Some(ap3).validNec,
        if (ap3.isBefore(groupEndDate)) Ap3BeforeGroupEndDate(ap3, groupEndDate).invalidNec else Some(ap3).validNec
      )
      case _ => None.validNec
    }

  def validateTotalRestriction(implicit path: JsPath): ValidationResult[BigDecimal] = {

    val hasApRestrictions =
      allocatedRestrictionsModel.disallowanceAp1.isDefined ||
        allocatedRestrictionsModel.disallowanceAp2.isDefined ||
        allocatedRestrictionsModel.disallowanceAp3.isDefined

    val totalDisallowancesCalculated: BigDecimal = allocatedRestrictionsModel.disallowanceAp1.getOrElse[BigDecimal](0) +
      allocatedRestrictionsModel.disallowanceAp2.getOrElse[BigDecimal](0) +
      allocatedRestrictionsModel.disallowanceAp3.getOrElse[BigDecimal](0)

    if (hasApRestrictions && allocatedRestrictionsModel.totalDisallowances.isEmpty) {
      AllocatedRestrictionTotalNotSupplied().invalidNec
    } else {
      val totalDisallowances: BigDecimal = allocatedRestrictionsModel.totalDisallowances.getOrElse(0)
      if (totalDisallowances < 0) {
        AllocatedRestrictionTotalNegative(totalDisallowances).invalidNec
      } else if(totalDisallowances % 0.01 != 0) {
        AllocatedRestrictionTotalDecimalError(totalDisallowances).invalidNec
      } else if(totalDisallowances != totalDisallowancesCalculated) {
        AllocatedRestrictionTotalDoesNotMatch(totalDisallowances, totalDisallowancesCalculated).invalidNec
      } else {
        totalDisallowances.validNec
      }
    }
  }

  def validate(groupAccountingPeriod: AccountingPeriodModel)(implicit path: JsPath): ValidationResult[AllocatedRestrictionsModel] =
    (apRestrictionValidator(allocatedRestrictionsModel.ap1EndDate, allocatedRestrictionsModel.disallowanceAp1, 1),
      apRestrictionValidator(allocatedRestrictionsModel.ap2EndDate, allocatedRestrictionsModel.disallowanceAp2, 2),
      apRestrictionValidator(allocatedRestrictionsModel.ap3EndDate, allocatedRestrictionsModel.disallowanceAp3, 3),
      validateAp1(groupAccountingPeriod.startDate),
      validateAp2,
      validateAp3(groupAccountingPeriod.endDate),
      validateTotalRestriction).mapN((_,_,_,_,_,_,_) => allocatedRestrictionsModel)
}

case class AllocatedRestrictionNotSupplied(i: Int)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"disallowanceAp$i"
  val errorMessage: String = s"disallowanceAp$i must have a value when ap${i}End is supplied"
  val value = Json.obj()
}

case class AllocatedRestrictionSupplied(i: Int)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"disallowanceAp$i"
  val errorMessage: String = s"disallowanceAp$i cannot have a value when no ap${i}End is supplied"
  val value = Json.obj()
}

case class AllocatedRestrictionLaterPeriodSupplied(i: Int)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"ap${i}End"
  val errorMessage: String = s"ap${i}End cannot be supplied as a previous period is missing"
  val value = Json.obj()
}

case class AllocatedRestrictionNegative(i: Int, amt: BigDecimal)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"disallowanceAp$i"
  val errorMessage: String = s"disallowanceAp$i cannot be negative"
  val value = Json.obj()
}

case class AllocatedRestrictionDecimalError(i: Int, amt: BigDecimal)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"disallowanceAp$i"
  val errorMessage: String = s"disallowanceAp$i has greater than the allowed 2 decimal places."
  val value = Json.toJson(amt)
}

case class AllocatedRestrictionTotalNotSupplied()(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "totalDisallowances"
  val errorMessage: String = "totalDisallowances must be supplied if restrictions are supplied"
  val value = Json.obj()
}

case class AllocatedRestrictionTotalNegative(amt: BigDecimal)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "totalDisallowances"
  val errorMessage: String = "totalDisallowances cannot be negative"
  val value = Json.obj()
}

case class AllocatedRestrictionTotalDecimalError(amt: BigDecimal)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "totalDisallowances"
  val errorMessage: String = "totalDisallowances has greater than the allowed 2 decimal places."
  val value = Json.toJson(amt)
}

case class AllocatedRestrictionTotalDoesNotMatch(amt: BigDecimal, calculatedAmt: BigDecimal)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "totalDisallowances"
  val errorMessage: String = s"The totalDisallowances was $calculatedAmt which does not match the supplied amount of $amt"
  val value = Json.obj()
}

case class AllocatedRestrictionDateBeforePrevious(i: Int)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ s"ap${i}EndDate"
  val errorMessage: String = s"ap${i}End cannot be equal to or before ap${i-1}EndDate"
  val value = Json.obj()
}

case class Ap1NotAfterGroupStartDate(ap1Date: LocalDate, groupStartDate: LocalDate)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "ap1EndDate"
  val errorMessage: String = s"ap1EndDate ($ap1Date) must be after the group accounting period start date ($groupStartDate)"
  val value = Json.obj("ap1EndDate" -> ap1Date, "groupStartDate" -> groupStartDate)
}

case class Ap3BeforeGroupEndDate(ap3Date: LocalDate, groupEndDate: LocalDate)(implicit topPath: JsPath) extends Validation {
  val path = topPath \ "ap3EndDate"
  val errorMessage: String = s"ap3EndDate ($ap3Date) is before the group accounting period end date ($groupEndDate)"
  val value = Json.obj("ap3EndDate" -> ap3Date, "groupEndDate" -> groupEndDate)
}