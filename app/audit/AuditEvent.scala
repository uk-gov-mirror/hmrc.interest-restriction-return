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

package audit

trait AuditEvent {
  def auditType: String

  def details: Map[String, String]
}

trait AuditEventTypes {
  val FULL_RETURN = "FullSubmission"
  val ABBREVIATED_RETURN = "AbbreviatedSubmission"
  val REVOKE_REPORTING_COMPANY = "RevokeReportingCompany"
  val APPOINT_REPORTING_COMPANY = "AppointReportingCompany"
  val FAILED_VALIDATION = "ValidationFailed"
}
