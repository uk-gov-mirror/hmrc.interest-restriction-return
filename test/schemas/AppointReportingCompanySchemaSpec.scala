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

import helpers._
import play.api.libs.json.{JsValue, Json}
import schemas.helpers.appointReportingCompany.AppointReportingCompanyModel

class AppointReportingCompanySchemaSpec extends BaseSchemaSpec {

  def validate(json: JsValue): Boolean = validateJson("appointReportingCompanySchema.json", json)

  "AppointReportingCompany Json Schema" should {

    "Return valid" when {

      "Validated a successful JSON payload" in {

        val json = Json.toJson(AppointReportingCompanyModel())
        validate(json) shouldBe true
      }

      " crn is two characters and six numbers" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          reportingCompany = Some(ReportingCompany(crn = Some("AA111111")))
        ))

        validate(json) shouldBe true
      }

      "Start Date is a valid date" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          accountingPeriod = Some(AccountingPeriod(startDate = Some("2020-09-28"), endDate = Some("2020-10-01")))
        ))

        validate(json) shouldBe true
      }

      "Reporting Ultimate Parent ctutr is None" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          ultimateParentCompany = Some(ReportingUltimateParent(ctutr = None))
        ))

        validate(json) shouldBe true
      }

      "Reporting Ultimate Parent crn is None" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          ultimateParentCompany = Some(ReportingUltimateParent(crn = None))
        ))

        validate(json) shouldBe true
      }

      "Reporting Ultimate Parent company of Incorporation is None" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          ultimateParentCompany = Some(ReportingUltimateParent(countryOfIncorporation = None))
        ))

        validate(json) shouldBe true
      }

      "Reporting Ultimate Parent has local company Number is None" in {

        val json = Json.toJson(AppointReportingCompanyModel(
          ultimateParentCompany = Some(ReportingUltimateParent(hasLocalCompanyNumber = None))
        ))

        validate(json) shouldBe true
      }
    }

    "Return Invalid" when {

      "agentName" when {

        "is not supplied" in {

          val json = Json.toJson(AppointReportingCompanyModel(agentDetails = None))

          validate(json) shouldBe false
        }
      }

      "reportingCompany" when {

        "is not supplied" in {

          val json = Json.toJson(AppointReportingCompanyModel(reportingCompany = None))

          validate(json) shouldBe false
        }
      }

      "authorisingCompanies" when {

        "empty sequence" in {

          val json = Json.toJson(AppointReportingCompanyModel(authorisingCompanies = Some(Seq())))

          validate(json) shouldBe false
        }

        "not supplied" in {

          val json = Json.toJson(AppointReportingCompanyModel(authorisingCompanies = None))

          validate(json) shouldBe false
        }

        "companyName" when {

          "exceeds 160" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                companyName = Some("A" * (maxCompanyNameLength + 1))
              )))
            ))

            validate(json) shouldBe false
          }

          "is empty" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                companyName = Some("")
              )))
            ))

            validate(json) shouldBe false
          }

          "is not applied" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                companyName = None
              )))
            ))

            validate(json) shouldBe false
          }
        }

        "utr" when {

          s"below $utrLength" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                utr = Some("1" * (utrLength - 1))
              )))
            ))

            validate(json) shouldBe false
          }

          s"above $utrLength" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                utr = Some("1" * (utrLength + 1))
              )))
            ))

            validate(json) shouldBe false
          }

          "is non numeric" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                utr = Some("a" * (utrLength))
              )))
            ))

            validate(json) shouldBe false
          }

          "is a symbol" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                utr = Some("@")
              )))
            ))

            validate(json) shouldBe false
          }

          "is not applied" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              authorisingCompanies = Some(Seq(AuthorisingCompanies(
                utr = None
              )))
            ))

            validate(json) shouldBe false
          }
        }
      }

      "declaration" when {

        "not supplied" in {

          val json = Json.toJson(AppointReportingCompanyModel(declaration = None))

          validate(json) shouldBe false
        }
      }

      "Accounting Period" when {

        "Start Date is None" in {

          val json = Json.toJson(AppointReportingCompanyModel(
            accountingPeriod = Some(AccountingPeriod(startDate = None))
          ))

          validate(json) shouldBe false
        }

        "End Date is None" in {

          val json = Json.toJson(AppointReportingCompanyModel(
            accountingPeriod = Some(AccountingPeriod(endDate = None))
          ))

          validate(json) shouldBe false
        }
      }

      "Reporting Ultimate Parent Registered Company Name" when {

        "is None" in {
          val json = Json.toJson(AppointReportingCompanyModel(
            ultimateParentCompany = Some(ReportingUltimateParent(registeredCompanyName = None))
          ))

          validate(json) shouldBe false
        }
        "is too long" in {
          val json = Json.toJson(AppointReportingCompanyModel(
            ultimateParentCompany = Some(ReportingUltimateParent(registeredCompanyName = Some("a" * (maxCompanyNameLength + 1))))
          ))

          validate(json) shouldBe false
        }

        "is empty" in {
          val json = Json.toJson(AppointReportingCompanyModel(
            ultimateParentCompany = Some(ReportingUltimateParent(registeredCompanyName = Some("")))
          ))

          validate(json) shouldBe false
        }
      }

      "Reporting Ultimate Parent Company ctutr" when {

        "is too long" in {
          val json = Json.toJson(AppointReportingCompanyModel(
            ultimateParentCompany = Some(ReportingUltimateParent(ctutr = Some("1" * 11)))
          ))
          validate(json) shouldBe false
        }
        "is alphanumeric " in {
          val json = Json.toJson(AppointReportingCompanyModel(
            ultimateParentCompany = Some(ReportingUltimateParent(ctutr = Some("sd12871287")))
          ))
          validate(json) shouldBe false
        }

        "Reporting Ultimate Parent Company crnis empty" in {

            val json = Json.toJson(AppointReportingCompanyModel(
              ultimateParentCompany = Some(ReportingUltimateParent(crn = Some("")))
            ))

            validate(json) shouldBe false
          }


        "Reporting Ultimate Parent Company country of incorporation" when {

          "is not an ISO country code " in {

            val json = Json.toJson(AppointReportingCompanyModel(
              ultimateParentCompany = Some(ReportingUltimateParent(countryOfIncorporation = Some("Ladon Island")))
            ))
            validate(json) shouldBe false
          }

          "has numbers and special characters" in {
            val json = Json.toJson(AppointReportingCompanyModel(
              ultimateParentCompany = Some(ReportingUltimateParent(countryOfIncorporation = Some("111@!~~~###")))
            ))

            validate(json) shouldBe false
          }
        }
      }
    }
  }
}

