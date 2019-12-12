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

package connectors

import assets.appointReportingCompany.AppointReportingCompanyConstants._
import connectors.httpParsers.AppointReportingCompanyHttpParser.{AppointReportingCompanyResponse, ErrorResponse, SuccessResponse, UnexpectedFailure}
import connectors.mocks.MockHttpClient
import models.appointReportingCompany.AppointReportingCompanyModel
import play.api.http.Status._
import utils.BaseSpec

class AppointReportingCompanyConnectorSpec extends MockHttpClient with BaseSpec {

  "AppointReportingCompanyConnector.appoint" when {

    def setup(response: AppointReportingCompanyResponse): AppointReportingCompanyConnector = {
      val desUrl = "http://localhost:9262/interest-restriction/reporting-company/appoint"
      mockHttpPost[AppointReportingCompanyModel, Either[ErrorResponse, SuccessResponse]](desUrl, appointReportingCompanyModelMax)(response)
      new AppointReportingCompanyConnector(mockHttpClient, appConfig)
    }

    "appointment is successful" should {

      "return a Right(SuccessResponse)" in {

        val connector = setup(Right(SuccessResponse(ackRef)))
        val result = connector.appoint(appointReportingCompanyModelMax)

        await(result) shouldBe Right(SuccessResponse(ackRef))
      }
    }

    "update is unsuccessful" should {

      "return a Left(UnexpectedFailure)" in {

        val connector = setup(Left(UnexpectedFailure(INTERNAL_SERVER_ERROR, "Error")))
        val result = connector.appoint(appointReportingCompanyModelMax)

        await(result) shouldBe Left(UnexpectedFailure(INTERNAL_SERVER_ERROR, "Error"))
      }
    }
  }
}
