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

package v1.controllers.actions

import play.api.http.HttpEntity
import play.api.mvc.{Action, AnyContent, ResponseHeader, Result, Results}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import utils.BaseSpec
import v1.connectors.mocks.{FakeFailingAuthConnector, FakeSuccessAuthConnector}
import v1.models.requests.IdentifierRequest

import scala.concurrent.Future

class AuthActionSpec extends BaseSpec {

  class Harness(authAction: AuthAction) {
    def onPageLoad(): Action[AnyContent] = authAction { _ => Results.Ok }
  }

  "Auth Action" when {
    "calling withInternalExtraHeaders" should {
      "add if it is internal" in {
        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](Some(Credentials("id","SCP"))), bodyParsers)
        val result = authAction.withInternalExtraHeaders(true,authAction(bodyParsers)(implicit request => {
          Future.successful(addHeaders(request))}))

        result.apply(fakeRequest).header.headers.get("isInternal").get shouldBe "true"
      }

      "add if it is not internal" in {
        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](Some(Credentials("id","SCP"))), bodyParsers)
        val result = authAction.withInternalExtraHeaders(false,authAction(bodyParsers)(implicit request => {
          Future.successful(addHeaders(request))}))

        result.apply(fakeRequest).header.headers.get("isInternal").get shouldBe "false"
      }

      "not delete any existing headers" in {
        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](Some(Credentials("id","SCP"))), bodyParsers)
        val result = authAction.withInternalExtraHeaders(false,authAction(bodyParsers)(implicit request => {
          Future.successful(addHeaders(request))}))

        result.apply(fakeRequest).header.headers.get("isInternal").get shouldBe "false"
        result.apply(fakeRequest.withHeaders("initialHeader" -> "test")).header.headers.get("initialHeader").isDefined shouldBe true
      }

      "overwrite any existing `isInternal` headers" in {
        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](Some(Credentials("id","SCP"))), bodyParsers)
        val result = authAction.withInternalExtraHeaders(false,authAction(bodyParsers)(implicit request => {
          Future.successful(addHeaders(request))}))

        result.apply(fakeRequest.withHeaders("isInternal" -> "true")).header.headers.get("isInternal").get shouldBe "false"
      }


    }

    "the user is logged in with providerId returned" must {

      "successful cary out request" in {

        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](Some(Credentials("id","SCP"))), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe OK
      }
    }

    "the user is logged in with NO providerId returned" must {

      "redirect to unauthorised" in {

        val authAction = new AuthAction(new FakeSuccessAuthConnector[Option[Credentials]](None), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user hasn't logged in" must {

      "redirect the user to log in " in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new MissingBearerToken), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user's session has expired" must {

      "redirect the user to log in " in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new BearerTokenExpired), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user doesn't have sufficient enrolments" must {

      "redirect the user to the unauthorised page" in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new InsufficientEnrolments), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user doesn't have sufficient confidence level" must {

      "redirect the user to the unauthorised page" in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new InsufficientConfidenceLevel), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user used an unaccepted auth provider" must {

      "redirect the user to the unauthorised page" in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new UnsupportedAuthProvider), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user has an unsupported affinity group" must {

      "redirect the user to the unauthorised page" in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new UnsupportedAffinityGroup), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }

    "the user has an unsupported credential role" must {

      "redirect the user to the unauthorised page" in {

        val authAction = new AuthAction(new FakeFailingAuthConnector(new UnsupportedCredentialRole), bodyParsers)
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(fakeRequest)

        status(result) shouldBe UNAUTHORIZED
      }
    }
  }

  private def addHeaders(request: IdentifierRequest[AnyContent]) = {
    Result(header = ResponseHeader(1, request.headers.map(h => h.toSimpleMap)), HttpEntity.NoEntity)
  }
}