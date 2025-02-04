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

package routing

import com.typesafe.config.ConfigFactory
import mocks.MockAppConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Inside, Matchers}
import play.api.Configuration
import play.api.http.HeaderNames.ACCEPT
import play.api.http.{HttpConfiguration, HttpFilters}
import play.api.libs.json.Json
import play.api.mvc.{EssentialAction, _}
import play.api.routing.Router
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.config.HttpAuditEvent
import utils.{BaseSpec, ErrorHandler}
import v1.models.errors.{InvalidAcceptHeaderError, UnsupportedVersionError}

class VersionRoutingRequestHandlerSpec extends BaseSpec with Matchers with MockFactory with Inside with MockAppConfig {
  test =>

  private val defaultRouter = mock[Router]
  private val v1Router = mock[Router]
  private val v2Router = mock[Router]
  private val v3Router = mock[Router]

  private val routingMap = new VersionRoutingMap {
    override val defaultRouter: Router = test.defaultRouter
    override val map = Map("1.0" -> v1Router, "2.0" -> v2Router, "3.0" -> v3Router)
  }

  class Test(implicit acceptHeader: Option[String]) {
    val httpConfiguration = HttpConfiguration("context")
    val auditConnector = mock[AuditConnector]
    val httpAuditEvent = mock[HttpAuditEvent]
    val configuration = Configuration("appName" -> "myApp", "bootstrap.errorHandler.warnOnly.statusCodes" -> Seq.empty[Int])

    private val errorHandler = new ErrorHandler(configuration, auditConnector, httpAuditEvent)
    private val filters = mock[HttpFilters]
    (filters.filters _).stubs().returns(Seq.empty)

    MockedAppConfig.featureSwitch.returns(Some(Configuration(ConfigFactory.parseString(
      """
        |version-1.enabled = true
        |version-2.enabled = true
      """.stripMargin))))

    val actionBuilder = DefaultActionBuilder(stubBodyParser(AnyContentAsEmpty))
    //noinspection ScalaDeprecation
    val requestHandler: VersionRoutingRequestHandler =
      new VersionRoutingRequestHandler(routingMap, errorHandler, httpConfiguration, mockAppConfig, filters, actionBuilder)

    def stubHandling(router: Router, validPaths: List[String])(handler: Handler) = {
      val routes: Router.Routes = { case rh: RequestHeader if validPaths.contains(rh.path) => handler}

      (router.routes _)
        .expects()
        .returning(routes)
    }

    def buildRequest(path: String): RequestHeader =
      acceptHeader
        .foldLeft(FakeRequest("GET", path)) { (req, accept) =>
          req.withHeaders((ACCEPT, accept))
        }
  }

  "Routing requests with no version" should {
    implicit val acceptHeader: None.type = None

    handleWithDefaultRoutes(defaultRouter)
  }

  "Routing requests with valid version" should {
    implicit val acceptHeader: Some[String] = Some("application/vnd.hmrc.1.0+json")

    handleWithDefaultRoutes(defaultRouter)
  }

  "Routing requests to non default router with no version" should {
    implicit val acceptHeader: None.type = None

    "return 406" in new Test {
      val handler: Handler = mock[Handler]
      stubHandling(defaultRouter, List())(handler)

      val request: RequestHeader = buildRequest("path")
      inside(requestHandler.routeRequest(request)) {
        case Some(a: EssentialAction) =>
          val result = a.apply(request).run()

          status(result) shouldBe NOT_ACCEPTABLE
          contentAsJson(result) shouldBe Json.toJson(InvalidAcceptHeaderError)
      }
    }
  }

  "Routing requests with v1" should {
    implicit val acceptHeader: Some[String] = Some("application/vnd.hmrc.1.0+json")

    handleWithVersionRoutes(v1Router)
  }

  "Routing requests with v2" should {
    implicit val acceptHeader: Some[String] = Some("application/vnd.hmrc.2.0+json")
    handleWithVersionRoutes(v2Router)
  }

  "Routing requests with unsupported version" should {
    implicit val acceptHeader: Some[String] = Some("application/vnd.hmrc.5.0+json")

    "return 404" in new Test {
      val handler: Handler = mock[Handler]
      stubHandling(defaultRouter, List())(handler)

      private val request = buildRequest("path")

      inside(requestHandler.routeRequest(request)) {
        case Some(a: EssentialAction) =>
          val result = a.apply(request).run()

          status(result) shouldBe NOT_FOUND
          contentAsJson(result) shouldBe Json.toJson(UnsupportedVersionError)
      }
    }
  }

  "Routing requests for supported version but not enabled" when {
    implicit val acceptHeader: Some[String] = Some("application/vnd.hmrc.3.0+json")

    "the version has a route for the resource" must {
      "return 404 Not Found" in new Test {
        val handler: Handler = mock[Handler]

        stubHandling(defaultRouter, List())(handler)

        private val request = buildRequest("path")
        inside(requestHandler.routeRequest(request)) {
          case Some(a: EssentialAction) =>
            val result = a.apply(request).run()

            status(result) shouldBe NOT_FOUND
            contentAsJson(result) shouldBe Json.toJson(UnsupportedVersionError)

        }
      }
    }
  }

  private def handleWithDefaultRoutes(router: Router)(implicit acceptHeader: Option[String]): Unit = {
    "if the request ends with a trailing slash" when {
      "handler found" should {
        "use it" in new Test {
          val handler: Handler = mock[Handler]
          stubHandling(router, List("path/"))(handler).once()

          requestHandler.routeRequest(buildRequest("path/")) shouldBe Some(handler)
        }
      }

      "handler not found" should {
        "try without the trailing slash" in new Test {
          val handler: Handler = mock[Handler]
          stubHandling(router, List("path"))(handler).twice()

          requestHandler.routeRequest(buildRequest("path/")) shouldBe Some(handler)
        }
      }
    }
  }

  private def handleWithVersionRoutes(router: Router)(implicit acceptHeader: Option[String]): Unit = {
    "if the request ends with a trailing slash" when {
      "handler found" should {
        "use it" in new Test {
          val handler: Handler = mock[Handler]

          stubHandling(defaultRouter, List())(handler).twice()
          stubHandling(router, List("path/"))(handler).once()

          requestHandler.routeRequest(buildRequest("path/")) shouldBe Some(handler)
        }
      }

      "handler not found" should {
        "try without the trailing slash" in new Test {
          val handler: Handler = mock[Handler]

          stubHandling(defaultRouter, List())(handler).twice()
          stubHandling(router, List("path"))(handler).twice()

          requestHandler.routeRequest(buildRequest("path/")) shouldBe Some(handler)
        }
      }
    }
  }

}
