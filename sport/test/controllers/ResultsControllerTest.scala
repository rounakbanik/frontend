package controllers

import football.controllers.ResultsController
import org.scalatest.{DoNotDiscover, Matchers, WordSpec}
import play.api.libs.json.JsValue
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import test.FootballTestData

import scala.concurrent.Future

@DoNotDiscover class ResultsControllerTest
  extends WordSpec
  with Matchers
  with FootballTestData {

  val resultsController = new ResultsController(testCompetitionsService)

  "GET all results" should {
    val request = FakeRequest(method = "GET", path = "/football/results.json")
    val action = resultsController.allResultsJson()
    lazy val response: Future[Result] = call(action, request)
    "200" in {
      status(response) should be(200)
    }
  }
  "GET all results for a specific date" when {
    "date is correct" should {
      val request = FakeRequest(method = "GET", path = "/football/results/2016/May/3.json")
      val action = resultsController.allResultsForJson("2016", "May", "3")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)

      }
    }
  }
  "GET more results for a specific date" should {
    val request = FakeRequest(method = "GET", path = "/football/results/more/2016/May/3.json")
    val action = resultsController.moreResultsForJson("2016", "May", "3")
    lazy val response: Future[Result] = call(action, request)
    "200" in {
      status(response) should be(200)
    }
  }
  "GET results for a specific tag" when {
    "tag (competition) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/premierleague/results.json")
      val action = resultsController.tagResultsJson("premierleague")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag (team) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/liverpool/results.json")
      val action = resultsController.tagResultsJson("liverpool")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag doesn't exist" should {
      val request = FakeRequest(method = "GET", path = "/football/DONOTEXIST/results.json")
      val action = resultsController.tagResultsJson("DONOTEXIST")
      lazy val response: Future[Result] = call(action, request)
      "404" in {
        status(response) should be(404)
      }
    }
  }
  "GET results for a specific tag and date" when {
    "tag (competition) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/premierleague/results.json")
      val action = resultsController.tagResultsForJson("2016", "May", "3", "premierleague")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag (team) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/liverpool/results.json")
      val action = resultsController.tagResultsForJson("2016", "May", "3", "liverpool")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag doesn't exist" should {
      val request = FakeRequest(method = "GET", path = "/football/DONOTEXIST/results.json")
      val action = resultsController.moreTagResultsForJson("2016", "May", "3", "DONOTEXIST")
      lazy val response: Future[Result] = call(action, request)
      "404" in {
        status(response) should be(404)
      }
    }
  }
  "GET more results for a specific tag" when {
    "tag (competition) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/premierleague/results.json")
      val action = resultsController.moreTagResultsForJson("2016", "May", "3", "premierleague")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag (team) exists" should {
      val request = FakeRequest(method = "GET", path = "/football/liverpool/results.json")
      val action = resultsController.moreTagResultsForJson("2016", "May", "3", "liverpool")
      lazy val response: Future[Result] = call(action, request)
      "200" in {
        status(response) should be(200)
      }
    }
    "tag doesn't exist" should {
      val request = FakeRequest(method = "GET", path = "/football/DONOTEXIST/results.json")
      val action = resultsController.moreTagResultsForJson("2016", "May", "3", "DONOTEXIST")
      lazy val response: Future[Result] = call(action, request)
      "404" in {
        status(response) should be(404)
      }
    }
  }
}
