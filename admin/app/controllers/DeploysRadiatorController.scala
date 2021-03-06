package controllers.admin

import common.Logging
import implicits.Requests
import model.NoCache
import model.deploys.{RiffRaffService, ApiResults}
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import model.deploys._
import play.api.libs.ws.WSClient

trait DeploysRadiatorController extends Controller with Logging with Requests {

  val riffRaff: RiffRaffService
  val teamcity: TeamcityService

  def getDeploys(pageSize: Option[String], projectName: Option[String], stage: Option[String]) = Action.async {
    riffRaff.getRiffRaffDeploys(pageSize, projectName, stage).map(ApiResults(_))
  }

  def getBuild(number: String) = Action.async {
    teamcity.getTeamCityBuild(number).map(ApiResults(_))
  }

  def renderDeploysRadiator() = Action {
    NoCache(Ok(views.html.deploysRadiator.main()))
  }

}

class DeploysRadiatorControllerImpl(wsClient: WSClient) extends DeploysRadiatorController {
  val httpClient = new HttpClient(wsClient)
  override val riffRaff = new RiffRaffService(httpClient)
  override val teamcity = new TeamcityService(httpClient)
}

