package controllers.commercial

import com.gu.contentapi.client.model.v1.ItemResponse
import common.commercial.hosted._
import common.commercial.hosted.hardcoded.{HostedPages, LegacyHostedPages}
import common.{Edition, ExecutionContexts, Logging}
import contentapi.ContentApiClient
import model.Cached.RevalidatableResult
import model.{Cached, NoCache}
import play.api.mvc.{Action, Controller}
import views.html.hosted.{guardianHostedArticle, guardianHostedArticle2, guardianHostedGallery, guardianHostedVideo}
import views.support.RenderOtherStatus

import scala.concurrent.Future
import scala.util.control.NonFatal

class HostedContentController extends Controller with ExecutionContexts with Logging {

  private def renderPage(
    campaignName: String,
    pageName: String,
    fromCampaignAndPageName: (String, String) => Option[HostedPage]
  ) = Action { implicit request =>
    fromCampaignAndPageName(campaignName, pageName) match {
      case Some(page: HostedVideoPage) => Cached(60)(RevalidatableResult.Ok(guardianHostedVideo(page)))
      case Some(page: HostedGalleryPage) => Cached(60)(RevalidatableResult.Ok(guardianHostedGallery(page)))
      case Some(page: HostedArticlePage) => Cached(60)(RevalidatableResult.Ok(guardianHostedArticle(page)))
      case Some(page: HostedArticlePage2) => Cached(60)(RevalidatableResult.Ok(guardianHostedArticle2(page)))
      case _ => NoCache(NotFound)
    }
  }

  def renderLegacyHostedPage(campaignName: String, pageName: String) =
    renderPage(campaignName, pageName, LegacyHostedPages.fromCampaignAndPageName)

  def renderHostedPage(campaignName: String, pageName: String) =
    renderPage(campaignName, pageName, HostedPages.fromCampaignAndPageName)

  // todo: path param
  def renderCapiHostedPage() = Action.async { implicit request =>

    val path = "advertiser-content/renault-car-of-the-future/design-competition-episode2"

    // todo: move out to capi
    def lookup(itemId: String): Future[ItemResponse] = {
      val edition = Edition(request)
      // todo reduce props fetched
      val capiItem = ContentApiClient.item(itemId, edition)
                     .showTags("all")
                     .showFields("all")
                     .showReferences("all")
                     .showAtoms("all")
      ContentApiClient.getResponse(capiItem)
    }

    lookup(path) map { response =>


      val content = response.content.get


      HostedVideoPage.fromContent(content) map { page =>
        Cached(60)(RevalidatableResult.Ok(guardianHostedVideo(page)))
      } getOrElse {
        log.error(s"Couldn't parse capi response: $response")
        NoCache(NotFound)
      }
    } recover {
      case NonFatal(e) =>
        log.warn(s"Capi lookup of item '$path' failed: ${e.getMessage}")
        NoCache(NotFound)
    }
  }
}
