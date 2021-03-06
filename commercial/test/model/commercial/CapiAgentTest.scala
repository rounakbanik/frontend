package model.commercial

import org.scalatest.{FlatSpec, Matchers}

class CapiAgentTest extends FlatSpec with Matchers {

  "idsFromShortUrls" should "give ID of a valid short URL" in {
    CapiAgent.idsFromShortUrls(Seq("p/43b2q")) shouldBe Seq("p/43b2q")
  }

  it should "give ID of a valid short URL with campaign suffix" in {
    CapiAgent.idsFromShortUrls(Seq("p/4dy39/stw")) shouldBe Seq("p/4dy39")
  }

  it should "give ID of a valid short URL with leading slash" in {
    CapiAgent.idsFromShortUrls(Seq("/p/4dy39")) shouldBe Seq("p/4dy39")
  }

  it should "give ID of a valid short URL with leading slash and campaign suffix" in {
    CapiAgent.idsFromShortUrls(Seq("/p/4dy39/stw")) shouldBe Seq("p/4dy39")
  }
}
