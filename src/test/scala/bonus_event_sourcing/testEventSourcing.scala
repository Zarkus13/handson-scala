package bonus_event_sourcing

import support.HandsOnSuite
import bonus_event_sourcing.model.Posts
import bonus_event_sourcing.events._
import bonus_event_sourcing.events.PostEdited
import bonus_event_sourcing.events.PostContent
import bonus_event_sourcing.events.PostAdded


class testEventSourcing extends HandsOnSuite {

  val articleBiaiséSurlES =  PostContent("(_ + _) (<- l'opérateur panda, membre de la confrérie du semi groupe)",

                                                "à propos de la scalabilité infinie en lecture",

                                                               "bla bla bla")

  val articleUnPeuPlusSérieux = PostContent("($/_$/)",

                                              "L'Event Sourcing, une opportunité pour votre businezz !",

                                                              "bla bla bla")



  exercice("Ajout de post") {

    val postId = PostId.generate()

    val posts = Posts.fromHistory().apply(PostAdded(postId,articleBiaiséSurlES))

    posts.get(postId) should be('defined)

    posts.get(postId).get.content should be(articleBiaiséSurlES)

  }

  exercice("modification de post") {

    val postId = PostId.generate()

    val posts = Posts.fromHistory(PostAdded(postId, articleBiaiséSurlES))

    val post = posts.apply(PostEdited(postId, articleUnPeuPlusSérieux)).get(postId)

    post should be('defined)

    post.get.content should be(articleUnPeuPlusSérieux)
  }

  exercice("suppression d'un post") {

    val postId = PostId.generate()

    val posts = Posts.fromHistory(PostAdded(postId, articleUnPeuPlusSérieux))

    posts.get(postId) should be('defined)

    posts.apply(PostDeleted(postId)).get(postId) should be('empty)

  }

  exercice("versionning") {

    /* ....*/

  }

}
