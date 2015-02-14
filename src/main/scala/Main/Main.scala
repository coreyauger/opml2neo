package Main

import scala.xml._
import DriveNode._

/**
 * Created by suroot on 13/02/15.
 */


object Main extends App {

  override def main(args: Array[String]) {
    println(args)
    val topic = "BBC"
    val lines = scala.io.Source.fromFile(s"data/${topic}.opml").mkString
    //println(lines)
    val xml = XML.loadString(lines)
    val body = xml  \ "body"
    val outline = body \ "outline"
    val drive = parseOutLine(outline \ "outline")

    println(
      s"""
        |MERGE (t:Topic {name:'${topic}'})
        |CREATE UNIQUE (t)-[:MANAGES]->(p:Profile {id:'${topic}', name:'${topic}',avatarUrl:'http://www.walkabout.im/assets/images/profiles/avatar-${topic}.png', cover:'http://www.walkabout.im/assets/images/profiles/cover-${topic}.jpg'})-[:OWNS]->(d:Drive {id:'CNN'});
      """.stripMargin)

    println(driveNodeToNeo(topic, drive))

    // First off we need to delete the structure in neo4j

  }
}
