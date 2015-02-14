package Main

import scala.xml._

/**
 * Created by suroot on 13/02/15.
 */

sealed trait DriveNode
case class Uri(uri: String, title: String, description: String, imgs: String, isGenerator: Boolean, date: String) extends DriveNode
case class Category(name: String, contents: List[DriveNode]) extends DriveNode

object Main extends App {

  def parseOutLine(xml: NodeSeq):List[DriveNode] = {
    xml.map{
      case x:Node =>
        if( (x \ "@xmlUrl").text == "") {
          Category( (x\"@text").text.replace("\\","/"), parseOutLine( x \ "outline"))
        }else {
          Uri((x \ "@xmlUrl").text, (x \ "@text").text, "", "", true, "")
        }
    }.toList
  }


  def driveNodeToNeo(topic:String, nodes:List[DriveNode]): List[String] ={
    nodes.map{
      case Category(path, tail) =>
        val parts = path.split('/')
        val neoPath = parts.drop(1).map{
          case p =>
            val catId = path.substring(0,path.indexOf(p)) + p
            s"-[:CONTAINS]->(:Category {name:'${p}', path:'${catId}'})"
        }.mkString("")
        driveNodeToNeo(topic, tail).map {
          case end =>
              s"""
              |MATCH (d:Drive {id:'${topic}'})
              |CREATE UNIQUE (d)${neoPath}-[:CONTAINS]->
            """.stripMargin + end
        }.mkString("\n")

        //tail.map(driveNodeToNeo(topic,_)).flatten
      case Uri(uri, title, description, imgs, isGenerator, date) =>
        s"""
           |(f:Uri {
           |uri:'${uri}',
           |title:'${title}',
           |description: '${description}',
           |imgs: '',
           |date: '${date}',
           |isGenerator: 'true'
           |});
         """.stripMargin
    }
  }

  override def main(args: Array[String]) {
    println(args)
    val topic = "CNN"
    val lines = scala.io.Source.fromFile("CNN.opml").mkString
    //println(lines)
    val xml = XML.loadString(lines)
    val body = xml  \ "body"
    val outline = body \ "outline"
    val drive = parseOutLine(outline \ "outline")

    println(
      s"""
        |MERGE (t:Topic {name:'${topic}'})
        |CREATE UNIQUE (t)-[:MANAGES]->(p:Profile {id:'${topic}', name:'${topic}',avatarUrl:'http://www.wp7connect.com/wp-content/uploads/2012/02/cnn.jpg', cover:'http://www.newscaststudio.com/wp-content/uploads/2013/04/cnn-1.png'})-[:OWNS]->(d:Drive {id:'CNN'});
      """.stripMargin)

    println(driveNodeToNeo(topic, drive))

    // First off we need to delete the structure in neo4j

  }
}
