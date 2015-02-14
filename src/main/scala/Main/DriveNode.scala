package Main

import scala.xml._


sealed trait DriveNode
case class Uri(uri: String, title: String, description: String, imgs: String, isGenerator: Boolean, date: String) extends DriveNode
case class Category(name: String, contents: List[DriveNode]) extends DriveNode


/**
 * Created by suroot on 13/02/15.
 */
object DriveNode {

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

}
