import sbt._

class FoilProject(info : ProjectInfo)  extends DefaultProject(info) {
  val specs165 = "specs" % "specs" % "1.6.5" from "file:C:/Work/Scala/specs_2.8.0-1.6.5.jar"
}
