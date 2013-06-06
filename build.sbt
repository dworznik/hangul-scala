import S3._

organization := "in.anotherbrightidea"

name := "hangul"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"


s3Settings

mappings in upload := Seq((new java.io.File("target/scala-2.10/hangul_2.10-0.1.0-SNAPSHOT.jar"),"hangul_2.10-0.1.0-SNAPSHOT.jar"))

host in upload := "anotherbrightidea-sbt.s3.amazonaws.com"

credentials += Credentials(Path.userHome / ".s3credentials")
