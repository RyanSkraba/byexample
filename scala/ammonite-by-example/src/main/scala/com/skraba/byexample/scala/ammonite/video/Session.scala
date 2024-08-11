package com.skraba.byexample.scala.ammonite.video

case class Session(
    id: String,
    title: String,
    speakers: String,
    track: String,
    startFile: Option[os.Path],
    startTime: Option[String],
    endFile: Option[os.Path],
    endTime: Option[String]
) {}
