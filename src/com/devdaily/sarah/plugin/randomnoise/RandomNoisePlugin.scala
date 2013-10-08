package com.devdaily.sarah.plugin.randomnoise

import com.devdaily.sarah.plugins._
import java.util.Calendar
import java.text.SimpleDateFormat
import com.devdaily.sarah.actors.StartPluginMessage
import com.devdaily.sarah.actors.StopPluginMessage
import com.devdaily.sarah.actors.SetPluginDir
import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging
import java.util.Properties
import scala.util.Random
import java.io.File
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor

class AkkaRandomNoisePlugin 
extends SarahAkkaActorBasedPlugin
with Logging
{
  val log = Logger("RandomNoise")
  var helper:ActorRef = _

  def receive = {
    case SetPluginDir(canonDir) =>          // this message is received first
         startHelper
         helper ! SetPluginDir(canonDir)

    case StartPluginMessage(theBrain) =>    // this message is received second
         brain = theBrain
         helper ! SetBrainReference(theBrain)
         helper ! StartHelperLoopMessage

    case whatTheHeck => 
         log.info("RandomNoisePlugin got an unexpected message:")
         log.info(whatTheHeck.toString)
  }
  
  def startHelper {
    helper = context.actorOf(Props(new RandomNoiseHelper), name = "RandomNoiseHelper")
  }
  
  /**
   * TODO These methods are needed to implement the SarahPlugin.
   *      Fix the API so they aren't needed for actors?
   */
  def textPhrasesICanHandle: List[String] = List("foo bar baz")
  def handlePhrase(phrase: String): Boolean = false
  def startPlugin = ()

} // end RandomNoisePlugin


// messages we pass
case object StartHelperLoopMessage
case class SetBrainReference(brain: ActorRef)


/**
 * This helper class now does the heavy lifting.
 */
class RandomNoiseHelper
extends Actor
with Logging
{
  
  val log = Logger("RandomNoiseHelper")
  private var brain:ActorRef = _

  private var canonPluginDir = ""
  private var allSoundFiles:Array[File] = _
  
  private var properties:Properties = _
  private val PROPERTIES_REL_FILENAME = "RandomNoise.properties"
  private val PROP_MAX_TIME_KEY = "max_wait_time_in_mins"  // key in the properties file
  private var maxWaitTimeInMins = 20
  
  // in case i need this
  //val randomNoisePlugin:ActorRef = context.parent

  def receive = {

    case SetPluginDir(canonDir) =>
         canonPluginDir = canonDir
         readConfigFile
         allSoundFiles = getRecursiveListOfSoundFiles(canonPluginDir)
         log.info("Found " + allSoundFiles.size + " sound files")

    case SetBrainReference(theBrain) =>
         brain = theBrain

    case StartHelperLoopMessage =>
         startLoop

    case unknown => 
         log.info(format("got an unknown request(%s), ignoring it", unknown.toString))

  }

  def startLoop {
    var count = 0
    var randomWaitTime = getRandomWaitTimeInMinutes
    while (true) {
      sleepForAMinute
      count += 1
      if (count >= randomWaitTime) {
        playSoundFile(getRandomSoundFile)
        count = 0
        randomWaitTime = getRandomWaitTimeInMinutes
      }
    }
  }
  
  def readConfigFile {
    try {
      log.info("in readConfigFile")
      val canonConfigFilename = canonPluginDir + PluginUtils.getFilepathSeparator + PROPERTIES_REL_FILENAME
      log.info("canonConfigFilename = " + canonConfigFilename)
      properties = PluginUtils.readPropertiesFile(canonConfigFilename)
      maxWaitTimeInMins = properties.getProperty(PROP_MAX_TIME_KEY).toInt
      log.info("(props) maxWaitTimeInMins = " + maxWaitTimeInMins)
    } catch {
      case e:Exception => log.error(e.getMessage)
    }
  }
  
  def getRandomWaitTimeInMinutes:Int = {
    val r = new Random
    val t = r.nextInt(maxWaitTimeInMins)
    log.info("random wait time is " + t)
    t
  }
  
  def playSoundFile(f:File) {
    log.info("1: " + f.getName())
    log.info("2: " + f.getAbsoluteFile())
    log.info("3: " + f.getAbsoluteFile().getName())
    log.info("4: " + f.getCanonicalFile())
    log.info("5: " + f.getCanonicalFile().getName())
    log.info("6: " + f.getCanonicalPath())
    log.info("7: " + f.getPath())
    brain ! new PlaySoundFileRequest(f.getCanonicalPath())
  }
  
  def getRandomSoundFile:File = {
    val r = new Random
    val i = r.nextInt(allSoundFiles.size)
    log.info("next sound file index: " + i)
    log.info("# of sound files: " + allSoundFiles.size)
    log.info("# of sound files: " + allSoundFiles.length)
    return allSoundFiles(i)
  }

  /**
   * Get a recursive list of all sound files, presumably from beneath the plugin dir.
   */
  def getRecursiveListOfSoundFiles(dirName: String) = {
    val files = PluginUtils.getRecursiveListOfFiles(new File(dirName))
    for (file <- files 
        if hasSoundFileExtension(file)
        if !soundFileIsLong(file)) yield file
  }

  def soundFileIsLong(f: File) = {
    if (f.getName.toLowerCase.contains("long")) true
    else false
  }
  
  def hasSoundFileExtension(file: File):Boolean = {
    val okFileExtensions = Array("wav", "mp3")
    for (extension <- okFileExtensions) {
      if (file.getName.toLowerCase.endsWith(extension)) return true
    }
    false
  }

  def sleepForAMinute = PluginUtils.sleep(60*1000)
  
  // returns true if minutes = 0, i.e., the current time is "on the hour"
  def onTheHour :Boolean = {
    val today = Calendar.getInstance().getTime()
    val minuteFormat = new SimpleDateFormat("mm")
    val currentMinuteAsString = minuteFormat.format(today)
    try {
      val currentMinute = Integer.parseInt(currentMinuteAsString)
      if (currentMinute % 60 == 0) return true
      else return false
    } catch {
      case _ => return false
    }
  }

  // returns the current hour as a string
  def getCurrentHour: String = {
    val today = Calendar.getInstance().getTime()
    val hourFormat = new SimpleDateFormat("hh")
    try {
      // returns something like "01" if i just return at this point, so cast it to
      // an int, then back to a string (or return the leading '0' if you prefer)
      val currentHour = Integer.parseInt(hourFormat.format(today))
      return "" + currentHour
    } catch {
      // TODO return Some/None/Whatever
      case _ => return "0"
    }
    return hourFormat.format(today)
  }
  
  // returns the current minute as a string
  def getCurrentMinute: String = {
    val today = Calendar.getInstance().getTime()
    val minuteFormat = new SimpleDateFormat("mm")
    // in this case, returning "01" is okay
    return minuteFormat.format(today)
  }
  
  // returns "AM" or "PM"
  def getAmOrPm: String = {
    val today = Calendar.getInstance().getTime()
    val amPmFormat = new SimpleDateFormat("a")
    return amPmFormat.format(today)
  }


}
  






