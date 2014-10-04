import ch.qos.logback.classic.Logger
import com.karalabe.iris.{Connection, TopicHandler}
import org.slf4j.LoggerFactory

object PortalChellEntry {
  // Disable the logger
  val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
  logger.detachAndStopAllAppenders()

    def main(args: Array[String]) {
// START OMIT
// Topic subscription handler processing inbound events
class Chell extends TopicHandler {
    override def handleEvent(event: Array[Byte]) { // HLsub
        println(new String(event) + "\n")
    }
}
// Connect to the Iris network as Chell
val connection = new Connection(55555) // HLsub
try {
    println("Tuning in to Aperture channels...");
    connection.subscribe("official", new Chell) // HLsub

    Thread.sleep(60 * 1000)
} finally {
    connection.close()
}
// END OMIT
    }
}
