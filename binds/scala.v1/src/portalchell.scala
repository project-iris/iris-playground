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
class ChellTuner extends TopicHandler {
    override def handleEvent(event: Array[Byte]) { // HLsub
        System.out.println(new String(event) + "\n")
    }
}
// Connect to the Iris network as Chell
val conn = new Connection(55555) // HLsub
try {
    // Subscribe to some interesting Iris topics
    System.out.println("Tuning in to Aperture channels...");

    conn.subscribe("official", new ChellTuner) // HLsub
    Thread.sleep(60000)
} finally {
    conn.close()
}
// END OMIT
    }
}
