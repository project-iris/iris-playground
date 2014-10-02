import ch.qos.logback.classic.Logger
import com.karalabe.iris.Connection
import org.slf4j.LoggerFactory

import scala.util.{Success, Failure, Try}

object WebBrowser {
    // Disable the logger
    val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    logger.detachAndStopAllAppenders

    def main(args: Array[String]) {
// START OMIT
// Connect to the network as a simple client
val connection = new Connection(55555) // HLreq
try {
    // Issue a dummy request every second
    for (i <- 1 to 60) {
        val request = s"Request #$i".getBytes
        Try(connection.request("webserver", request, 1000)) match { // HLreq
            case Success(reply) => System.out.println("Web reply: " + new String(reply))
            case Failure(error) => System.out.println("Request failed: " + error.getMessage)
        }
        Thread.sleep(1000)
    }
} finally {
    connection.close() // HLreq
}
// END OMIT
    }
}
