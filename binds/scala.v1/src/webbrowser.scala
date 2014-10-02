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
// Connect to the Iris network as a simple client
val conn = new Connection(55555) // HLreq
try {
    // Issue a dummy request every second
    for (i <- 0 to 99) {
        val request = ("Request #" + i).getBytes
        Try(conn.request("webserver", request, 1000)) match { // HLreq
            case Success(reply) => System.out.println("Web reply: " + new String(reply))
            case Failure(error) => System.out.println("Request failed: " + error.getMessage)
        }
        Thread.sleep(1000)
    }
} finally {
    conn.close
}
// END OMIT
    }
}
