import ch.qos.logback.classic.Logger
import com.karalabe.iris.Service
import com.karalabe.iris.ServiceHandler
import com.karalabe.iris.exceptions.InitializationException
import com.karalabe.iris.exceptions.RemoteException
import org.jetbrains.annotations.NotNull
import org.slf4j.LoggerFactory
import java.io.IOException
import java.util.Random

object WebServerEntry {
    // Disable the logger
      val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
      logger.detachAndStopAllAppenders

    def main(args: Array[String]) {
// START OMIT
class WebServer extends ServiceHandler {
    // Generate a random ID for the web server
    val id = new Random().nextInt(100)

    // Format each request a bit and return as the reply
    override def handleRequest(request: Array[Byte]): Array[Byte] = { // HLreq
        return (s"scala-www-$id: ${new String(request)}").getBytes
    }
}

// Register a webserver micro-service into the network
val service = new Service(55555, "webserver", new WebServer) // HLreq
try {
    System.out.println("Waiting for inbound requests...")
    Thread.sleep(60 * 1000)
} finally {
    service.close() // HLreq
}
// END OMIT
    }
}


