import ch.qos.logback.classic.Logger
import com.karalabe.iris.Connection
import com.karalabe.iris.Tunnel
import com.karalabe.iris.exceptions.ClosedException
import org.slf4j.LoggerFactory
import java.io.IOException

import scala.util.{Failure, Success, Try}

object RepoClient {
  // Disable the logger
  val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
  logger.detachAndStopAllAppenders()

  def main(args: Array[String]) {
    // Connect to the Iris network as a simple client
    val connection = new Connection(55555)
    try {
// START OMIT
// Open an outbound tunnel to a data store
Try(connection.tunnel("repository", 1000)) match { // HLtunnel
    case Failure(error) =>
        println("Tunneling failed: " + error.getMessage());

    case Success(tunnel) =>
        // Request a file and retrieve the multi-part response
        tunnel.send("some file".getBytes) // HLtunnel

        var active = true
        while (active) {
            Try(tunnel.receive) match { // HLtunnel
                case Success(message) => println(new String(message))
                case Failure(error)   => active = false
            }
        }
        tunnel.close // HLtunnel
}
// END OMIT
    } finally {
      connection.close
    }
  }
}
