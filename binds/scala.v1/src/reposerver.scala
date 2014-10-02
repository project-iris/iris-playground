import ch.qos.logback.classic.Logger
import com.karalabe.iris.Service
import com.karalabe.iris.ServiceHandler
import com.karalabe.iris.Tunnel
import org.slf4j.LoggerFactory
import java.util.Random

object RepoServerEntry {
  // Disable the logger
  val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
  logger.detachAndStopAllAppenders()

  def main(args: Array[String]) {
    class RepoServer extends ServiceHandler {
      // Generate a random ID for the repo server
      val id = new Random().nextInt(100)

// START OMIT
// ServiceHandler callback, invoked when a tunnel is inbound
override def handleTunnel(tunnel: Tunnel) { // HLtunnel
    // Fetch the file name
    val name = new String(tunnel.receive()) // HLtunnel

    // Simulate sending some multi-part data stream
    for (i <- 1 to 10) {
        val part = s"Scala repo #$id: <$name> part #$i"
        tunnel.send(part.getBytes, 1000) // HLtunnel
    }
    // Tear down the tunnel (should be in finally block)
    tunnel.close() // HLtunnel
}
// END OMIT
    }

    // Connect to the Iris network
    val service = new Service(55555, "repository", new RepoServer) // HLreq
    try {
      System.out.println("Waiting for inbound tunnels...")
      for (i <- 60 to 1 by -1) {
      	System.out.println(s"$i seconds left till terminate...")
      	Thread.sleep(1000)
      }
    } finally {
      service.close()
    }
  }
}
