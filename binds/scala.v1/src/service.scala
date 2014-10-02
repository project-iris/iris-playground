import com.karalabe.iris.{Service, Connection, ServiceHandler, Tunnel}

// START OMIT
class ServiceDemo extends ServiceHandler {
    // Handler initialization, invoked after successful registration
    override def init(connection: Connection) { } // HLreg

    // Remaining callbacks methods, not used in this demo
    override def handleBroadcast(message: Array[Byte])              { }
    override def handleRequest(request: Array[Byte]): Array[Byte] = {return request }
    override def handleTunnel(tunnel: Tunnel)                       { }
    override def handleDrop(reason: Exception)                      { }
}

object ServiceDemo {
    def main(args: Array[String]) {
        // Register a micro-service instance into the network
        val service = new Service(55555, "Lausanne service", new ServiceDemo()) // HLreg

        // Unregister the service
        service.close() // HLreg
    }
}
// END OMIT
