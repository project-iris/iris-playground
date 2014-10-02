import ch.qos.logback.classic.Logger;
import com.karalabe.iris.Connection;
import com.karalabe.iris.Tunnel;
import com.karalabe.iris.exceptions.ClosedException;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class RepoClient {
    public static void main(String args[]) throws IOException, InterruptedException {
        // Disable the logger
        final Logger logger = (Logger) LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        logger.detachAndStopAllAppenders();

        // Connect to the Iris network as a simple client
        try (Connection connection = new Connection(55555)) {
// START OMIT
// Open an outbound tunnel to a data store
try (Tunnel tunnel = connection.tunnel("repository", 1000)) { // HLtunnel
    // Request a file and retrieve the multi-part response
    tunnel.send("some file".getBytes()); // HLtunnel

    while (true) {
        try {
            byte[] part = tunnel.receive();  // HLtunnel
            System.out.println(new String(part));
        } catch (Exception e) {
            break;
        }
    }
} catch (Exception e) {
    System.out.println("Tunneling failed: " + e.getMessage());
}
// END OMIT
        }
    }
}
