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

// START OMIT
// Connect to the Iris network as a simple client
try (Connection conn = new Connection(55555)) {
    // Open an outbound tunnel to a data store
    try (Tunnel tun = conn.tunnel("Javatarium", 1000)) { // HLtunnel
        // Request a file and retrieve the multi-part response
        tun.send("some file".getBytes()); // HLtunnel

        while (true) {
            try {
                byte[] part = tun.receive();  // HLtunnel
                System.out.println(new String(part));
            } catch (ClosedException e) {
                break;
            }
        }
    } catch (Exception e) {
        System.out.println("Data store connection failed: " + e.getMessage());
    }
}
// END OMIT
    }
}
