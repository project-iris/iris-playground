import ch.qos.logback.classic.Logger;
import com.karalabe.iris.Service;
import com.karalabe.iris.ServiceHandler;
import com.karalabe.iris.Tunnel;
import com.karalabe.iris.exceptions.ClosedException;
import com.karalabe.iris.exceptions.InitializationException;
import com.karalabe.iris.exceptions.TimeoutException;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Random;

public class RepoServerEntry implements ServiceHandler {
    public static void main(String args[]) throws IOException, InterruptedException, InitializationException {
        // Disable the logger
        final Logger logger = (Logger) LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        logger.detachAndStopAllAppenders();

        class RepoServer implements ServiceHandler {
            // Generate a random ID for the repo server
            final int id = new Random().nextInt(100);

// START OMIT
// ServiceHandler callback, invoked when a tunnel is inbound
@Override public void handleTunnel(Tunnel tunnel) { // HLtunnel
    try {
        // Fetch the file name
        String name = new String(tunnel.receive()); // HLtunnel

        // Simulate sending some multi-part data stream
        for (int i = 1; i <= 10; i++) {
            String part = String.format("Java repo #%d: <%s> part #%d", id, name, i);
            tunnel.send(part.getBytes(), 1000); // HLtunnel
        }
        // Tear down the tunnel (should be in finally block)
        tunnel.close(); // HLtunnel
    } catch (Exception e) {
        e.printStackTrace();
    }
}
// END OMIT
        }
        // Connect to the Iris network, serve a while, then quit
        try (Service service = new Service(55555, "repository", new RepoServer())) {
            System.out.println("Waiting for inbound tunnels...");
            for (int i = 60; i >= 1; i--) {
                System.out.printf("%d seconds left till terminate...\n", i);
                Thread.sleep(1000);
            }
        }
    }
}
