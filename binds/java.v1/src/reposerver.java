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
@Override public void handleTunnel(Tunnel tunnel) { // HLtunnel
    try {
        // Fetch the file name
        String name = new String(tunnel.receive(1000)); // HLtunnel

        // Simulate sending some multi-part data stream
        for (int i = 0; i < 10; i++) {
            String part = ("Repo #" + id + ": <" + name + "> part #" + (i+1));
            tunnel.send(part.getBytes(), 1000); // HLtunnel
        }
    } catch (IOException | TimeoutException | ClosedException e) {
        e.printStackTrace();
    } finally {
        // Make sure the tunnel is cleaned up
        try {
            tunnel.close(); // HLtunnel
        } catch (IOException | ClosedException e) {
            System.out.println("Failed to closed teh tunnel: " + e.getMessage());
        }
    }
}
// END OMIT
        }

        // Connect to the Iris network
        try (Service service = new Service(55555, "Javatarium", new RepoServer())) {
            // Serve a while, then quit
            System.out.println("Waiting for requests...");
            Thread.sleep(100_000);
        }
    }
}
