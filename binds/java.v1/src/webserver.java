import ch.qos.logback.classic.Logger;
import com.karalabe.iris.Service;
import com.karalabe.iris.ServiceHandler;
import com.karalabe.iris.exceptions.InitializationException;
import com.karalabe.iris.exceptions.RemoteException;
import org.jetbrains.annotations.NotNull;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Random;

public class WebServerEntry implements ServiceHandler {
    public static void main(String args[]) throws IOException, InterruptedException, InitializationException {
        // Disable the logger
        final Logger logger = (Logger) LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        logger.detachAndStopAllAppenders();

// START OMIT
class WebServer implements ServiceHandler {
    // Generate a random ID for the web server
    final int id = new Random().nextInt(100);

    // Format each request a bit and return as the reply
    @Override public byte[] handleRequest(byte[] request) throws RemoteException { // HLreq
        return ("java-www-" + id + ": " + new String(request)).getBytes();
    }
}

// Register a new webserver into the Iris network
try (Service service = new Service(55555, "webserver", new WebServer())) { // HLreq
    // Serve a while, then quit
    System.out.println("Waiting for requests...");
    Thread.sleep(100_000);
}
// END OMIT
    }
}
