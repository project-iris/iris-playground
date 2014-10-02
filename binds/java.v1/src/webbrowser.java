import ch.qos.logback.classic.Logger;
import com.karalabe.iris.Connection;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class WebBrowser {
    public static void main(String args[]) throws IOException, InterruptedException {
        // Disable the logger
        final Logger logger = (Logger)LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        logger.detachAndStopAllAppenders();

// START OMIT
// Connect to the network as a simple client
try (Connection connection = new Connection(55555)) { // HLreq
    // Issue a dummy request every second
    for (int i = 1; i <= 60; i++) {
        byte[] request = ("Request #" + i).getBytes();

        try {
            byte[] reply = connection.request("webserver", request, 1000); // HLreq
            System.out.println("Web reply: " + new String(reply));
        } catch (Exception e) {
            System.out.println("Request failed: " + e.getMessage());
        }
        Thread.sleep(1000);
    }
}
// END OMIT
    }
}
