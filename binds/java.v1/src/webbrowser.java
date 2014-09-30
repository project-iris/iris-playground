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
// Connect to the Iris network as a simple client
try (Connection conn = new Connection(55555)) { // HLreq
    // Issue a dummy request every second
    for (int i = 0; i < 100; i++) {
        byte[] request = ("Request #" + i).getBytes();

        try {
            byte[] reply = conn.request("webserver", request, 1000); // HLreq
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
