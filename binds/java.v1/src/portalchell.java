import ch.qos.logback.classic.Logger;
import com.karalabe.iris.Connection;
import com.karalabe.iris.ServiceHandler;
import com.karalabe.iris.TopicHandler;
import com.karalabe.iris.exceptions.ClosedException;
import com.karalabe.iris.exceptions.InitializationException;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class PortalChellEntry implements ServiceHandler {
    public static void main(String args[]) throws IOException, InterruptedException, InitializationException, ClosedException {
        // Disable the logger
        final Logger logger = (Logger) LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        logger.detachAndStopAllAppenders();

// START OMIT
class ChellTuner implements TopicHandler {
    @Override public void handleEvent(byte[] event) { // HLsub
        System.out.println(new String(event) + "\n");
    }
}
// Connect to the Iris network as Chell
try (Connection conn = new Connection(55555)) { // HLsub
    // Subscribe to some interesting Iris topics
    System.out.println("Tuning in to Aperture channels...");

    conn.subscribe("official", new ChellTuner()); // HLsub
    Thread.sleep(60_000);
}
// END OMIT
    }
}
