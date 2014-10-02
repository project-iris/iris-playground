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
// Topic subscription handler processing inbound events
class Chell implements TopicHandler {
    @Override public void handleEvent(byte[] event) { // HLsub
        System.out.println(new String(event) + "\n");
    }
}
// Connect to the Iris network as Chell
try (Connection connection = new Connection(55555)) { // HLsub
    System.out.println("Tuning in to Aperture channels...");
    connection.subscribe("official", new Chell()); // HLsub

    Thread.sleep(60 * 1000);
}
// END OMIT
    }
}
