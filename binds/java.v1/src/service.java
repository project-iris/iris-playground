import com.karalabe.iris.Connection;
import com.karalabe.iris.Service;
import com.karalabe.iris.ServiceHandler;
import com.karalabe.iris.Tunnel;
import com.karalabe.iris.exceptions.InitializationException;
import com.karalabe.iris.exceptions.RemoteException;

import java.io.IOException;

// START OMIT
public class ServiceDemo implements ServiceHandler {
    // Handler initialization, invoked after successful registration
    @Override public void init(Connection connection) throws InitializationException { } // HLreg

    public static void main(String args[]) {
        // Register a micro-service instance into the Iris network
        try (Service service = new Service(55555, "Javatar service", new ServiceDemo())) { // HLreg
            // Do something, before tearing down
        }
        catch (IOException | InitializationException | InterruptedException e) {
            throw new RuntimeException("Failed to register the service", e);
        }
    }

    // Remaining callbacks methods, not used in this demo
    @Override public void handleBroadcast(byte[] message)                                { }
    @Override public byte[] handleRequest(byte[] req) throws RemoteException { return req; }
    @Override public void handleTunnel(Tunnel tunnel)                                    { }
    @Override public void handleDrop(Exception reason)                                   { }
}
// END OMIT
