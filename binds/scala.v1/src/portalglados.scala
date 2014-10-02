import java.util.Random

import ch.qos.logback.classic.Logger
import com.karalabe.iris.Connection
import org.slf4j.LoggerFactory

object PortalGlados {
    // Disable the logger
    val logger: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    logger.detachAndStopAllAppenders()

    private final val wishes: Array[String] = Array(
        "There was even going to be a party for\nyou. A big party that all your friends were in-\nvited to. I even invited your best friend the\ncompanion cube. Of course he couldn't come be-\ncause you murdered him. All your other friends\ncouldn't come either because you don't have any\nother friends because of how unlikable you are.",
        "Remember when the platform was sliding\ninto the fire pit and I said 'Goodbye' and you\nwere like 'No way' and then I was all 'We pre-\ntended we were going to murder you'? That was\ngreat.",
        "Unbelievable! You, [Subject Name Here]\nmust be the pride of [Subject Hometown Here]!",
        "Please note that we have added a conse-\nquence for failure. Any contact with the cham-\nber floor will result in an 'unsatisfactory'\nmark on your official testing record, followed\nby death. Good luck!",
        "As part of a previously mentioned re-\nquired test protocol, we can no longer lie to\nyou. When the testing is over, you will be...\nmissed.",
        "I have your brain scanned and perma-\nnently backed up in case something terrible\nhappens to you, which it's just about to. Don't\nbelieve me? Here, I'll put you on... '~Helloooo\noooooooooo~' That's you! That's how dumb you\nsound! You have been wrong about every single\nthing you've ever done, including this thing.\nYou're not smart. You're not a scientist. You\nare not a doctor. You're not even a full-time\nemployee. Where did your life go so wrong?",
        "When the testing is over, you will be\nbaked, and then there will be cake.",
        "That thing is probably some kind of raw\nsewage container. Go ahead and rub your face\nall over it!",
        "It says so right here in your person-\nnel file; 'Unlikable. Liked by no one. A bit-\nter unlikable loner whose passing shall not be\nmourned' SHALL NOT BE MOURNED. That's exactly\nwhat it says. Very formal. Very official. It\nalso says you were adopted. So that is funny\ntoo.",
        "The Enrichment Center reminds you that\nthe Weighted Companion Cube cannot speak. In\nthe event that the Weighted Companion Cube does\nspeak, the Enrichment Center urges you to dis-\nregard its advice.",
        "Good news, I've figured out what that\nthing you just incinerated is. It was a morali-\nty core they installed after I flooded the en-\nrichment centre with a deadly neurotoxin to\nmake me stop flooding the enrichment centre\nwith deadly neurotoxin, so get comfortable\nwhile I warm up the neurotoxin emitters.",
        "Momentum; a function of mass and velo-\ncity; is conserved between portals. In layman's\nterms: speedy thing goes in, speedy thing comes\nout.",
        "What are you doing with that thing that\nno one knows what it does?",
        "We've both said things you're going to\nregret."
    )

    def main(args: Array[String]) {
// START OMIT
// Connect to the Iris network as GLaDOS
val connection = new Connection(55555) // HLpub
try {
    System.out.println("GLaDOS is online, sending wishes...")

    while (true) {
        // Pick a random wish from hidden 'wishes' array
        val wish = wishes(new Random().nextInt(wishes.length))

        connection.publish("official", ("GLaDOS: " + wish).getBytes) // HLpub
        Thread.sleep(5000)
    }
} finally {
    connection.close()
}
// END OMIT
    }
}
