package main

import (
	"fmt"
	"time"

	"gopkg.in/inconshreveable/log15.v2"
	"gopkg.in/project-iris/iris-go.v1"
)

func init() {
	iris.Log.SetHandler(log15.DiscardHandler())
}

// START OMIT
// Topic subscription handler processing inbound events
type chell struct{}
func (c *chell) HandleEvent(event []byte) { // HLsub
	fmt.Printf("%s\n\n", string(event))
}

func main() {
	// Connect to the Iris network as Chell
	connection, err := iris.Connect(55555) // HLsub
	if err != nil {
		fmt.Printf("Failed to connect: %v\n", err); return
	}
	defer connection.Close()

	fmt.Println("Tuning in to Aperture channels...")
	connection.Subscribe("official", new(chell), nil) // HLsub

	time.Sleep(time.Minute)
}
// END OMIT
