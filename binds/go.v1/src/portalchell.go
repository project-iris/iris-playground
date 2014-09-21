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

type echo struct{}

// START OMIT

// Print all events arriving on a subscription
func (e *echo) HandleEvent(msg []byte) { // HLsub
	fmt.Printf("%s\n\n", string(msg))
}

func main() {
	// Connect to the Iris network as Chell
	conn, err := iris.Connect(55555) // HLsub
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	// Subscribe to some interesting Iris topics
	fmt.Println("Tuning in to Aperture channels...")
	conn.Subscribe("official", new(echo), nil) // HLsub

	time.Sleep(time.Minute)
}

// END OMIT
