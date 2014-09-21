package main

import (
	"fmt"
	"log"
	"time"

	"gopkg.in/inconshreveable/log15.v2"
	"gopkg.in/project-iris/iris-go.v1"
)

func init() {
	iris.Log.SetHandler(log15.DiscardHandler())
}

func main() {
	// Connect to the Iris network
	conn, err := iris.Connect(55555)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	// START OMIT

// Open an outbound tunnel to a data store
tun, err := conn.Tunnel("Gopher Library", time.Second) // HLtunnel
if err != nil {
	log.Fatalf("Data store connection failed: %v", err)
}
defer tun.Close() // HLtunnel

// Request a file and retrieve the multi-part response
tun.Send([]byte("some file"), time.Second) // HLtunnel
for {
	msg, err := tun.Recv(time.Second) // HLtunnel
	if err != nil {
		break
	}
	fmt.Println(string(msg))
}
	// END OMIT
}
