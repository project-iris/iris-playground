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

func main() {
	// Connect to the Iris network
	connection, err := iris.Connect(55555)
	if err != nil {
		panic(err)
	}
	defer connection.Close()

// START OMIT
// Open an outbound tunnel to a data store
tunnel, err := connection.Tunnel("repository", time.Second) // HLtunnel
if err != nil {
	fmt.Println("Tunneling failed:", err); return
}
defer tunnel.Close() // HLtunnel

// Request a file and retrieve the multi-part response
tunnel.Send([]byte("some file"), time.Second) // HLtunnel
for {
	msg, err := tunnel.Recv(time.Second) // HLtunnel
	if err != nil {
		break
	}
	fmt.Println(string(msg))
}
// END OMIT
}
