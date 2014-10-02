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
	// START OMIT

// Connect to the network as a simple client
connection, err := iris.Connect(55555) // HLreq
if err != nil {
	fmt.Println("Failed to connect:", err); return
}
defer connection.Close() // HLreq

// Issue a dummy request every second
for i := 1; i <= 60; i++ {
	request := []byte(fmt.Sprint("Request #", i))

	reply, err := connection.Request("webserver", request, time.Second) // HLreq
	if err != nil {
		fmt.Println("Request failed:", err)
	} else {
		fmt.Println("Web reply:", string(reply))
	}
	time.Sleep(time.Second)
}
// END OMIT
}
