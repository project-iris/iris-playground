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

// Connect to the Iris network as a simple client
conn, err := iris.Connect(55555) // HLreq
if err != nil {
	panic(err)
}
defer conn.Close() // HLreq

// Issue a dummy request every second
for i := 1; i <= 100; i++ {
	rep, err := conn.Request("webserver", []byte(fmt.Sprint("Request #", i)), time.Second) // HLreq
	if err != nil {
		fmt.Println("Request failed:", err)
	} else {
		fmt.Println("Web reply: ", string(rep))
	}
	time.Sleep(time.Second)
}
	// END OMIT
}
