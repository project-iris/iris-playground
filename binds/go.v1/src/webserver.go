package main

import (
	"fmt"
	"math/rand"
	"time"

	"gopkg.in/inconshreveable/log15.v2"
	"gopkg.in/project-iris/iris-go.v1"
)

func init() {
	rand.Seed(time.Now().UnixNano())
	iris.Log.SetHandler(log15.DiscardHandler())
}

type webserver struct {
	id int
}

func (w *webserver) Init(conn *iris.Connection) error { return nil }
func (w *webserver) HandleBroadcast(msg []byte)       { panic("Not implemented!") }
func (w *webserver) HandleTunnel(tunn *iris.Tunnel)   { panic("Not implemented!") }
func (w *webserver) HandleDrop(reason error)          { panic("Not implemented!") }

// START OMIT

// Format each request a bit and return as the reply
func (w *webserver) HandleRequest(request []byte) ([]byte, error) { // HLreq
	return []byte(fmt.Sprint("go-www-", w.id, ": ", string(request))), nil
}

func main() {
	// Register a webserver micro-service into the network
	service, err := iris.Register(55555, "webserver", &webserver{id: rand.Intn(100)}, nil) // HLreq
	if err != nil {
		fmt.Println("Failed to register micro-service:", err); return
	}
	defer service.Unregister() // HLreq

	fmt.Println("Waiting for inbound requests...")
	time.Sleep(60 * time.Second)
}

// END OMIT
