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

type datastore struct {
	id int
}

func (d *datastore) Init(conn *iris.Connection) error         { return nil }
func (d *datastore) HandleBroadcast(msg []byte)               { panic("Not implemented!") }
func (d *datastore) HandleRequest(msg []byte) ([]byte, error) { panic("Not implemented!") }
func (d *datastore) HandleDrop(reason error)                  { panic("Not implemented!") }

// START OMIT
// ServiceHandler callback, invoked when a tunnel is inbound
func (d *datastore) HandleTunnel(tunnel *iris.Tunnel) { // HLtunnel
	// Make sure tunnel is cleaned up
	defer tunnel.Close() // HLtunnel

	// Fetch the file name
	name, _ := tunnel.Recv(time.Second) // HLtunnel

	// Simulate sending some multi-part data stream
	for i := 1; i <= 10; i++ {
		part := fmt.Sprintf("Go repo #%d: <%s> part #%d", d.id, string(name), i)
		tunnel.Send([]byte(part), time.Second) // HLtunnel
	}
}

// END OMIT

func main() {
	id := rand.Intn(100)

	// Connect to the Iris network, serve a while, then quit
	serv, err := iris.Register(55555, "repository", &datastore{id: id}, nil)
	if err != nil {
		panic(err)
	}
	defer serv.Unregister()

	fmt.Println("Waiting for inbound tunnels...")
	for i := 60; i >= 1; i-- {
		fmt.Printf("%d seconds left till terminate...\n", i)
		time.Sleep(time.Second)
	}
}
