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

func (d *datastore) HandleTunnel(tun *iris.Tunnel) { // HLtunnel
	// Make sure tunnel is cleaned up
	defer tun.Close() // HLtunnel

	// Fetch the file name
	name, err := tun.Recv(time.Second) // HLtunnel
	if err != nil {
		return
	}
	// Simulate sending some multi-part data stream
	for i := 0; i < 10; i++ {
		part := fmt.Sprintf("Repo #%d: <%s> part #%d", d.id, string(name), i+1)
		tun.Send([]byte(part), time.Second) // HLtunnel
	}
}

// END OMIT

func main() {
	id := rand.Intn(100)

	// Connect to the Iris network
	serv, err := iris.Register(55555, "Gopher Library", &datastore{id: id}, nil) // HLtunnel
	if err != nil {
		panic(err)
	}
	defer serv.Unregister()

	fmt.Printf("Database #%d: waiting for connections...", id)
	time.Sleep(100 * time.Second)
}
