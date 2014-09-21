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

type server struct {
	id int
}

func (s *server) Init(conn *iris.Connection) error { return nil }
func (s *server) HandleBroadcast(msg []byte)       { panic("Not implemented!") }
func (s *server) HandleTunnel(tunn *iris.Tunnel)   { panic("Not implemented!") }
func (s *server) HandleDrop(reason error)          { panic("Not implemented!") }

// START OMIT

// Format each request a bit and return as the reply
func (s *server) HandleRequest(req []byte) ([]byte, error) { // HLreq
	return []byte(fmt.Sprint("go-www-", s.id, ": ", string(req))), nil
}

func main() {
	// Register a new webserver into the Iris network
	serv, err := iris.Register(55555, "webserver", &server{id: rand.Intn(100)}, nil) // HLreq
	if err != nil {
		panic(err)
	}
	defer serv.Unregister()

	// Serve a while, then quit
	fmt.Println("Waiting for requests...")
	time.Sleep(100 * time.Second)
}

// END OMIT
