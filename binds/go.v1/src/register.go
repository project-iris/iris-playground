package main

import (
	"fmt"
	"os"

	"gopkg.in/inconshreveable/log15.v2"
	"gopkg.in/project-iris/iris-go.v1"
)

func init() {
	iris.Log.SetHandler(log15.StreamHandler(os.Stdout, log15.LogfmtFormat()))
}

// START OMIT
// Callback handler for inbound events
type servicedemo struct{}

// Handler initialization, invoked after successful registration
func (s *servicedemo) Init(conn *iris.Connection) error { return nil } // HLreg

func main() {
	// Register a micro-service instance into the network
	service, err := iris.Register(55555, "Gopher Service", new(servicedemo), nil) // HLreg
	if err != nil {
		fmt.Println("Failed to register service:", err); return
	}
	defer service.Unregister() // HLreg
}

// Remaining callbacks methods, not used in this demo
func (s *servicedemo) HandleBroadcast(msg []byte)               { panic("Not implemented!") }
func (s *servicedemo) HandleRequest(msg []byte) ([]byte, error) { panic("Not implemented!") }
func (s *servicedemo) HandleTunnel(tunn *iris.Tunnel)           { panic("Not implemented!") }
func (s *servicedemo) HandleDrop(reason error)                  { panic("Not implemented!") }

// END OMIT
