package main

import (
	"log"
	"os"

	"gopkg.in/inconshreveable/log15.v2"
	"gopkg.in/project-iris/iris-go.v1"
)

func init() {
	iris.Log.SetHandler(log15.StreamHandler(os.Stdout, log15.LogfmtFormat()))
}

// START OMIT
// Callback handler for inbound events
type handler struct{}

// Handler initialization, invoked after successful registration
func (h *handler) Init(conn *iris.Connection) error { return nil } // HLreg

func main() {
	// Register a micro-service instance into the Iris network
	serv, err := iris.Register(55555, "Gopher Service", new(handler), nil) // HLreg
	if err != nil {
		log.Fatalf("Failed to register service: %v.", err)
	}
	defer serv.Unregister() // HLreg
}

// Remaining callbacks methods, not used in this demo
func (h *handler) HandleBroadcast(msg []byte)               { panic("Not implemented!") }
func (h *handler) HandleRequest(msg []byte) ([]byte, error) { panic("Not implemented!") }
func (h *handler) HandleTunnel(tunn *iris.Tunnel)           { panic("Not implemented!") }
func (h *handler) HandleDrop(reason error)                  { panic("Not implemented!") }

// END OMIT
