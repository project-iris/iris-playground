package main

import (
	"time"

	"gopkg.in/project-iris/iris-go.v1"
)

func main() {
	// Connect to the Iris network
	conn, err := iris.Connect(55555)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	// Publish hidden messages till eternity
	for {
		conn.Publish("unofficial", []byte("Silent echoes: The cake is a lie!"))
		time.Sleep(2 * time.Second)
	}
}
