  Iris playground
===================

This is the official binding playground of the Iris cloud messaging framework. It is a specially prepared [Docker](https://docker.com/) container with an embedded Iris node, capable of running client code written in any of the officially supported languages. On top of this environment, a presentation software is running, showcasing the basics of Iris, as well as demo challenges and executable solutions using all official client APIs.

The currently supported demos are: [`Erlang`](http://play.iris.karalabe.com/talks/binds/erlang.v1.slide) [`Go`](http://play.iris.karalabe.com/talks/binds/go.v1.slide) [`Java`](http://play.iris.karalabe.com/talks/binds/java.v1.slide) [`Scala`](http://play.iris.karalabe.com/talks/binds/scala.v1.slide)

  Quickstart
--------------

As the playground is based on Docker containers, it requires only a working Docker installation. All the complexity of installing required programming language SDKs, individual Iris client dependencies and their configurations is hidden inside the container, completely isolated from the host machine.

### Fetching the playground

The easiest way of assembling the playground is simply downloading the trusted build from the docker image registry (**large**):

```bash
$ docker pull iris/playground
```

However, if you prefer building the playground locally, simply pull the repository and issue a Docker build:

```bash
$ git pull https://github.com/project-iris/iris-playground.git
$ docker build -t iris/playground ./iris-playground
```

### Running the playground

After downloading or assembling the container, it can be started with:

```bash
$ docker run -d -p 3999:3999 -e "ORIGHOST=localhost" iris/playground
```

Details of the above command:

 * `-d`:  start in daemon mode in the background
 * `-p 3999:3999`: publish the internal port 3999 (second) to the local 3999 port (first)
 * `-e "ORIGHOST=localhost"`: browser origin host where code execution is allowed from

Note, it may take up to 10 seconds for Iris to converge before code snippets execute properly.

### Accessing the playground

After startup, the executable demo presentations can be accessed from:

 * Erlang: [http://localhost:3999/talks/binds/erlang.v1.slide](http://localhost:3999/talks/binds/erlang.v1.slide)
 * Go:     [http://localhost:3999/talks/binds/go.v1.slide](http://localhost:3999/talks/binds/go.v1.slide)
 * Java:   [http://localhost:3999/talks/binds/java.v1.slide](http://localhost:3999/talks/binds/java.v1.slide)
 * Scala:  [http://localhost:3999/talks/binds/scala.v1.slide](http://localhost:3999/talks/binds/scala.v1.slide)

The lengthy path (`/talks/binds`) is required to mimic the main Iris website layout.

  Contributions
-----------------

Currently my development aims are to stabilize the project and its language bindings. Hence, although I'm open and very happy for any and all contributions, the most valuable ones are tests, benchmarks and actual binding usage to reach a high enough quality.

Due to the already significant complexity of the project (Iris in general), I kindly ask anyone willing to pinch in to first file an [issue](https://github.com/project-iris/iris-playground/issues) with their plans to achieve a best possible integration :).

Additionally, to prevent copyright disputes and such, a signed contributor license agreement is required to be on file before any material can be accepted into the official repositories. These can be filled online via either the [Individual Contributor License Agreement](http://iris.karalabe.com/icla) or the [Corporate Contributor License Agreement](http://iris.karalabe.com/ccla).
