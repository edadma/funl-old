# The FunL Programming Language

*FunL* (pronounced "funnel") is a functional dynamically typed scripting language. The name FunL stands for "fun language", but it can also stand for "functional language".  One of the goals in creating FunL was to make a scripting language that would be as enjoyable and convenient to use as Python but with the added support for pattern matching and more of a "functional" style of programming.  FunL has the same kind of indentation or "off-side rule" syntax as Python.

As an example, here is a tail-recursive version of the well known factorial function.

    def factorial( n )
      | n >= 0 =
        def
          fact( acc, 0 ) = acc
          fact( acc, n ) = fact( acc*n, n - 1 )

        fact( 1, n )
      | otherwise = error( "factorial: n should be non-negative" )

Here is the same calculation expressed more concisely (which is also tail-recursive internally).

    def factorial( n ) = product( 1..n )

Another goal in creating FunL was to have a scripting language that is highly Java and Scala interoperable.  FunL is implemented in Scala and therefore relies upon both the Java and Scala runtime support libraries.

To give a small example of a complete program in FunL that does something useful, here is a simple static file web server (in under 50 lines).

    native java.io.{File, InputStreamReader, BufferedReader, PrintStream}
    native java.net.ServerSocket
    import concurrent.thread
    import io.readFile

    val listener = ServerSocket( port )
    val dir = File( '.' ).getCanonicalPath()
    val types = {'txt': 'text/plain', 'html': 'text/html', 'css': 'text/css',
      'js': 'application/javascript', 'png': 'image/png', 'gif': 'image/gif', 'jpeg': 'image/jpeg'}

    def connection( socket ) =
      input = BufferedReader( InputStreamReader(socket.getInputStream(), 'UTF-8') )
      output = PrintStream( socket.getOutputStream(), true )

      def response( code, type ) =
        output.println( 'HTTP/1.1 ' + code )
        output.println( 'Content-Type: ' + (if type != null then type else 'text/html') )
        output.println()
        
        if (type == null) then output.println( '<!DOCTYPE html><html><header><title>' + code + '</title><body><h1>' + code + '</h1></body></html>' )

      val line = input.readLine()

      if line != null
        val (request, url, _) = tuple( line.split(' +') )
        val file = File( dir + url + (if url == '/' then 'index.html' else '') ).getCanonicalFile()

        if request == 'GET'
          if file.getPath().startsWith( dir ) and file.exists() and file.isFile() and file.canRead()
            ext = list( file.getPath().split('\\.') ).last()
            
            if (ext in types)
              response( '200 OK', types(ext) )
              output.write( readFile(file) )
              output.flush()
            else
              response( '415 Unsupported Media Type', null )
          else
            response( '404 Not Found', null )
        else
          response( '405 Method Not Allowed', null )

      socket.shutdownInput()
      socket.shutdownOutput()
      socket.close()

    loop thread( connection, listener.accept() ).start()

## License

FunL is distributed under the LGPL v3 License, meaning that you are free to use it in your free or proprietary software.


## Documentation

The project's website is <http://funl-lang.org> where you will find documentation and download instructions so that you can get a binary executable of the interpreter.

There are also many examples of FunL code to be found at <http://rosettacode.org/wiki/FunL>, which serve to document how FunL can be used in solving various types of programming problems in a concise and functional way.


## Usage

Use the following elements to use FunL in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>org.funl-lang</groupId>
		<artifactId>funl</artifactId>
		<version>0.17</version>
	</dependency>

Add the following to your `build.sbt` file to use FunL in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "org.funl-lang" %% "funl" % "0.17"


## Building

### Requirements

- SBT 13.2+
- Java 6+

Clone and build:

	git clone git://github.com/edadma/funl.git
	cd funl
	sbt assembly

This will build an executable requiring only that Java be installed.  You can now also type

	sbt run

to start the REPL, or

	sbt "run <FunL script file>"

to execute a script.  Note that the double quotes are required.


## Installing

These instructions apply to Ubuntu or any Debian based distribution.  It is assumed that Java is installed and in your PATH, as well as the `unzip` utility.

In these instructions `<path to jar>` means the path where the executable JAR file was downloaded to, and `<path to funl>` means the path to the directory that you created in which to install FunL.

Here are the steps.

1.  Download the latest release from <https://dl.bintray.com/edadma/generic/funl-dev.jar>.
2.  Create a directory where FunL should be installed.
2.  Start a shell.
3.  Enter `cd <path to jar>`
4.  Enter `unzip funl-dev.jar bin/funl -d <path to funl>`
5.  Enter `chmod a+x <path to funl>/bin/*`
6.  Enter `cp funl-dev.jar <path to funl>/bin`
7.  Open ~/.profile in an editor.
8.  Add `PATH="<path to funl>/bin:$PATH"`
9.  Enter `source ~/.profile`
10. Enter `funl`

After the last step, you should see

```
    ______            __
   / ____/_  __ ___  / /     FunL Programming Language
  / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr.
 / /   / /_/ / / / / /__     http://funl-lang.org/
/_/    \____/_/ /_/____/                                     

Welcome to FunL version 0.17
Type in expressions to have them evaluated.
Type :help for more information.
```

indicating that the installation worked.  You are inside the FunL REPL.  Press Ctrl-C to get out.


## Executable

The latest development executable can be downloaded from <https://dl.bintray.com/edadma/generic/funl-dev.jar>.
