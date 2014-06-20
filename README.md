# The FunL Programming Language

*FunL* (pronounced "funnel") is a functional dynamically typed scripting language. The name FunL stands for "fun language", but it can also stand for "functional language".  The goal in creating FunL was to make a scripting language that would be as enjoyable and convenient to use as Python but with the added support for pattern matching and more of an "functional" style of programming.  FunL has the same kind of indentation or "off-side rule" syntax that Python has.

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

    forever thread( connection, listener.accept() ).start()

## License

FunL is distributed under the LGPL v3 License, meaning that you are free to use it in your free or proprietary software.


## Documentation

The project's website is <http://funl-lang.org> where you will find documentation and download instructions so that you can get a binary executable of the interpreter.


## Usage

Use the following definition to use FunL in your Maven project:

	<dependency>
		<groupId>org.funl-lang</groupId>
		<artifactId>funl</artifactId>
		<version>1.0</version>
	</dependency>

Add the following to your build file to use FunL in your SBT project:

	resolvers += Resolver.sonatypeRepo( "snapshots" )

	libraryDependencies ++= "org.funl-lang" %% "funl" % "1.0"


## Building

### Requirements

- SBT 13.2+
- Java 6+

Clone and build:

	git clone git://github.com/FunL/funl.git
	cd funl
	sbt assembly

This will build an executable requiring only that Java be installed.  You can now also type

	sbt run

to start the REPL, or

	sbt "run <FunL script file>"

to execute a script.  Note that the double quotes are required.