# The FunL Programming Language

*FunL* (pronounced "funnel") is a functional dynamically typed scripting language. The name FunL stands for "fun language", but it can also stand for "functional language".

Here is an example program in FunL.  It's a simple static file web server (in under 50 lines).

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
        output.println( 'Content-Type: ' + (if (type != null) then type else 'text/html') )
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
		<version>0.7-SNAPSHOT</version>
	</dependency>

Add the following to your build file to use FunL in your SBT project:

	resolvers += Resolver.sonatypeRepo( "snapshots" )

	libraryDependencies ++= "org.funl-lang" %% "funl" % "0.7-SNAPSHOT"


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