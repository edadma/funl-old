# The FunL Programming Language

*FunL* (pronounced "funnel") is a functional dynamically typed scripting language. The name FunL stands for "fun language", but it can also stand for "functional language".

Here is an example program in FunL

    def
      qsort( [] )             = []
      qsort( p:xs )           =
        def
          filter( p, [] )     = []
          filter( p, x:xs )
            | p( x )          = x : filter( p, xs )
            | otherwise       = filter( p, xs )

        qsort( filter(e -> e < p, xs) ) + [p] + qsort( filter(e -> e >= p, xs) )

    println( qsort([4, 2, 1, 3, 0, 2]) )
    println( qsort(["Bob", "Alice", "Barry", "Zoe", "Charlotte", "Fred"]) )


## License

FunL is distributed under the LGPL v3 License, meaning that you are free to use it in your free or proprietary software.


## Documentation

The project's website is <http://funl-lang.org> where you will find documentation and download instructions so that you can get a binary executable of the interpreter.


## Usage

Use the following definition to use FunL in your Maven project:

	<dependency>
		<groupId>org.funl-lang</groupId>
		<artifactId>funl</artifactId>
		<version>0.5-SNAPSHOT</version>
	</dependency>

Add the following to your build file to use FunL in your SBT project:

	resolvers += Resolver.sonatypeRepo( "snapshots" )

	libraryDependencies ++= "org.funl-lang" %% "funl" % "0.5-SNAPSHOT"


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