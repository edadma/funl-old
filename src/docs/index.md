# Introduction

*FunL* (pronounced "funnel") is a functional dynamically typed scripting language. The name FunL stands for "fun language", but it can also stand for "functional language".


## Installing

If you haven't yet done so, please download the software by clicking on the *Download* link at the top.  If you are on Windows, then it's probably best to just download it to your Desktop.  Their is no special installation procedure for the software itself, other than the fact that it runs on a platform known as the JVM (Java Virtual Machine), which you can get for free at <http://java.com>.  Just click on *Free Java Download* and follow their installation instructions.  Once the JVM is installed, you will be able to run the FunL REPL (repeat evaluate print loop) by starting a command line window (however you do that in the operating system you are using), changing to the directory where you downloaded the program, and typing
	
	java -jar funl.jar

After executing the above command to run the REPL, you shoud see

	Welcome to FunL version 0.7-SNAPSHOT
	Type in expressions to have them evaluated.
	Type :help for more information.

	FunL>

If so, you can now enter FunL expressions at the prompt.

Program files are executed by the following command

	java -jar <path to funl.jar> <program filename>
	
The program filename can be entered without the `.funl` filename extension.  If the filename does not end with `.funl`, it is assumed.


## Getting Started

The FunL REPL allows expressions to be evaluated, but in the current version, no function declarations can be made.  Variable assignments are possible, however.  For example, if you enter

	1/2 + 2/3

at the prompt, you should see

	res1: funl.lia.Rational = 7/6

The example highlights FunL's *exact arithmetic* capability.  The type `lia.Rational` is the rational or fraction type. The REPL creates a new variable for every result.  In this case, the result variable is `res1`.  The following code

	res1^-1
	
should produce the expected result of inverting the fraction to yield `6/7`.

FunL also uses JSON syntax for the *map* type.  The following code (with each line entered separately)

	a = {'one': 1, 'two': 2}

	a.two

should produce the expected result of `2`.  Also, the *dot notation* can be used to access object members, as in JavaScript.


### Running Scripts

We will now look at running small scripts.  Two examples will be given in this sub-section.  The next major section presents a complete language tutorial.  The first example script is the obligatory "hello world" program in FunL.  Create a text file called `hello.funl` with the following contents:

	main
	  println( 'Hello World' )

Now, open a command window (shell) and change to the directory that contains `hello.funl` and type the command

	java -jar <path to funl.jar> hello

You should see `Hello World` being output.

The keyword `main` indicates the entry point of the program.  `println` means "print line" and causes the given expression to be output, followed by a line feed.

As a more interesting example, here is an (not fairly efficient) implementation of the Quick Sort.  Create a file called `quicksort.funl` with the following contents:

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

Run the program by typing

	java -jar <path to funl.jar> quicksort
	
at the command prompt.  You should see

	[0, 1, 2, 2, 3, 4]
	[Alice, Barry, Bob, Charlotte, Fred, Zoe]