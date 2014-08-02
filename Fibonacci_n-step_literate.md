Computing the Fibonacci n-step and Related Sequences
====================================================

We want to display a table showing the first ten Fibo/Tribo/Tetra-nacci and Lucas numbers.

First we need to import the `TextTable` *constructor* (a function that creates an object of some kind) to help us display a nice table.

	import util.TextTable
	
We will be defining a function `fibLike` that will be the work-horse of this implementation.  It will take a list of initial values and generate a fibonacci-like sequence based on those initial values.  The behaviour of the function will depend on the number of initial values.  If given two initial value, it will generate a sequence that behaves like the Fibonacci sequence in that every value (after the first two) is the sum of the previous two values.  If given three initial values, then every value of the sequence (after the first three will be the sum of the previous three values.  And so on.

That function will make use of a queue, which is a first-in-first-out (FIFO) data structure.  So, we need to import the `Queue` constructor.

	native scala.collection.mutable.Queue

Now we can define the `fibLike` function.  It takes an list of initial sequence values called `init`.

	def fibLike( init ) =
	
The function begins by making a queue and calls it `q`, which is then filled with the initial values of the sequence in `init`.  The contents of `q` will always represent future values of the sequence as the algorithm proceeds.

		q = Queue()
		
		for i <- init do q.enqueue( i )

An internal recursive helper function called `fib` needs to be defined to actually generate the sequence using a simple recursive algorithm.  Both `q` and `fib` exist only within the `fibLike`'s internal name space, and are both "carried along" as the sequence is being computed.

		def fib =
		
The helper function first sums the contents of `q` and then appends that value to the back of the queue as the next value in the sequence.

			q.enqueue( sum(q) )
			
It then returns a sequence whose first value is taken from the head of the queue, and whose remaining values are generated by a recursive call to `fib`.

			q.dequeue() # fib()

And this is where things just got strange.  If all remaining values (of an un-ending sequence) are comming from a call to the same function, then doesn't that lead to an infinite loop without ever actually returning anything?  The answer is no.  The constructor for the sequence is the `#` symbol, in the about expression, which creates what is termed a *lazy list*.  The sequence object that is created knows the first value of the sequence, and it knows how to compute the rest but it doesn't actually do that until the values are requested.  The expression on the right hand side of `#` is said to be a *call-by-name* expression, meaning that it doesn't get executed until it needs to be.

The definition of `fibLike` ends by returning the sequence generated by `fib` with a 0 prepended to it.  The 0 is the zero'th value of the sequence and is needed because the sequence object is zero based.

		0 # fib()

We can now define a function called `fibN` to generate the family of Fibonacci n-step sequences.  `fibN` takes the step size as a parameter called `n`.  We observe that all Fibonacci n-step sequences begin with a 1 followed by n-1 powers of 2 beginning with 2 to the 0 (which is 1).  Therefore, we define `fibN` in terms of `fibLike` (defined above) by giving it the initial sequence just described.

	def fibN( n ) = fibLike( [1] + [2^i | i <- 0:n-1] )

Similarly the Lucas numbers are generated by a sequence called `lucas`, and is defined as the Fibonacci-like sequence beginning with 2, 1 as the starting values.

	val lucas = fibLike( [2, 1] )

We are done with all the definitions that we need, so it's time to setup a table object that will hold all the information to be displayed and then display that information in tabular format.  We begin by making a table object and calling it `t`.

	t = TextTable()
	
The headers are then added: 'k' is the index of the sequence, 'Fibonacci' is the normal Fibonacci numbers, 'Tribonacci' is the 3-step Fibonacci numbers, 'Tetranacci' the 4-step Fibonacci numbers, and 'Lucas' the Lucas numbers.  A line is then added to separate the header and data.

	t.header( 'k', 'Fibonacci', 'Tribonacci', 'Tetranacci', 'Lucas' )
	t.line()

Also, we need to tell the table object that all five columns are right aligned, because they're all numbers.

	for i <- 1..5
		t.rightAlignment( i )

To help us populate the table with the sequences we want, we place the four sequences into a 4-tupple called `seqs`.  The expression `fibN(2)` generates the 2-step Fibonacci numbers, and so on.

	seqs = (fibN(2), fibN(3), fibN(4), lucas)

Now we populate the table with the first ten values from each of the sequences in `seqs`, as well as the index value `k` for each row.

	for k <- 1..10
		t.row( ([k] + [seqs(i)(k) | i <- 0:4]).toIndexedSeq() )

Lastly, we display the contents of the table.

	print( t )

The above `print` statement should produce the following table:

| k  | Fibonacci | Tribonacci | Tetranacci | Lucas |
|---:|----------:|-----------:|-----------:|------:|
|  1 |         1 |          1 |          1 |     2 |
|  2 |         1 |          1 |          1 |     1 |
|  3 |         2 |          2 |          2 |     3 |
|  4 |         3 |          4 |          4 |     4 |
|  5 |         5 |          7 |          8 |     7 |
|  6 |         8 |         13 |         15 |    11 |
|  7 |        13 |         24 |         29 |    18 |
|  8 |        21 |         44 |         56 |    29 |
|  9 |        34 |         81 |        108 |    47 |
| 10 |        55 |        149 |        208 |    76 |