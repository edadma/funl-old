Grammar
=======

Here is the grammar used to parse FunL presented both in EBNF and in railroad diagrams.


Syntactic Grammar
-----------------

.. raw:: html

	<iframe src="_static/diagram.xhtml" scrolling="no" id="the_iframe" onload="setHeight();" width="100%"></iframe>
	

Lexical Grammar
---------------

The reserved words in the language are:  ``and``, ``break``, ``by``, ``case``, ``class``, ``continue``, ``data``, ``def``, ``do``, ``elif``, ``else``, ``false``, ``for``, ``function``, ``if``, ``import``, ``in``, ``is``, ``loop``, ``mod``, ``native``, ``not``, ``null``, ``of``, ``or``, ``otherwise``, ``repeat``, ``return``, ``then``, ``true``, ``until``, ``val``, ``var``, ``while``, ``xor``, ``yield``.

The special delimiters are:  ``+``, ``*``, ``-``, ``/``, ``^``, ``(``, ``)``, ``[``, ``]``, ``|``, ``{``, ``}``, ``,``, ``=``, ``==``, ``!=``, ``<``, ``$``, ``>``, ``<-``, ``<=``, ``>=``, ``--``, ``++``, ``.``, ``..``, ``<-``, ``->``, ``=>``, ``+=``, ``-=``, ``*=``, ``/=``, ``^=``, ``:``, ``\\``, ``::``, ``@``, ``?``.

::

	decimalParser =
		rep1(digit) ~ optFraction ~ optExponent |
		fraction ~ optExponent

	sign = '+' | '-'

	optSign = opt( sign )

	fraction = '.' ~ rep1(digit)

	optFraction = opt( fraction )

	exponent = ('e' | 'E') ~ optSign ~ rep1(digit)

	optExponent = opt(exponent)