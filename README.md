scala_cf3
=========
What this is now:
 - An incomplete parser for Context Free written in Scala with parser
combinators.

What this wants to be:
 - A parser and renderer for Context Free, usable as a Processing sketch, and
adding in some additional features such as realtime animation and hardware
rendering. The intention here is to pack the parser written in Scala into a
JAR which I utilize from Processing; while this is a little clunky, the P5 UI
and sites like OpenProcessing should still be able to run it. Using Processing
as a library from Scala itself is an option, but it lacks these benefits. The
remainder of the graphics might be done with something like GLGraphics. 
 - A web-based renderer for the same. One approach is with Processing.JS,
perhaps combined with something like J2JS that converts Java bytecode to
JavaScript; this would likely make parsing very slow, but could be totally
client-side. Another approach is to use a framework like Lift to do parsing
and some analysis server-side, but all rendering client-side by generating
JavaScript Canvas directives.

References:
 - Context Free: http://www.contextfreeart.org/
 - Processing: http://processing.org/
 - GLGraphics: http://glgraphics.sourceforge.net/
 - Processing.js: http://processingjs.org/
 - Helpful paper on writing parsers in Scala: http://www.labun.com/fh/ma.pdf
 - Another helpful page: http://www.artima.com/pins1ed/combinator-parsing.html
 - J2JS: http://www.j2js.com/ and https://github.com/decatur/j2js-compiler
 - Lift: http://liftweb.net/
