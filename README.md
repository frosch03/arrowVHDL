ArrowVHDL
=========

This software is intended to help a developer designing electronic
circuits by describing them with arrows. The arrow notation represents
the according. From the netlist the developer can generate various
other formats by compiling the arrow into them. With this software
three basic compilers are shipped. One generates a simple textual
representation that helps debugging the actual circuit. Another
generates VHDL representations of the circuit. The third one generates
DOT syntax for visualization of circuits.

# Setup #

The Software is available of various locations. The source is downloadable via [http://hackage.haskell.org/package/ArrowVHDL-1.1/ArrowVHDL-1.1.tar.gz](http://hackage.haskell.org/package/ArrowVHDL-1.1/ArrowVHDL-1.1.tar.gz)
It's git archive can be cloned with the following command:

    > git clone https://github.com/frosch03/arrowVHDL.git

Another way of installing this software is via haskell's package
management system cabal:

    > cabal update
    > cabal install ArrowVHDL

This software is intended too be used interactively within
ghci. Therefore no further setup is necessary.

# Basic Functionality #

There is a canon of functions to be used, while working with
netlists. These are

* `flatten` - With flatten one is able to extract atomic parts from a
  netlist. This function should be used with all more complex circuits
  to ensure the generation of meaningful netlists.
* `synthesize` - This function actually compiles the netlist arrow
  into one of the description languages. The result for complex
  netlists must then be feed into `flatten` to get a meaningful
  netlist.
* `simulate` - With simulate the callable part of the circuit arrow is
  extracted. To actually run a simulation of the circuit over a given
  input stream, the `runStream` function is needed.
* `runStream` - This function must be used to create a function that
  operates over a list of values of the type the circuit arrow
  expects.

# Examples #

Load up the example file `Beispiel.hs` into an interactive ghci
session via:

    ghci Beispiel.hs
    Ok, modules loaded: ...
    Beispiel> 
    
You can then go through the examples defined within `Beispiel.hs`.

## Data Types  ##

At first one should have a look onto the type of the given example
circuit. 

    Beispiel> :t aTest0
    aTest0 :: (Arrow a, Num c) => Grid a c c

The circuit represents a "times two" circuit. Therefore it's type
represents that every number *c* is mapped onto another number
*c*. The circuit consists of a duplication part connected to an
addition circuit.


## Simulation ##

To simulate that circuit over a list of input values one has to call
`simulate` on aTest0 and pass that result into the `runStream`
function. The resulting function can than applied to a list of input
values:

    Beispiel> (runStream (simulate aTest0)) [1..10]
    [2,4,6,8,10,12,14,16,18,20]

As Haskell supports infinite lists, one could simulate the execution
of the defined circuit over an infinite number of input values. Of
course such a simulation would take forever. To actually simulate over
infinite input values one has only to supply the circuit with an
infinite list of Haskell:

    Beispiel> (runStream (simulate aTest0)) (repeat 23)
    [46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,...]

## Code Generation ##

To generate code from arrows one could use the supplied command
`synthesize`. This function takes an arrow into another representation
of that arrow. The actual supported variants could be graphviz's
dot-notation as well as vhdl code. The process of synthesizing is done
by

    Beispiel> (synthesize aShiftR5_XorKey)

According to the way, the ArrowVHDL library generates netlists from
the arrow description, the above instruction would only return the
last step of the combined netlist. To create a meaningful description
one needs to feed the result of the command into the `flatten`
function

    Beispiel> (flatten (synthesize aShiftR5_XorKey))

This will then return a complete netlist described within the selected
description method.

## Switching Describing Methods ##

For the lack of a better process, the switch of the describing method
must be done manually by changing the included **Show**
library. Therefore one must open the `Show.hs` file within the folder
`./System/ArrowVHDL/Circuit/`.

Within that file, there are three import statements for the different
show mechanisms; two of them commented. Uncomment the desired method
and comment the other two, like if one wants to output VHDL source,
the import statements should look like the following

    -- import System.ArrowVHDL.Circuit.Show.Simple
    import System.ArrowVHDL.Circuit.Show.VHDL
    -- import System.ArrowVHDL.Circuit.Show.DOT


After that, the ArrowVHDL library is again ready to use. It will now 
generate it's output within the selected format.

If there are for any reason more then one **Show** methods selected,
one will get an `Ambiguous occurrence` error:

    System/ArrowVHDL/Circuit/Show.hs:42:12:
    Ambiguous occurrence ‘showCircuit’
    It could refer to either ‘Simple.showCircuit’,
    imported from ‘System.ArrowVHDL.Circuit.Show....







questions: frosch03@gmail.com
