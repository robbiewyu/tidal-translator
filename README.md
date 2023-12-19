# Tidal Translator

Robert Yu
* Github: robbiewyu
* Penn ID: ryu02

Shyam Mehta
* Github: smehta23
* Penn ID: smehta1

## Module organization

In `src`, the following modules are present:
* [TidalToLily.hs](src/TidalToLily.hs) contains the core logic for converting TidalCycles
patterns to Lilypond expressions. 
* [LilypondPlus](src/LilypondPlus) contains the expanded Lilypond grammar to enable
drum notation. It is used by [TidalToLily](src/TidalToLily.hs) for Tidal to Lilypond conversion.
* [Parse](src/Parse.hs) contains the parser for TidalCycles patterns. It is adapted from
the original TidalCycles parser, with minor modifications.
* [TranslationStepper](src/TranslationStepper2) contains the main stepper (and associated logic), which
allows for realtime, interactive composition creation using TidalCycles patterns. As patterns
are entered, they are compiled to Lilypond. Once the composotion is complete, 
the full composition is written to a Lilypond file `stepper_output.ly`, and is converted
to PDF and MIDI files using the Lilypond compiler.
* [ParseConversionCmd](src/ParseConversionCmd.hs) is the parser for the stepper that allows for 
realtime, interactive composition creation as a command-line tool.

In `test`:
* [Spec](test/Spec.hs) is the entry point for the test suite.
* [TidalToLilyCheck](test/TidalToLilyCheck.hs) contains the roundtrip QuickCheck tests for 
Tidal to Lilypond. These tests arbitrarily generate very simple Tidal patterns and verify 
that the roundtrip conversion to Lilypond and back to Tidal is the same (using MIDI as an IR).
* [UnitTests](test/UnitTests.hs) contains the unit tests for the Tidal to Lilypond conversion. These
focus on the correctness of individual functions in [TidalToLily](src/TidalToLily.hs).
* [TidalParseTest](test/TidalParseTest.hs) contains the tests for the TidalCycles parser. These 
are adapted from the original TidalCycles tests, with minor modifications.

There are a few ways to run the project:
* _Using stepper_: `stack ghci` and then `stepper` to run the stepper. This will allow you to
interactively enter TidalCycles patterns, which will be converted to Lilypond and written to
`stepper_output.ly` once you are finished entering patterns (type `quit`). 
* _Using main_: Proceed with `stack run main`. This will allow you to enter a single pattern 
as a command-line argument, which will be converted to Lilypond and written to `output/output.ly`.

Once the Lilypond file is generated, a PDF containing the translated score and MIDI file
will automatically be generated in the `output` directory with the same name 
as the Lilypond file. 

<!-- Haskell packages typically divide their source code into three separate places:

  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
    If you do add new modules to this directory you should list them
    in the [cabal file](https://github.com/upenn-cis5520/empty-project/blob/74ad761562bb89d20e99621a76f607048a09f62e/project-cis5520.cabal#L44).
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs). -->

## Building, running, and testing

For roundtrip testing from Tidal -> Lilypond -> MIDI -> Tidal, the `midi_to_tidalcycles` library
is used (which is written in Python), which itself relies on `python-midi`. These repositories
are inside this directory. To set these up, `cd` into `python-midi` and first run 
`python setup.py install`. (During round-trip testing, all outputs involves in the process
(Lilypond, MIDI, and Tidal, after the roundtrip test is completed) are written to the `debug` 
directory.)

Once this is done, as before, you can compile the project with `stack build`, run the tests with
`stack test`, and start a REPL with `stack ghci`.

<!-- This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`.  -->

## Importing additional libraries

The Tidalcycles pattern language (Sound.Tidal) and Lilypond music expressions (Data.Music.Lilypond)
are imported, as are some of their downstream dependencies (e.g. prettify, vector-space, haskellish). 
These are all listed in the cabal file (and stack.yaml accordingly), so no additional 
libraries need to be imported; the `stack` commands above will take care of compilation, pulling 
these libraries to local when required.

Since this project significantly used Tidalcycles and Lilypond, but certain features of these 
libraries are not yet present, we adapted some of the code from the original libraries and
modified them to suit our needs. These are all in the `src` directory; in particular, the 
LilypondPlus directory contains expanded Lilypond expressions to enable drum notation. Since 
the parser for TidalCycles is not exposed, we also copied and made minor changes to their parser 
(see `src/Parse.hs`), as well their tests (see `test/TidalParseTest`).

As mentioned above, [python-midi](https://github.com/vishnubob/python-midi) and 
[midi_to_tidalcycles](https://github.com/TylerMclaughlin/midi_to_tidalcycles)
are also imported, but these are Python libraries used for roundtrip testing.



## Examples

You can sequentially enter the following commands into a stepper session.

"Brother John" canon
``` 
> d1 $ n "[c d e c]*2"
> advance
> d1 $ n "[e f g _]*2"
> d2 $ n "[c d e c]*2"
> advance
> d1 $ n "[g a g f e _ c _] * 2"
> d2 $ n "[e f g _]*2"
> d3 $ n "[c d e c]*2"
> advance
> d1 $ n "[c g4 c _] * 2"
> d2 $ n "[g a g f e _ c _] * 2"
> d3 $ n "[e f g _]*2"
> d4 $ n "[c d e c]*2"
> advance
> silence d4
> advance
> silence d3
> advance
> silence d2
> advance
> silence d1
> quit
```
Random Pattern
```
> d1 $ n "[c | e | g | b]"
```
Pattern with time manipulation
```
> d1 $ every 3 (hurry 2) $ sound "bd sn [~ bd] [sn bd*2]"
```
A special song that showcases most capabilities
```
> d1 $ n "<[e _ _ e _ _ d _ _ _ _ _ g4 a4 c a4] [d _ _ d _ _ c _ _ _ _ _ g4 a4 c a4] [c _ _ d _ _ b4 _ _ a4 g4 _ _ _ g4 _] [d c _ [g4 a4 c a4]]>"
> d2 $ n "<[[c4, f4, a4] [b3, d4, g4]] [[b3, e4, g4] [a3, c4, e4]] [[c4, f4, a4] [b3, d4, g4]] [[d4, e4, a4] [c4, e4, a4]]>"
> d3 $ s "[bd hh sn hh] * 2"
> quit
```