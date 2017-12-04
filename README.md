bisect-binary
=============

This little program helps with the following task:

You need to analyse some binary file (e.g., the firmware for some
microcontroller). You want to understand how it does certain things, but the
file is big. One approach to help you in the process is to erase parts of the file
(by overwriting it with binary zeros) and see if the file still does what it
should be doing. If it does, then you know that the interesting parts are in
another part of the file.

`bisect-binary` assists in this process by doing the book-keeping and zeroing
out parts of the file.


Screenshot
----------

The following screenshot shows `bisect-binary` in action, as it determins which
part of a “Hello World” program written in Haskell are actually needed:
It is taken from a [Asciinema demo], and edited to fit your screen.

```
$ bisect-binary -i hello -o hello-test -c 'chmod +x hello-test; timeout -k2s 2s ./hello-test
…
12.7% zeroes   131072B new   897000B left  [⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠛⠛⠛⠛⠛⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⣠⣤⣤⣤⣤⣤⣤⡄] [YNRUQ?] n
Segmentation fault
12.7% zeroes   131072B new   897000B left  [⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠛⠛⠛⠛⠛⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣤⣤⣤⣤⣤⣤⡄] [YNRUQ?] n
hello-test: internal error: stg_ap_p_ret
(GHC version 8.0.2 for x86_64_unknown_linux)
Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
Aborted
12.7% zeroes   131072B new   897000B left  [⠀⠀⠀⠀⠀⠈⠛⠛⠛⠛⠛⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣤⣤⣤⣤⣤⣤⡄] [YNRUQ?] n
Hello World!
12.7% zeroes    65536B new   897000B left  [⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠛⣻⣿⣥⣤⣤⣤⣤⡄] [YNRUQ?] y
Hello World!
15.9% zeroes    65536B new   864232B left  [⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠛⠛⢋⣤⣤⣤⣤⣤⣤⣤⣤⡄] [YNRUQ?] y
```

[Asciinema demo]: https://asciinema.org/a/7FcXeDU1BTa0SvynqSLezuaQ0?speed=2

Installation
------------

Install Haskell, and run `cabal install bisect-binary`. You will find the
binary in `~/.cabal/bin/bisect-binary`, and may want to move that to your
`$PATH`.

Usage
-----

Run `bisect-binary` with the input file, the output file, and optionally a
command to run on every try (which could, for example, flash the
microcontroller with the binary):

```
$ bisect-binary --help
Binary file bisector

Usage: bisect-binary (-i|--input FILE) (-o|--output FILE) [-c|--command COMMAND]
  Fills a file with as much zeroes as possible

Available options:
  -h,--help                Show this help text
  -i,--input FILE          input file
  -o,--output FILE         output file
  -c,--command COMMAND     command to run
```

Now `bisect-binary` will zero out parts of the file and ask you if the result is
now good or bad, as seen in the screenshot above. Your options are:

 * `Y`: Mark this example as good. It will remember which portions it zeroed out and from now
        on only add to it
 * `N`: Mark this example as bad. It will revert to the previous good version and try out
        other parts of the file.
 * `R`: Re-run the command.
 * `U`: Undo the last marking.
 * `Q`: Quit the program.

Upon quitting, `bisect-binary` will leave `output` in the last known state.

A persisent file name `output-file.bisect.log` is kept, so you can quit and
restart at any time without losing your work.

The status line tells you:
 * how much of the file could be zeroed out sucessfully.
 * how many bytes it is now adding to that.
 * how many bytes of the file may be relevant for your task
 * a status bar describing the progress:

   * The top row visualizes the part of the file that we know we can zero out for sure.
   * The bottom row visualizes the part that we are zeroing out next.

   Two dots indicate _all_ bytes in this part, one dot indicates _some_ bytes.

Contact
-------

Please reports bugs and missing features at the [GitHub bugtracker]. This is
also where you can find the [source code].

`bisect-binary` was written by [Joachim Breitner] and is licensed under a permissive MIT
[license].

[GitHub bugtracker]: https://github.com/nomeata/bisect-binary/issues
[source code]: https://github.com/nomeata/bisect-binary
[Joachim Breitner]: http://www.joachim-breitner.de/
[license]: https://github.com/nomeata/gipeda/blob/LICENSE

