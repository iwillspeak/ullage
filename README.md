# Ullage

Ullage started as an attempt to implement a top-down operator precedence parser in Rust. It has since started developing into a small language. Feel free to have a poke around but don't expect much to work at the moment.

## Current Status

It is now possible to write simple programs. The following program computes 9 factorial:

    let n = 1
    let i = 9
	while i
	  n = i * n
	  i = i - 1
	end
	n

The REPL creates modules on a per-line basis though. To get that to
work you'll need to remove the line breaks:

    let n = 1 let i = 9 while i n = i * n i = i -1 end n

## License

Ullage is open source, under the [MIT License](LICENSE.md).
