# Ullage

Ullage started as an attempt to implement a top-down operator precedence parser in Rust. It has since started developing into a small language. Feel free to have a poke around but don't expect much to work at the moment.

## Current Status

It is now possible to write simple programs. The following program computes 9 factorial:

    fn fact(n: Number): Number
        var acc = 1
        var i = 1
        while i < n
            acc = acc * i
            i = i + 1
        end
        acc
    end
    
    print fact(9) # => 362880

## License

Ullage is open source, under the [MIT License](LICENSE.md).
