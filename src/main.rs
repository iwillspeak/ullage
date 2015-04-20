#![cfg_attr(test, feature(test))]
#![feature(plugin,collections,str_char)]
#![plugin(peg_syntax_ext)]

peg_file! parser("grammar.peg"); // Fix " " .

#[cfg(not(test))]
fn dump_parse(s: &str) -> Result<(),parser::ParseError> {
    let words = try!(parser::words(s));
    println!("{} = [{}]", s, words.connect(", "));
    Ok(())
}

#[cfg(not(test))]
fn main() {
    dump_parse("hello world").unwrap();
}

#[cfg(test)]
mod test {
    extern crate test;

    use self::test::Bencher;

    use super::parser::*;

    #[test]
    fn test_comments() {
        let parsed = words("// this is a comment").unwrap();
        assert_eq!(0, parsed.len());
    }

    #[test]
    fn test_single_word() {
        let parsed = words("w").unwrap();
        assert_eq!("w", parsed[0]);
    }

    #[test]
    fn test_multiple_words() {
        let parsed = words("some words to parse").unwrap();
        assert_eq!("some", parsed[0]);
        assert_eq!("words", parsed[1]);
        assert_eq!("to", parsed[2]);
        assert_eq!("parse", parsed[3]);
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            words("Nulla facilisiFusce ut euismod urna ut bibendum nibh
Nam leo urna suscipit sed lorem non vestibulum mattis mauris
Pellentesque commodo porta nulla
//Phasellus fringilla metus metus nec elementum risus euismod at
Phasellus porttitor felis risus et scelerisque enim fringilla in
Nullam vel vehicula nibh
Quisque pellentesque fringilla ante non porta
Donec neque diam luctus auctor ante in consectetur feugiat ante
Donec malesuada ante in tellus porttitor at imperdiet lorem pulvinar
//Nullam vitae purus finibus tristique ante ut dapibus velit
//In eget mollis sapien sed accumsan velit
Curabitur aliquet sapien a turpis lobortis dapibus a ut dolor
//Ut accumsan ligula risus vel sodales lacus sollicitudin eu.
//
").unwrap();
        });
    }
}
