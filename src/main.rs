#![feature(plugin,collections,str_char)]
#![plugin(peg_syntax_ext)]

peg! parser(r#"
// entry tokens
#[pub]
words -> Vec<String>
    = word *

// Main tokens
word -> String
    = sp w:word_chars sp { w }

word_chars -> String
    = [a-zA-z]+ { match_str.to_string() }

// whitepsace handling
sp
    = (space_char / comment)*
space_char
    = [ \t\u{a0}\u{feff}\u{1680}\u{180e}\u{2000}-\u{200a}\u{202f}\u{205f}\u{3000}]
comment
    = "//" [^\n]*

"#); // Fix " " .

fn join_words(words: Vec<String>) -> String {
    let mut ret = String::new();
    let mut comma = false;

    for word in words {
        if comma {
            ret.push_str(", ");
        }
        ret.push_str(&word);
        comma = true;
    }

    ret
}

fn dump_parse(s: &str) -> Result<(),parser::ParseError> {
    let words = try!(parser::words(s));
    println!("{} = [{}]", s, join_words(words));
    Ok(())
}

fn main() {
    dump_parse("hello world").unwrap();
}
