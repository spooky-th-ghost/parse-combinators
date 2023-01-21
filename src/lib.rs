// #[test]
// fn literal_parser() {
//     let parse_joe = match_literal("Hello Joe!");
//     assert_eq!(Ok(("", ())), parse_joe("Hello Joe!"));

//     assert_eq!(
//         Ok((" Hello Robert!", ())),
//         parse_joe("Hello Joe! Hello Robert!")
//     );

//     assert_eq!(Err("Hello Mike!"), parse_joe("Hello Mike!"))
// }

use std::ops::RangeBounds;

// This function takes an input string slice, and will only succeed if the first character is 'a'
fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    // turn the input into an iterator over characters
    match input.chars().next() {
        //if iter.next() is Some('a'), aka the first character was 'a' return Ok(<the rest of the
        //string after 'a'>, ())
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        // If anything but 'a' is found, return our error type, which in this case is just the
        // input wrapped in Err()
        _ => Err(input),
    }
}

// Lamdba definition, returns a function that follows the same blueprint as `the_letter_a'
fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    //this match is grabbing a slice from the beginning of `input` through the length of the string
    //it expects, so `next()` for our iter will be a chunk of text equal in length to our expected
    //string
    move |input: &'a str| match input.get(0..expected.len()) {
        // The closure here matches the return type, so this is what gets returned, calling
        // `match_literal("a")` would give return an identical function to `the_letter_a`
        //
        // this is saying "If the next character is our expected string, match against it and
        // return the happy path
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

// True parser blueprint
// Fn(Input) -> Result<(Input, Output), Error>
//
// Fn(&str) -> Result<(&str, Element), &str>

fn identifier(input: &str) -> ParseResult<String> {
    //String to contain all the characters that we match
    let mut matched = String::new();

    //iterator of the characters in our string
    let mut chars = input.chars();

    //Grab the first character
    match chars.next() {
        //if the first char is alphabetical, push it into our matched string
        Some(next) if next.is_alphabetic() => matched.push(next),
        //if the first char is non alphabetical return an error early
        _ => return Err(input),
    }

    // As long as there are characters left in the string, keeping grabbing the next one
    while let Some(next) = chars.next() {
        // if the next character is alphanumeric or a `-` push it into our matched string
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            //if not break the loop and stop evaluating the next character
            break;
        }
    }
    // get the length of what we did successfully matched to find the proper index to return the
    // leftover input from
    let next_index = matched.len();
    //return the leftover parts of the string, as well as what we matched
    Ok((&input[next_index..], matched))
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "i-am-an-identifier".to_string())),
        identifier("i-am-an-identifier")
    );

    assert_eq!(
        Ok((" entirely an identifier", "not".to_string())),
        identifier("not entirely an identifier")
    );

    assert_eq!(
        Err("!not at all an identifier"),
        identifier("!not at all an identifier")
    );
}

fn old_pair<P1, P2, R1, R2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
where
    //This ensures that the types for the parsers we pass in to `pair` are functions with a
    //signature that takes in a string slice and return a result with the success value being a
    //tuple of (string slice, and whatever type the corresponding parser returns as it's success
    //type) and an error value of string slice
    P1: Fn(&str) -> Result<(&str, R1), &str>,
    P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    // closure signature for the function to return
    move |input| match parser1(input) {
        // if the provided input gives a succesful result, then use the slice returned by P1 as the
        // input for P2
        Ok((next_input, result1)) => match parser2(next_input) {
            // if P2 returns success, both parsers worked, and we can return the remaining slice
            // and the results of both parsers
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            //if P2 fails, throw an err
            Err(err) => Err(err),
        },
        //if P1 fails, throw an error
        Err(err) => Err(err),
    }
}

//#[test]
//fn pair_combinator() {
//    // this combinator is using two of our earlier defined functions, match_literal, which succeeds
//    // if the passed string starts with the literal we define, and identifier, which matches any
//    // a single alphabetical character, followed by any number of alphanumeric characters and `-`'s
//    let tag_opener = pair(match_literal("<"), identifier);

//    assert_eq!(
//        //this cobinator should grab the entirety of an xml element, and give us back the ending
//        // "/>"
//        Ok(("/>", ((), "my-first-element".to_string()))),
//        // note the first `()` being returned in our final result type is because `match_literal`
//        // just returns a unit type if it's successfull
//        // so this output can be read as (leftover_string_after_parsing, (success type for `match
//        // literal`, success type for `identifier`))
//        tag_opener("<my-first-element/>")
//    );
//    // This should fail because our input does not start with the literal we want to match `<`
//    assert_eq!(Err("oops"), tag_opener("oops"));
//    // This should fail because the first character after `<` needs to be alphabetic
//    assert_eq!(Err("!oops"), tag_opener("<!oops"))
//}

// P: Our parser
// A: Return Type of P, (in a tuple alongside the remaining characters in the slice it parsed)
// B: Desired return value of the combinator
// F: Function that will be used to map P's result (A) into our desired return value (B)
fn old_map<P, F, A, B>(parser: P, map_fn: F) -> impl Fn(&str) -> Result<(&str, B), &str>
where
    P: Fn(&str) -> Result<(&str, A), &str>,
    F: Fn(A) -> B,
{
    move |input| parser(input).map(|(next_input, result)| (next_input, map_fn(result)))

    // The above pattern is called a functor and can be written more verbosely as:
    // ######################################################################
    // # move |input| match parser(input) {                                 #
    // #     Ok((next_input, result)) => Ok((next_input, map_fn(result))),  #
    // #     Err(err) => Err(err),                                          #
    // # }                                                                  #
    // ######################################################################
}

// Define a type alias for our oft used return value, this is the main usecase for a type alias,
// we're not creating a new type like with a struct, we are telling the compiler that the left hand
// side of the type alias is shorthand for the more verbose right hand side
// in this case it is a generic type that takes a lifetime, and an output type that will be wrapped
// in a result, the lifetime here is required because we're using a slice, this type will be called
// like so:
//      ParseResult<String>
// the lifetime here is just telling the compiler that our ParseResult should be around at least as
// long as the intput string
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

// Trait for parsers
trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    // converts map to a method
    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

// Implement this trait for any function in scope that matches the signature of a string parser,
// using a blanket implementation

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", "my-first-element".to_string())),
        tag_opener.parse("<my-first-element/>")
    );

    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

// When you hit a character, return it as a success, if not return an error
fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

// This combinator takes a parser and a predicate function (fancy name for a function that either
// returns true or false, simmilar to .some and .none in JS), the parser will parse
// the input string, if the predicate evaluates to true, return the result, if not, throw an error
fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
}

fn whitspace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitspace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitspace_char())
}

// Most complex parser so far, going to try to document before reading on
fn quoted_string<'a>() -> impl Parser<'a, String> {
    // The right combinator essentially swallows the input of our left hand parser and just
    // returns the value of the right hand parser
    right(
        // look for 1 double quote, macth literal returns `()` on success along side the input
        // string so we use the right combinator to discard that result
        match_literal("\""),
        // The left parser does the opposite of right and returns the left hand parsers input
        // in this case it's `zero_or_more` which returns everything it captures as a result
        left(
                // Combining zero or more with pred allows us to tell zero or more to keep parsing
                // input until it hit's a double quote
                //
                // If this was going to be used more it could honestly be it's own parser expressed
                // as `Give me the next 0..infinity characters in the input, and stop when we see a
                // double quote`
                zero_or_more(any_char.pred(|c| *c != '"')),
                // once we have a double quote we step out of `zero_or_more` and use `match_literal` to ensure that we do in fact have a double quote
                match_literal("\""),
            ).
        // Because of the combination of the `right` and `left` combinators we only return the
        // result of zero or more, this map func takes `chars` as the output of `zero_or_more` and
        // returns the vec of `char`s we found inbetween the quotes with `zero_or_more`
        map(|chars| chars.into_iter().collect()),
    )
}

#[test]
fn quoted_string_parser() {
    assert_eq!(
        Ok(("", "Hello Joe!".to_string())),
        quoted_string().parse("\"Hello Joe!\"")
    );
}

//This parses an attribute pair, so given the text `some_attribute="some value"` we would expect it
//to return (remaining_input("some_attribute", "some value"))
fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

// Find all of the attributes inside of an XMLlike object
fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    // Find zero or more chunks of 1..infinity whitespace, and then return the attribute pairs in a
    // vec
    zero_or_more(right(space1(), attribute_pair()))
}

#[test]
fn attribute_parser() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )),
        attributes().parse("     one=\"1\" two=\"2\"")
    );
}

// will go from `<` through to the elements name, and then parse all of it's attributes, returning
// the element name and it's contained attribute pairs
fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

// Parse a single element (assuming it has no nested children) and return it as our Element Type
fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "float".to_string())],
                children: vec![]
            }
        )),
        single_element().parse("<div class=\"float\"/>")
    )
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

// As the name suggested try and use the left hand parser, if it throws an error, attempt the
// second parser
fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    // first match on parser 1
    move |input| match parser1.parse(input) {
        // if parser 1 was successful return it's result
        ok @ Ok(_) => ok,
        // if parser 1 failed then try parser 2
        Err(_) => parser2.parse(input),
    }
}

fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        left(zero_or_more(element()), close_element(el.name.clone())).map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}
#[test]
fn xml_parser() {
    let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
    let parsed_doc = Element {
        name: "top".to_string(),
        attributes: vec![("label".to_string(), "Top".to_string())],
        children: vec![
            Element {
                name: "semi-bottom".to_string(),
                attributes: vec![("label".to_string(), "Bottom".to_string())],
                children: vec![],
            },
            Element {
                name: "middle".to_string(),
                attributes: vec![],
                children: vec![Element {
                    name: "bottom".to_string(),
                    attributes: vec![("label".to_string(), "Another bottom".to_string())],
                    children: vec![],
                }],
            },
        ],
    };
    assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
}

#[test]
fn mismatched_closing_tag() {
    let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
    assert_eq!(Err("</middle>"), element().parse(doc));
}
