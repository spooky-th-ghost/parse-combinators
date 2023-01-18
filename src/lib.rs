#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    assert_eq!(Ok(("", ())), parse_joe("Hello Joe!"));

    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe("Hello Joe! Hello Robert!")
    );

    assert_eq!(Err("Hello Mike!"), parse_joe("Hello Mike!"))
}

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
