use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1, take_while_m_n},
    character::complete::{line_ending, one_of, space0},
    combinator::{map, not, opt, peek, recognize},
    multi::{many0, many1, many_m_n},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum TomlValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Datetime(String), // TODO: DateTime 型にする
    Array(Vec<TomlValue>),
    Table(HashMap<String, TomlValue>),
}

/// toml = expression *( newline expression )
pub fn parse_toml(input: &str) -> IResult<&str, HashMap<String, TomlValue>> {
    let (input, kv) = parse_expression(input)?;
    let (input, vec_kv) = many0(preceded(parse_newline, parse_expression))(input)?;

    // HashMapに変換して返す
    let mut output = HashMap::new();
    if let Some(kv) = kv {
        output.insert(kv.0, kv.1);
    }
    for kv in vec_kv.into_iter().flatten() {
        output.insert(kv.0, kv.1);
    }
    Ok((input, output))
}

/// expression =  ws [ comment ]
/// expression =/ ws keyval ws [ comment ]
/// expression =/ ws table ws [ comment ]
fn parse_expression(input: &str) -> IResult<&str, Option<(String, TomlValue)>> {
    let (input, _) = parse_ws(input)?;
    let r_keyval = parse_keyval(input);
    let r_table = parse_table(input);
    let (input, kv) = match (r_keyval, r_table) {
        (Ok((input, kv)), _) => {
            let (input, _) = parse_ws(input)?;
            (input, Some(kv))
        }
        (_, Ok((input, table))) => {
            let (input, _) = parse_ws(input)?;
            (input, Some(table))
        }
        _ => (input, None),
    };
    let (input, _) = opt(parse_comment)(input)?;
    Ok((input, kv))
}

/// ws = *wschar
/// wschar =  %x20  ; Space
/// wschar =/ %x09  ; Horizontal tab
fn parse_ws(input: &str) -> IResult<&str, &str> {
    space0(input)
}

/// newline =  %x0A     ; LF
/// newline =/ %x0D.0A  ; CRLF
fn parse_newline(input: &str) -> IResult<&str, &str> {
    line_ending(input)
}

/// comment = comment-start-symbol *allowed-comment-char
/// comment-start-symbol = %x23 ; #
/// allowed-comment-char = %x01-09 / %x0E-7F / non-ascii
/// non-ascii = %x80-D7FF / %xE000-10FFFF
fn parse_comment(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        tag("#"),
        take_while(
            |c| matches!(c as u32, 0x01..=0x09 | 0x0E..=0x7F | 0x80..=0xD7FF | 0xE000..=0x10FFFF),
        ),
    )))(input)
}

/// keyval = key keyval-sep val
fn parse_keyval(input: &str) -> IResult<&str, (String, TomlValue)> {
    // ToDo: keyがsimple-keyかdotted-keyかによって処理を分ける必要がある
    let (input, key) = parse_key(input)?;
    let (input, _) = parse_keyval_sep(input)?;
    let (input, val) = parse_val(input)?;
    Ok((input, (key, val)))
}

// keyval-sep = ws %x3D ws ; =
fn parse_keyval_sep(input: &str) -> IResult<&str, &str> {
    recognize(tuple((parse_ws, tag("="), parse_ws)))(input)
}

/// key = simple-key / dotted-key
fn parse_key(input: &str) -> IResult<&str, String> {
    alt((parse_dotted_key, parse_simple_key))(input)
}

/// val = string / boolean / array / inline-table / date-time / float / integer
fn parse_val(input: &str) -> IResult<&str, TomlValue> {
    alt((
        parse_string,
        parse_boolean,
        parse_array,
        parse_inline_table,
        parse_date_time,
        parse_float,
        parse_integer,
    ))(input)
}

/// simple-key = quoted-key / unquoted-key
fn parse_simple_key(input: &str) -> IResult<&str, String> {
    alt((parse_quoted_key, parse_unquoted_key))(input)
}

/// unquoted-key = 1*unquoted-key-char
fn parse_unquoted_key(input: &str) -> IResult<&str, String> {
    map(take_while1(is_unquoted_key_char), &str::to_string)(input)
}

/// unquoted-key-char = ALPHA / DIGIT / %x2D / %x5F         ; a-z A-Z 0-9 - _
/// unquoted-key-char =/ %xB2 / %xB3 / %xB9 / %xBC-BE       ; superscript digits, fractions
/// unquoted-key-char =/ %xC0-D6 / %xD8-F6 / %xF8-37D       ; non-symbol chars in Latin block
/// unquoted-key-char =/ %x37F-1FFF                         ; exclude GREEK QUESTION MARK, which is basically a semi-colon
/// unquoted-key-char =/ %x200C-200D / %x203F-2040          ; from General Punctuation Block, include the two tie symbols and ZWNJ, ZWJ
/// unquoted-key-char =/ %x2070-218F / %x2460-24FF          ; include super-/subscripts, letterlike/numberlike forms, enclosed alphanumerics
/// unquoted-key-char =/ %x2C00-2FEF / %x3001-D7FF          ; skip arrows, math, box drawing etc, skip 2FF0-3000 ideographic up/down markers and spaces
/// unquoted-key-char =/ %xF900-FDCF / %xFDF0-FFFD          ; skip D800-DFFF surrogate block, E000-F8FF Private Use area, FDD0-FDEF intended for process-internal use (unicode)
/// unquoted-key-char =/ %x10000-EFFFF                      ; all chars outside BMP range, excluding Private Use planes (F0000-10FFFF)
fn is_unquoted_key_char(c: char) -> bool {
    matches!(
        c as u32,
        0x61..=0x7A | 0x41..=0x5A | 0x30..=0x39 | 0x2D | 0x5F | // a-z A-Z 0-9 - _
        0xB2 | 0xB3 | 0xB9 | 0xBC..=0xBE | // superscript digits, fractions
        0xC0..=0xD6 | 0xD8..=0xF6 | 0xF8..=0x37D | // non-symbol chars in Latin block
        0x37F..=0x1FFF | // exclude GREEK QUESTION MARK, which is basically a semi-colon
        0x200C..=0x200D | 0x203F..=0x2040 | // from General Punctuation Block, include the two tie symbols and ZWNJ, ZWJ
        0x2070..=0x218F | 0x2460..=0x24FF | // include super-/subscripts, letterlike/numberlike forms, enclosed alphanumerics
        0x2C00..=0x2FEF | 0x3001..=0xD7FF | // skip arrows, math, box drawing etc, skip 2FF0-3000 ideographic up/down markers and spaces
        0xF900..=0xFDCF | 0xFDF0..=0xFFFD | // skip D800-DFFF surrogate block, E000-F8FF Private Use area, FDD0-FDEF intended for process-internal use (unicode)
        0x10000..=0xEFFFF // all chars outside BMP range, excluding Private Use planes (F0000-10FFFF)
    )
}

/// quoted-key = basic-string / literal-string
fn parse_quoted_key(input: &str) -> IResult<&str, String> {
    alt((parse_basic_string, parse_literal_string))(input)
}

/// dotted-key = simple-key 1*( dot-sep simple-key )
fn parse_dotted_key(input: &str) -> IResult<&str, String> {
    map(recognize(tuple((
        parse_simple_key,
        many1(tuple((parse_dot_sep, parse_simple_key))),
    ))), &str::to_string)(input)
}

// dot-sep   = ws %x2E ws  ; . Period
fn parse_dot_sep(input: &str) -> IResult<&str, &str> {
    recognize(tuple((parse_ws, tag("."), parse_ws)))(input)
}

/// string = ml-basic-string / basic-string / ml-literal-string / literal-string
fn parse_string(input: &str) -> IResult<&str, TomlValue> {
    let (input, s) = alt((
        parse_ml_basic_string,
        parse_ml_literal_string,
        parse_basic_string,
        parse_literal_string,
    ))(input)?;
    Ok((input, TomlValue::String(s)))
}

/// basic-string = quotation-mark *basic-char quotation-mark
// quotation-mark = %x22            ; "
fn parse_basic_string(input: &str) -> IResult<&str, String> {
    map(
        delimited(
            tag("\""),
            recognize(many0(parse_basic_char)),
            tag("\"")
        ),
        &str::to_string
    )(input)
}

/// basic-char = basic-unescaped / escaped
fn parse_basic_char(input: &str) -> IResult<&str, &str> {
    alt((parse_basic_unescaped, parse_escaped))(input)
}

/// basic-unescaped = wschar / %x21 / %x23-5B / %x5D-7E / non-ascii
fn parse_basic_unescaped(input: &str) -> IResult<&str, &str> {
    fn is_basic_unescaped(c: char) -> bool {
        matches!(
            c as u32,
            0x09 | 0x20..=0x21 | 0x23..=0x5B | 0x5D..=0x7E | 0x80..=0xD7FF | 0xE000..=0x10FFFF
        )
    }
    take_while_m_n(1, 1, is_basic_unescaped)(input)
}

/// escaped = escape escape-seq-char
// escape = %x5C                   ; \
// escape-seq-char =  %x22         ; "    quotation mark  U+0022
// escape-seq-char =/ %x5C         ; \    reverse solidus U+005C
// escape-seq-char =/ %x62         ; b    backspace       U+0008
// escape-seq-char =/ %x65         ; e    escape          U+001B
// escape-seq-char =/ %x66         ; f    form feed       U+000C
// escape-seq-char =/ %x6E         ; n    line feed       U+000A
// escape-seq-char =/ %x72         ; r    carriage return U+000D
// escape-seq-char =/ %x74         ; t    tab             U+0009
// escape-seq-char =/ %x78 2HEXDIG ; xHH                  U+00HH
// escape-seq-char =/ %x75 4HEXDIG ; uHHHH                U+HHHH
// escape-seq-char =/ %x55 8HEXDIG ; UHHHHHHHH            U+HHHHHHHH
fn parse_escaped(input: &str) -> IResult<&str, &str> {
    alt((
        tag("\\\""),
        tag("\\\\"),
        tag("\\b"),
        tag("\\e"),
        tag("\\f"),
        tag("\\n"),
        tag("\\r"),
        tag("\\t"),
        recognize(tuple((
            tag("\\x"),
            many_m_n(2, 2, one_of("0123456789abcdefABCDEF")),
        ))),
        recognize(tuple((
            tag("\\u"),
            many_m_n(4, 4, one_of("0123456789abcdefABCDEF")),
        ))),
        recognize(tuple((
            tag("\\U"),
            many_m_n(8, 8, one_of("0123456789abcdefABCDEF")),
        ))),
    ))(input)
}

/// ml-basic-string = ml-basic-string-delim [ newline ] ml-basic-body
///                   ml-basic-string-delim
/// ml-basic-string-delim = 3quotation-mark
pub fn parse_ml_basic_string(input: &str) -> IResult<&str, String> {
    delimited(
        tuple((tag("\"\"\""), opt(parse_newline))),
        parse_ml_basic_body,
        tag("\"\"\""),
    )(input)
}

/// ml-basic-body = *mlb-content *( mlb-quotes 1*mlb-content ) [ mlb-quotes ]
fn parse_ml_basic_body(input: &str) -> IResult<&str, String> {
    let (input, content) = many0(parse_mlb_content)(input)?;
    let (input, quotes_content) = many0(tuple((parse_mlb_quotes, many1(parse_mlb_content))))(input)?;
    let (input, quotes) = opt(parse_mlb_quotes)(input)?;

    let mut s = content.join("");
    let s2: String = quotes_content
        .iter()
        .map(|(quotes, content)| quotes.to_string() + &content.join(""))
        .collect();
    s += &s2;
    if let Some(quotes) = quotes {
        s += quotes;
    }
    Ok((input, s))
}

/// mlb-quotes = 1*2quotation-mark
fn parse_mlb_quotes(input: &str) -> IResult<&str, &str> {
    alt((
        recognize(tuple((tag("\""), not(tag("\""))))),
        recognize(tuple((tag("\"\""), not(tag("\""))))),
        recognize(tuple((tag("\""), peek(tag("\"\"\""))))),
        recognize(tuple((tag("\"\""), peek(tag("\"\"\""))))),
    ))(input)
}

/// mlb-content = basic-char / newline / mlb-escaped-nl
/// mlb-escaped-nl = escape ws newline *( wschar / newline )
pub fn parse_mlb_content(input: &str) -> IResult<&str, &str> {
    let result = alt((parse_basic_char, parse_newline))(input);
    if result.is_ok() {
        return result;
    }
    let result = tuple((
        tag("\\"),
        parse_ws,
        parse_newline,
        many0(alt((tag(" "), tag("\t"), parse_newline)))
    ))(input);
    match result {
        Ok((input, _)) => Ok((input, "")),
        Err(e) => Err(e),
    }
}

/// literal-string = apostrophe *literal-char apostrophe
///
/// apostrophe = %x27 ; ' apostrophe
fn parse_literal_string(input: &str) -> IResult<&str, String> {
    map(
        delimited(
            tag("'"),
            take_while(is_literal_char),
            tag("'")
        ),
        &str::to_string
    )(input)
}

/// literal-char = %x09 / %x20-26 / %x28-7E / non-ascii
fn is_literal_char(c: char) -> bool {
    matches!(
        c as u32,
        0x09 | 0x20..=0x26 | 0x28..=0x7E | 0x80..=0xD7FF | 0xE000..=0x10FFFF
    )
}

/// ml-literal-string = ml-literal-string-delim [ newline ] ml-literal-body
///                     ml-literal-string-delim
/// ml-literal-string-delim = 3apostrophe
pub fn parse_ml_literal_string(input: &str) -> IResult<&str, String> {
    map(delimited(
        tuple((tag("'''"), opt(parse_newline))),
        parse_ml_literal_body,
        tag("'''"),
    ), &str::to_string)(input)
}

/// ml-literal-body = *mll-content *( mll-quotes 1*mll-content ) [ mll-quotes ]
fn parse_ml_literal_body(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        many0(parse_mll_content),
        many0(tuple((parse_mll_quotes, many1(parse_mll_content)))),
        opt(parse_mll_quotes),
    )))(input)
}

/// mll-content = literal-char / newline
pub fn parse_mll_content(input: &str) -> IResult<&str, &str> {
    alt((take_while_m_n(1, 1, is_literal_char), parse_newline))(input)
}

/// mll-quotes = 1*2apostrophe
pub fn parse_mll_quotes(input: &str) -> IResult<&str, &str> {
    alt((
        recognize(tuple((tag("'"), not(tag("'"))))),
        recognize(tuple((tag("''"), not(tag("'"))))),
        recognize(tuple((tag("'"), peek(tag("'''"))))),
        recognize(tuple((tag("''"), peek(tag("'''"))))),
    ))(input)
}

/// integer = dec-int / hex-int / oct-int / bin-int
///
/// minus = %x2D                       ; -
/// plus = %x2B                        ; +
/// underscore = %x5F                  ; _
/// digit1-9 = %x31-39                 ; 1-9
/// digit0-7 = %x30-37                 ; 0-7
/// digit0-1 = %x30-31                 ; 0-1
///
/// hex-prefix = %x30.78               ; 0x
/// oct-prefix = %x30.6F               ; 0o
/// bin-prefix = %x30.62               ; 0b
fn parse_integer(input: &str) -> IResult<&str, TomlValue> {
    let (input, n) = alt((
        parse_hex_int,
        parse_oct_int,
        parse_bin_int,
        parse_dec_int, // Memo: dec-intを最後にしないと、prefixの0がパースされてしまう
    ))(input)?;
    Ok((input, TomlValue::Integer(n)))
}

/// dec-int = [ minus / plus ] unsigned-dec-int
fn parse_dec_int(input: &str) -> IResult<&str, i64> {
    let (input, sign) = opt(alt((tag("-"), tag("+"))))(input)?;
    let (input, n) = parse_unsigned_dec_int(input)?;
    Ok((
        input,
        match sign {
            Some("-") => -n,
            _ => n,
        },
    ))
}

/// unsigned-dec-int = DIGIT / digit1-9 1*( DIGIT / underscore DIGIT )
fn parse_unsigned_dec_int(input: &str) -> IResult<&str, i64> {
    let parser = alt((
        recognize(tuple((
            one_of("123456789"),
            many1(alt((
                one_of("0123456789"),
                preceded(tag("_"), one_of("0123456789")),
            ))),
        ))),
        recognize(one_of("0123456789")),
    ));
    map(parser, |s: &str| s.replace('_', "").parse().unwrap())(input)
}

fn parse_xxx_int<'a>(
    input: &'a str,
    prefix: &str,
    digits: &str,
    radix: u32,
) -> IResult<&'a str, i64> {
    let (input, _) = tag(prefix)(input)?;
    let parser = tuple((
        one_of(digits),
        many0(alt((one_of(digits), preceded(tag("_"), one_of(digits))))),
    ));
    map(recognize(parser), |s: &str| {
        i64::from_str_radix(&s.replace('_', ""), radix).unwrap()
    })(input)
}

/// hex-int = hex-prefix HEXDIG *( HEXDIG / underscore HEXDIG )
fn parse_hex_int(input: &str) -> IResult<&str, i64> {
    parse_xxx_int(input, "0x", "0123456789ABCDEFabcdef", 16)
}

/// oct-int = oct-prefix digit0-7 *( digit0-7 / underscore digit0-7 )
fn parse_oct_int(input: &str) -> IResult<&str, i64> {
    parse_xxx_int(input, "0o", "01234567", 8)
}

/// bin-int = bin-prefix digit0-1 *( digit0-1 / underscore digit0-1 )
fn parse_bin_int(input: &str) -> IResult<&str, i64> {
    parse_xxx_int(input, "0b", "01", 2)
}

/// float = float-int-part ( exp / frac [ exp ] )
/// float =/ special-float
///
/// float-int-part = dec-int
fn parse_float(input: &str) -> IResult<&str, TomlValue> {
    if let Ok((input, x)) = parse_special_float(input) {
        return Ok((input, TomlValue::Float(x)));
    }
    let parser = tuple((
        parse_dec_int,
        alt((
            recognize(parse_exp),
            recognize(tuple((parse_frac, opt(parse_exp)))),
        )),
    ));
    map(recognize(parser), |s| {
        TomlValue::Float(s.replace('_', "").parse::<f64>().unwrap())
    })(input)
}

/// frac = decimal-point zero-prefixable-int
/// decimal-point = %x2E               ; .
fn parse_frac(input: &str) -> IResult<&str, i64> {
    let (input, _) = tag(".")(input)?;
    parse_zero_prefixable_int(input)
}

/// zero-prefixable-int = DIGIT *( DIGIT / underscore DIGIT )
fn parse_zero_prefixable_int(input: &str) -> IResult<&str, i64> {
    let parser = tuple((
        one_of("0123456789"),
        many0(alt((
            one_of("0123456789"),
            preceded(tag("_"), one_of("0123456789")),
        ))),
    ));
    map(recognize(parser), |s: &str| {
        s.replace('_', "").parse().unwrap()
    })(input)
}

/// exp = "e" float-exp-part
/// float-exp-part = [ minus / plus ] zero-prefixable-int
fn parse_exp(input: &str) -> IResult<&str, i64> {
    let (input, _) = one_of("Ee")(input)?;
    let (input, sign) = opt(alt((tag("-"), tag("+"))))(input)?;
    let (input, n) = parse_zero_prefixable_int(input)?;
    Ok((
        input,
        match sign {
            Some("-") => -n,
            _ => n,
        },
    ))
}

/// special-float = [ minus / plus ] ( inf / nan )
/// inf = %x69.6e.66  ; inf
/// nan = %x6e.61.6e  ; nan
fn parse_special_float(input: &str) -> IResult<&str, f64> {
    let (input, sign) = opt(alt((tag("-"), tag("+"))))(input)?;
    let (input, value) = alt((tag("inf"), tag("nan")))(input)?;
    Ok((
        input,
        match value {
            "inf" => match sign {
                Some("-") => f64::NEG_INFINITY,
                _ => f64::INFINITY,
            },
            "nan" => f64::NAN,
            _ => unreachable!(),
        },
    ))
}

/// boolean = true / false
/// true    = %x74.72.75.65     ; true
/// false   = %x66.61.6C.73.65  ; false
fn parse_boolean(input: &str) -> IResult<&str, TomlValue> {
    let (input, value) = alt((tag("true"), tag("false")))(input)?;
    Ok((input, TomlValue::Boolean(value == "true")))
}

/// date-time      = offset-date-time / local-date-time / local-date / local-time
fn parse_date_time(input: &str) -> IResult<&str, TomlValue> {
    map(
        alt((
            parse_offset_date_time,
            parse_local_date_time,
            parse_local_date,
            parse_local_time,
        )),
        |s| TomlValue::Datetime(s.to_string()),
    )(input)
}

/// date-fullyear  = 4DIGIT
/// date-month     = 2DIGIT  ; 01-12
/// date-mday      = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on month/year
/// time-delim     = "T" / %x20 ; T, t, or space
/// time-hour      = 2DIGIT  ; 00-23
/// time-minute    = 2DIGIT  ; 00-59
/// time-second    = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second rules
/// time-secfrac   = "." 1*DIGIT

/// partial-time   = time-hour ":" time-minute [ ":" time-second [ time-secfrac ] ]
fn parse_partial_time(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        many_m_n(2, 2, one_of("0123456789")),
        tag(":"),
        many_m_n(2, 2, one_of("0123456789")),
        opt(tuple((
            tag(":"),
            many_m_n(2, 2, one_of("0123456789")),
            opt(preceded(tag("."), many1(one_of("0123456789")))),
        ))),
    )))(input)
}

/// full-date      = date-fullyear "-" date-month "-" date-mday
fn parse_full_date(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        many_m_n(4, 4, one_of("0123456789")),
        tag("-"),
        many_m_n(2, 2, one_of("0123456789")),
        tag("-"),
        many_m_n(2, 2, one_of("0123456789")),
    )))(input)
}

/// full-time      = partial-time time-offset
fn parse_full_time(input: &str) -> IResult<&str, &str> {
    // time-numoffset = ( "+" / "-" ) time-hour ":" time-minute
    let parse_time_numoffset = recognize(tuple((
        one_of("+-"),
        many_m_n(2, 2, one_of("0123456789")),
        tag(":"),
        many_m_n(2, 2, one_of("0123456789")),
    )));

    // time-offset    = "Z" / time-numoffset
    let parse_time_offset = alt((tag_no_case("Z"), parse_time_numoffset));

    recognize(tuple((parse_partial_time, parse_time_offset)))(input)
}

/// offset-date-time = full-date time-delim full-time
fn parse_offset_date_time(input: &str) -> IResult<&str, &str> {
    recognize(tuple((parse_full_date, one_of("Tt "), parse_full_time)))(input)
}

/// local-date-time = full-date time-delim partial-time
fn parse_local_date_time(input: &str) -> IResult<&str, &str> {
    recognize(tuple((parse_full_date, one_of("Tt "), parse_partial_time)))(input)
}

/// local-date = full-date
fn parse_local_date(input: &str) -> IResult<&str, &str> {
    parse_full_date(input)
}

/// local-time = partial-time
fn parse_local_time(input: &str) -> IResult<&str, &str> {
    parse_partial_time(input)
}

/// array = array-open [ array-values ] ws-comment-newline array-close
///
/// array-open =  %x5B ; [
/// array-close = %x5D ; ]
pub fn parse_array(input: &str) -> IResult<&str, TomlValue> {
    map(
        delimited(
            tag("["),
            opt(parse_array_values),
            tuple((parse_ws_comment_newline, tag("]"))),
        ),
        |v| TomlValue::Array(v.unwrap_or_default()),
    )(input)
}

/// array-values =  ws-comment-newline val ws-comment-newline array-sep array-values
/// array-values =/ ws-comment-newline val ws-comment-newline [ array-sep ]
///
/// array-sep = %x2C  ; , Comma
fn parse_array_values(input: &str) -> IResult<&str, Vec<TomlValue>> {
    let parse_array_value = delimited(
        parse_ws_comment_newline,
        parse_val,
        tuple((parse_ws_comment_newline, opt(tag(",")))),
    );
    many1(parse_array_value)(input)
}

/// ws-comment-newline = *( wschar / [ comment ] newline )
fn parse_ws_comment_newline(input: &str) -> IResult<&str, &str> {
    recognize(
        many0(
            alt((
                recognize(one_of(" \t")),
                recognize(tuple((opt(parse_comment), parse_newline))),
            ))
        )
    )(input)
}


/// table = std-table / array-table
///
/// ;; Standard Table
///
/// std-table = std-table-open key std-table-close
///
/// std-table-open  = %x5B ws     ; [ Left square bracket
/// std-table-close = ws %x5D     ; ] Right square bracket
fn parse_table(input: &str) -> IResult<&str, (String, TomlValue)> {
    todo!()
}

/// inline-table = inline-table-open [ inline-table-keyvals ] ws-comment-newline inline-table-close
///
/// inline-table-open  = %x7B  ; {
/// inline-table-close = %x7D  ; }
/// inline-table-sep   = %x2C  ; , Comma
fn parse_inline_table(input: &str) -> IResult<&str, TomlValue> {
    map(
        delimited(
            tag("{"),
            opt(parse_inline_table_keyvals),
            tuple((parse_ws_comment_newline, tag("}"))),
        ),
        |t| TomlValue::Table(t.unwrap_or_default()),
    )(input)
}

/// inline-table-keyvals =  ws-comment-newline keyval ws-comment-newline inline-table-sep inline-table-keyvals
/// inline-table-keyvals =/ ws-comment-newline keyval ws-comment-newline [ inline-table-sep ]
fn parse_inline_table_keyvals(input: &str) -> IResult<&str, HashMap<String, TomlValue>> {
    let parse_inline_table_keyval = delimited(
        parse_ws_comment_newline,
        parse_keyval,
        tuple((parse_ws_comment_newline, opt(tag(",")))),
    );
    map(
        many1(parse_inline_table_keyval),
        |v| v.into_iter().collect(),
    )(input)
}

/// array-table = array-table-open key array-table-close
/// array-table-open  = %x5B.5B ws  ; [[ Double left square bracket
/// array-table-close = ws %x5D.5D  ; ]] Double right square bracket
fn parse_array_table(input: &str) -> IResult<&str, String> {
    let (input, _) = tag("[[")(input)?;
    let (input, _) = parse_ws(input)?;
    let (input, key) = parse_key(input)?;
    let (input, _) = parse_ws(input)?;
    let (input, _) = tag("]]")(input)?;
    Ok((input, key))
}

// ALPHA = %x41-5A / %x61-7A ; A-Z / a-z
// DIGIT = %x30-39 ; 0-9
// HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment() {
        assert_eq!(
            parse_comment("# この行は全てコメントです。"),
            Ok(("", "# この行は全てコメントです。"))
        );
        assert_eq!(
            parse_comment("# この行は全てコメントです。\n"),
            Ok(("\n", "# この行は全てコメントです。"))
        );
        assert_eq!(
            parse_comment("# この行は全てコメントです。\r\n"),
            Ok(("\r\n", "# この行は全てコメントです。"))
        );
    }

    #[test]
    fn test_key() {
        // unquoted-key
        assert_eq!(parse_key("key"), Ok(("", "key".to_string())));
        assert_eq!(parse_key("bare_key"), Ok(("", "bare_key".to_string())));
        assert_eq!(parse_key("bare-key"), Ok(("", "bare-key".to_string())));
        assert_eq!(parse_key("1234"), Ok(("", "1234".to_string())));

        // quoted-key
        assert_eq!(parse_key("\"127.0.0.1\""), Ok(("", "127.0.0.1".to_string())));
        assert_eq!(
            parse_key("\"character encoding\""),
            Ok(("", "character encoding".to_string()))
        );
        assert_eq!(parse_key("\"ʎǝʞ\""), Ok(("", "ʎǝʞ".to_string())));
        assert_eq!(parse_key("'key2'"), Ok(("", "key2".to_string())));
        assert_eq!(
            parse_key("'quoted \"value\"'"),
            Ok(("", "quoted \"value\"".to_string()))
        );
        assert_eq!(parse_key("\"\""), Ok(("", "".to_string())));
        assert_eq!(parse_key("''"), Ok(("", "".to_string())));

        // dotted-key
        assert_eq!(parse_key("physical.color"), Ok(("", "physical.color".to_string())));
        assert_eq!(parse_key("physical.shape"), Ok(("", "physical.shape".to_string())));
        assert_eq!(
            parse_key("site.\"google.com\""),
            Ok(("", "site.\"google.com\"".to_string()))
        );
    }

    #[test]
    fn test_basic_string() {
        assert_eq!(
            parse_string(
                "\"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.\""
            ),
            Ok((
                "",
                TomlValue::String(
                    "I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF."
                        .to_string()
                ),
            ))
        );
    }

    #[test]
    fn test_ml_basic_string() {
        assert_eq!(
            parse_string("\"\"\"\nRoses are red\nViolets are blue\"\"\""),
            Ok((
                "",
                TomlValue::String("Roses are red\nViolets are blue".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"\nThe quick brown \\\n\n\n  fox jumps over \\\n    the lazy dog.\"\"\""),
            Ok((
                "",
                TomlValue::String("The quick brown fox jumps over the lazy dog.".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"\\\n       The quick brown \\\n       fox jumps over \\\n       the lazy dog.\\\n       \"\"\""),
            Ok((
                "",
                TomlValue::String("The quick brown fox jumps over the lazy dog.".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"Here are two quotation marks: \"\". Simple enough.\"\"\""),
            Ok((
                "",
                TomlValue::String("Here are two quotation marks: \"\". Simple enough.".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"Here are three quotation marks: \"\"\\\".\"\"\""),
            Ok((
                "",
                TomlValue::String("Here are three quotation marks: \"\"\\\".".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"Here are fifteen quotation marks: \"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\".\"\"\""),
            Ok((
                "",
                TomlValue::String("Here are fifteen quotation marks: \"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\".".to_string()),
            ))
        );
        assert_eq!(
            parse_string("\"\"\"\"This,\" she said, \"is just a pointless statement.\"\"\"\""),
            Ok((
                "",
                TomlValue::String("\"This,\" she said, \"is just a pointless statement.\"".to_string()),
            ))
        );
    }

    #[test]
    fn test_literal_string() {
        assert_eq!(
            parse_string("'C:\\Users\\nodejs\\templates'"),
            Ok((
                "",
                TomlValue::String("C:\\Users\\nodejs\\templates".to_string())
            ))
        );
        assert_eq!(
            parse_string("'\\\\ServerX\\admin$\\system32\\'"),
            Ok((
                "",
                TomlValue::String("\\\\ServerX\\admin$\\system32\\".to_string())
            ))
        );
        assert_eq!(
            parse_string("'Tom \"Dubs\" Preston-Werner'"),
            Ok((
                "",
                TomlValue::String("Tom \"Dubs\" Preston-Werner".to_string())
            ))
        );
        assert_eq!(
            parse_string("'<\\i\\c*\\s*>'"),
            Ok((
                "",
                TomlValue::String("<\\i\\c*\\s*>".to_string())
            ))
        );
    }

    #[test]
    fn test_ml_literal_string() {
        assert_eq!(
            parse_string("'''I [dw]on't need \\d{2} apples'''"),
            Ok((
                "",
                TomlValue::String("I [dw]on't need \\d{2} apples".to_string())
            ))
        );
        assert_eq!(
            parse_string("'''\n文字列開始直後の改行は\n取り除かれます。\n    その他の空白、改行文字は\n\t保持されます。\n'''"),
            Ok((
                "",
                TomlValue::String("文字列開始直後の改行は\n取り除かれます。\n    その他の空白、改行文字は\n\t保持されます。\n".to_string()),
            ))
        );
        assert_eq!(
            parse_string("'''Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"'''"),
            Ok((
                "",
                TomlValue::String("Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"".to_string()),
            ))
        );
        assert_eq!(
            parse_string("''''That,' she said, 'is still pointless.''''"),
            Ok((
                "",
                TomlValue::String("'That,' she said, 'is still pointless.'".to_string()),
            ))
        );
    }

    #[test]
    fn test_parse_boolean() {
        assert_eq!(parse_boolean("true"), Ok(("", TomlValue::Boolean(true))));
        assert_eq!(parse_boolean("false"), Ok(("", TomlValue::Boolean(false))));
    }

    #[test]
    fn test_parse_integer() {
        // decimal int
        assert_eq!(parse_integer("+99"), Ok(("", TomlValue::Integer(99))));
        assert_eq!(parse_integer("42"), Ok(("", TomlValue::Integer(42))));
        assert_eq!(parse_integer("0"), Ok(("", TomlValue::Integer(0))));
        assert_eq!(parse_integer("-17"), Ok(("", TomlValue::Integer(-17))));
        assert_eq!(parse_integer("1_000"), Ok(("", TomlValue::Integer(1000))));
        assert_eq!(
            parse_integer("5_349_221"),
            Ok(("", TomlValue::Integer(5349221)))
        );
        assert_eq!(
            parse_integer("53_49_221"),
            Ok(("", TomlValue::Integer(5349221)))
        );
        assert_eq!(
            parse_integer("1_2_3_4_5"),
            Ok(("", TomlValue::Integer(12345)))
        );

        // hex int
        assert_eq!(parse_integer("0x0"), Ok(("", TomlValue::Integer(0))));
        assert_eq!(parse_integer("0x11"), Ok(("", TomlValue::Integer(17))));
        assert_eq!(parse_integer("0x1_1"), Ok(("", TomlValue::Integer(17))));
        assert_eq!(
            parse_integer("0xDEADBEEF"),
            Ok(("", TomlValue::Integer(3735928559)))
        );
        assert_eq!(
            parse_integer("0xdeadbeef"),
            Ok(("", TomlValue::Integer(3735928559)))
        );
        assert_eq!(
            parse_integer("0xdead_beef"),
            Ok(("", TomlValue::Integer(3735928559)))
        );

        // oct int
        assert_eq!(parse_integer("0o0"), Ok(("", TomlValue::Integer(0))));
        assert_eq!(parse_integer("0o11"), Ok(("", TomlValue::Integer(9))));
        assert_eq!(parse_integer("0o1_1"), Ok(("", TomlValue::Integer(9))));
        assert_eq!(
            parse_integer("0o1234567"),
            Ok(("", TomlValue::Integer(342391)))
        );
        assert_eq!(parse_integer("0o755"), Ok(("", TomlValue::Integer(493))));

        // bin int
        assert_eq!(parse_integer("0b0"), Ok(("", TomlValue::Integer(0))));
        assert_eq!(parse_integer("0b11"), Ok(("", TomlValue::Integer(3))));
        assert_eq!(parse_integer("0b1_1"), Ok(("", TomlValue::Integer(3))));
        assert_eq!(
            parse_integer("0b11010110"),
            Ok(("", TomlValue::Integer(214)))
        );
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(parse_float("+1.0"), Ok(("", TomlValue::Float(1.0))));
        assert_eq!(parse_float("3.1415"), Ok(("", TomlValue::Float(3.1415))));
        assert_eq!(parse_float("-0.01"), Ok(("", TomlValue::Float(-0.01))));
        assert_eq!(parse_float("5e+22"), Ok(("", TomlValue::Float(5e+22))));
        assert_eq!(parse_float("1e06"), Ok(("", TomlValue::Float(1e06))));
        assert_eq!(parse_float("-2E-2"), Ok(("", TomlValue::Float(-2E-2))));
        assert_eq!(
            parse_float("6.626e-34"),
            Ok(("", TomlValue::Float(6.626e-34)))
        );
        assert_eq!(
            parse_float("9_224_617.445_991_228_313"),
            Ok(("", TomlValue::Float(9224617.445991228313)))
        );

        // spacial float
        assert_eq!(
            parse_float("inf"),
            Ok(("", TomlValue::Float(std::f64::INFINITY)))
        );
        assert_eq!(
            parse_float("+inf"),
            Ok(("", TomlValue::Float(std::f64::INFINITY)))
        );
        assert_eq!(
            parse_float("-inf"),
            Ok(("", TomlValue::Float(std::f64::NEG_INFINITY)))
        );
        let r_nan = parse_float("nan");
        assert!(r_nan.is_ok());
        if let Ok((_, TomlValue::Float(f))) = r_nan {
            assert!(f.is_nan());
        }
        let r_nan = parse_float("+nan");
        assert!(r_nan.is_ok());
        if let Ok((_, TomlValue::Float(f))) = r_nan {
            assert!(f.is_nan());
        }
        let r_nan = parse_float("-nan");
        assert!(r_nan.is_ok());
        if let Ok((_, TomlValue::Float(f))) = r_nan {
            assert!(f.is_nan());
        }
    }

    #[test]
    fn test_parse_date_time() {
        // offset-date-time
        assert_eq!(
            parse_date_time("1979-05-27T07:32:00Z"),
            Ok(("", TomlValue::Datetime("1979-05-27T07:32:00Z".to_string())))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00-07:00"),
            Ok((
                "",
                TomlValue::Datetime("1979-05-27T00:32:00-07:00".to_string())
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00-07:00"),
            Ok((
                "",
                TomlValue::Datetime("1979-05-27T00:32:00-07:00".to_string())
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00.999999-07:00"),
            Ok((
                "",
                TomlValue::Datetime("1979-05-27T00:32:00.999999-07:00".to_string())
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27 07:32:00Z"),
            Ok(("", TomlValue::Datetime("1979-05-27 07:32:00Z".to_string())))
        );

        // local-date-time
        assert_eq!(
            parse_date_time("1979-05-27T07:32:00"),
            Ok(("", TomlValue::Datetime("1979-05-27T07:32:00".to_string())))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00.999999"),
            Ok((
                "",
                TomlValue::Datetime("1979-05-27T00:32:00.999999".to_string())
            ))
        );

        // local-date
        assert_eq!(
            parse_date_time("1979-05-27"),
            Ok(("", TomlValue::Datetime("1979-05-27".to_string())))
        );

        // local-time
        assert_eq!(
            parse_date_time("07:32:00"),
            Ok(("", TomlValue::Datetime("07:32:00".to_string())))
        );
        assert_eq!(
            parse_date_time("00:32:00.999999"),
            Ok(("", TomlValue::Datetime("00:32:00.999999".to_string())))
        );
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(
            parse_array("[ 1, 2, 3 ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Integer(1),
                    TomlValue::Integer(2),
                    TomlValue::Integer(3)
                ])
            ))
        );
        assert_eq!(
            parse_array("[ \"red\", \"yellow\", \"green\" ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::String("red".to_string()),
                    TomlValue::String("yellow".to_string()),
                    TomlValue::String("green".to_string())
                ])
            ))
        );
        assert_eq!(
            parse_array("[ [ 1, 2 ], [3, 4, 5] ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Array(vec![
                        TomlValue::Integer(1),
                        TomlValue::Integer(2)
                    ]),
                    TomlValue::Array(vec![
                        TomlValue::Integer(3),
                        TomlValue::Integer(4),
                        TomlValue::Integer(5)
                    ])
                ])
            ))
        );
        assert_eq!(
            parse_array("[ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Array(vec![
                        TomlValue::Integer(1),
                        TomlValue::Integer(2)
                    ]),
                    TomlValue::Array(vec![
                        TomlValue::String("a".to_string()),
                        TomlValue::String("b".to_string()),
                        TomlValue::String("c".to_string())
                    ])
                ])
            ))
        );
        assert_eq!(
            parse_array("[ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''' ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::String("all".to_string()),
                    TomlValue::String("strings".to_string()),
                    TomlValue::String("are the same".to_string()),
                    TomlValue::String("type".to_string())
                ])
            ))
        );
        assert_eq!(
            parse_array("[ 0.1, 0.2, 0.5, 1, 2, 5 ]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Float(0.1),
                    TomlValue::Float(0.2),
                    TomlValue::Float(0.5),
                    TomlValue::Integer(1),
                    TomlValue::Integer(2),
                    TomlValue::Integer(5)
                ])
            ))
        );
        // assert_eq!(
        //     parse_array("[\n\t\"Foo Bar <foo@example.com>\",\n\t{ name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }\n]"),
        // );
        assert_eq!(
            parse_array("[\n  1, 2, 3\n]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Integer(1),
                    TomlValue::Integer(2),
                    TomlValue::Integer(3)
                ])
            ))
        );
        assert_eq!(
            parse_array("[\n  1,\n  2, # this is ok\n]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::Integer(1),
                    TomlValue::Integer(2)
                ])
            ))
        );
    }

    #[test]
    fn test_parse_inline_table() {
        assert_eq!(
            parse_inline_table("{ first = \"Tom\", last = \"Preston-Werner\" }"),
            Ok((
                "",
                TomlValue::Table(
                    vec![
                        ("first".to_string(), TomlValue::String("Tom".to_string())),
                        ("last".to_string(), TomlValue::String("Preston-Werner".to_string()))
                    ].into_iter().collect()
                )
            ))
        );
        assert_eq!(
            parse_inline_table("{ x = 1, y = 2 }"),
            Ok((
                "",
                TomlValue::Table(
                    vec![
                        ("x".to_string(), TomlValue::Integer(1)),
                        ("y".to_string(), TomlValue::Integer(2))
                    ].into_iter().collect()
                )
            ))
        );
        assert_eq!(
            parse_inline_table("{ type.name = \"pug\" }"),
            Ok((
                "",
                TomlValue::Table(
                    vec![(
                        "type".to_string(),
                        TomlValue::Table(
                            vec![
                                ("name".to_string(), TomlValue::String("pug".to_string()))
                            ].into_iter().collect()
                        )
                    )].into_iter().collect()
                )
            ))
        );
    }
}
