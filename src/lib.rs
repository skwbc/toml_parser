use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1, take_while_m_n},
    character::complete::{line_ending, one_of, space0},
    combinator::{cut, map, not, opt, peek, recognize},
    error::{ErrorKind, ParseError},
    multi::{many0, many1, many_m_n},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{collections::HashMap, iter};

#[derive(Debug, PartialEq, Clone)]
enum TomlKey {
    SimpleKey(String),
    DottedKey(Vec<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TomlValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    OffsetDateTime(DateTime<chrono::offset::FixedOffset>),
    LocalDateTime(NaiveDateTime),
    LocalDate(NaiveDate),
    LocalTime(NaiveTime),
    Array(Vec<TomlValue>),
    InlineTable(HashMap<String, TomlValue>),
    Table(HashMap<String, TomlValue>),
    ArrayTable(Vec<HashMap<String, TomlValue>>),
}

#[derive(Debug, PartialEq, Clone)]
enum TomlExpression {
    KeyVal((TomlKey, TomlValue)),
    StdTable(TomlKey),
    ArrayTable(TomlKey),
}

#[derive(Debug, PartialEq)]
pub enum TomlParserError {
    NomError(String, nom::error::ErrorKind),
    DuplicationError(String),
}

impl ParseError<&str> for TomlParserError {
    fn from_error_kind(input: &str, kind: ErrorKind) -> Self {
        TomlParserError::NomError(input.to_string(), kind)
    }

    fn append(_: &str, _: ErrorKind, other: Self) -> Self {
        other
    }
}

type MyResult<I, O> = IResult<I, O, TomlParserError>;

/// toml = expression *( newline expression )
pub fn parse_toml(input: &str) -> MyResult<&str, HashMap<String, TomlValue>> {
    let (mut input, exp) = parse_expression(input)?;
    let mut toml = HashMap::new();
    let mut current_table = match exp {
        Some(e) => add_expression_to_toml(&mut toml, e)?,
        None => &mut toml,
    };
    loop {
        input = match preceded(parse_newline, parse_expression)(input) {
            Ok((input, exp)) => {
                if let Some(TomlExpression::StdTable(_) | TomlExpression::ArrayTable(_)) = exp {
                    current_table = &mut toml;
                }
                current_table = match exp {
                    Some(e) => add_expression_to_toml(current_table, e)?,
                    None => current_table,
                };
                input
            }
            Err(nom::Err::Failure(e)) => {
                return Err(nom::Err::Failure(e))?;
            }
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        };
    }

    // inputが空文字列でない場合はエラーとする
    // ToDo: 一部のケースでは、cut を使って nom::error::Failure を使えば普通のパーサーエラーに出来そう
    if input.is_empty() {
        Ok((input, toml))
    } else {
        Err(nom::Err::Failure(TomlParserError::NomError(
            input.to_string(),
            ErrorKind::Eof,
        )))?
    }
}

fn add_expression_to_toml(
    toml: &mut HashMap<String, TomlValue>,
    exp: TomlExpression,
) -> Result<&mut HashMap<String, TomlValue>, nom::Err<TomlParserError>> {
    match exp {
        TomlExpression::KeyVal((key, value)) => {
            add_to_toml(toml, key, value)?;
            Ok(toml)
        }
        TomlExpression::StdTable(key) => add_to_toml(toml, key, TomlValue::Table(HashMap::new())),
        TomlExpression::ArrayTable(key) => {
            add_to_toml(toml, key, TomlValue::ArrayTable(vec![HashMap::new()]))
        }
    }
}

fn _add_to_toml(
    toml: &mut HashMap<String, TomlValue>,
    key: String,
    value: TomlValue,
) -> Result<&mut HashMap<String, TomlValue>, nom::Err<TomlParserError>> {
    match (toml.contains_key(&key), value) {
        (true, TomlValue::ArrayTable(_)) => match toml.get_mut(&key).unwrap() {
            TomlValue::ArrayTable(v) => {
                v.push(HashMap::new());
                Ok(v.last_mut().unwrap())
            }
            _ => Err(nom::Err::Failure(TomlParserError::DuplicationError(
                format!("key {} is already defined", key),
            ))),
        },
        (true, _) => Err(nom::Err::Failure(TomlParserError::DuplicationError(
            format!("key {} is already defined", key),
        ))),
        (false, array_table @ TomlValue::ArrayTable(_)) => {
            toml.insert(key.to_owned(), array_table);
            match toml.get_mut(&key).unwrap() {
                TomlValue::ArrayTable(v) => Ok(v.last_mut().unwrap()),
                _ => unreachable!(),
            }
        }
        (false, table @ TomlValue::Table(_)) => {
            toml.insert(key.to_owned(), table);
            match toml.get_mut(&key).unwrap() {
                TomlValue::Table(t) => Ok(t),
                _ => unreachable!(),
            }
        }
        (false, value) => {
            toml.insert(key, value);
            Ok(toml)
        }
    }
}

fn add_to_toml(
    toml: &mut HashMap<String, TomlValue>,
    key: TomlKey,
    value: TomlValue,
) -> Result<&mut HashMap<String, TomlValue>, nom::Err<TomlParserError>> {
    let allow_array_table = matches!(value, TomlValue::Table(_) | TomlValue::ArrayTable(_));
    match key {
        TomlKey::SimpleKey(key) => _add_to_toml(toml, key, value),
        TomlKey::DottedKey(keys) => {
            let mut current = toml;
            let mut subkey = Vec::new();
            for k in keys[..keys.len() - 1].iter() {
                subkey.push(k.clone());
                current
                    .entry(k.clone())
                    .or_insert_with(|| TomlValue::Table(HashMap::new()));
                current = match current.get_mut(k) {
                    Some(TomlValue::Table(t)) => t,
                    Some(TomlValue::ArrayTable(v)) if allow_array_table => v.last_mut().unwrap(),
                    _ => {
                        return Err(nom::Err::Failure(TomlParserError::DuplicationError(
                            format!("key {} is already defined", subkey.join(".")),
                        )));
                    }
                };
            }
            match _add_to_toml(current, keys.last().unwrap().clone(), value) {
                Ok(t) => Ok(t),
                Err(_) => Err(nom::Err::Failure(TomlParserError::DuplicationError(
                    format!("key {} is already defined", keys.join(".")),
                ))),
            }
        }
    }
}

/// expression =  ws [ comment ]
/// expression =/ ws keyval ws [ comment ]
/// expression =/ ws table ws [ comment ]
fn parse_expression(input: &str) -> MyResult<&str, Option<TomlExpression>> {
    let (input, _) = parse_ws(input)?;
    let r = terminated(alt((parse_keyval, parse_table)), parse_ws)(input);
    let (input, expression) = match r {
        Ok((input, kv)) => (input, Some(kv)),
        Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
        Err(_) => (input, None),
    };
    let (input, _) = opt(parse_comment)(input)?;
    Ok((input, expression))
}

/// ws = *wschar
/// wschar =  %x20  ; Space
/// wschar =/ %x09  ; Horizontal tab
fn parse_ws(input: &str) -> MyResult<&str, &str> {
    space0(input)
}

/// newline =  %x0A     ; LF
/// newline =/ %x0D.0A  ; CRLF
fn parse_newline(input: &str) -> MyResult<&str, &str> {
    line_ending(input)
}

/// comment = comment-start-symbol *allowed-comment-char
/// comment-start-symbol = %x23 ; #
/// allowed-comment-char = %x01-09 / %x0E-7F / non-ascii
/// non-ascii = %x80-D7FF / %xE000-10FFFF
fn parse_comment(input: &str) -> MyResult<&str, &str> {
    recognize(tuple((
        tag("#"),
        take_while(
            |c| matches!(c as u32, 0x01..=0x09 | 0x0E..=0x7F | 0x80..=0xD7FF | 0xE000..=0x10FFFF),
        ),
    )))(input)
}

/// keyval = key keyval-sep val
fn parse_keyval(input: &str) -> MyResult<&str, TomlExpression> {
    map(_parse_keyval, TomlExpression::KeyVal)(input)
}

fn _parse_keyval(input: &str) -> MyResult<&str, (TomlKey, TomlValue)> {
    separated_pair(parse_key, parse_keyval_sep, cut(parse_val))(input)
}

// keyval-sep = ws %x3D ws ; =
fn parse_keyval_sep(input: &str) -> MyResult<&str, &str> {
    recognize(tuple((parse_ws, tag("="), parse_ws)))(input)
}

/// key = simple-key / dotted-key
fn parse_key(input: &str) -> MyResult<&str, TomlKey> {
    alt((parse_dotted_key, parse_simple_key))(input)
}

/// val = string / boolean / array / inline-table / date-time / float / integer
fn parse_val(input: &str) -> MyResult<&str, TomlValue> {
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
fn parse_simple_key(input: &str) -> MyResult<&str, TomlKey> {
    // map(alt((parse_quoted_key, parse_unquoted_key)), TomlKey::SimpleKey)(input)
    map(_parse_simple_key, TomlKey::SimpleKey)(input)
}

/// simple-keyをパースするが、TomlKey::SimpleKeyでラップせずStringを返す
fn _parse_simple_key(input: &str) -> MyResult<&str, String> {
    alt((parse_quoted_key, parse_unquoted_key))(input)
}

/// unquoted-key = 1*unquoted-key-char
fn parse_unquoted_key(input: &str) -> MyResult<&str, String> {
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
fn parse_quoted_key(input: &str) -> MyResult<&str, String> {
    alt((parse_basic_string, parse_literal_string))(input)
}

/// dotted-key = simple-key 1*( dot-sep simple-key )
fn parse_dotted_key(input: &str) -> MyResult<&str, TomlKey> {
    let (input, key) = _parse_simple_key(input)?;
    let (input, keys) = many1(preceded(parse_dot_sep, _parse_simple_key))(input)?;
    let output = TomlKey::DottedKey(iter::once(key).chain(keys).collect());
    Ok((input, output))
}

// dot-sep   = ws %x2E ws  ; . Period
fn parse_dot_sep(input: &str) -> MyResult<&str, &str> {
    recognize(tuple((parse_ws, tag("."), parse_ws)))(input)
}

/// string = ml-basic-string / basic-string / ml-literal-string / literal-string
fn parse_string(input: &str) -> MyResult<&str, TomlValue> {
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
fn parse_basic_string(input: &str) -> MyResult<&str, String> {
    map(
        delimited(tag("\""), recognize(many0(parse_basic_char)), tag("\"")),
        &str::to_string,
    )(input)
}

/// basic-char = basic-unescaped / escaped
fn parse_basic_char(input: &str) -> MyResult<&str, &str> {
    alt((parse_basic_unescaped, parse_escaped))(input)
}

/// basic-unescaped = wschar / %x21 / %x23-5B / %x5D-7E / non-ascii
fn parse_basic_unescaped(input: &str) -> MyResult<&str, &str> {
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
fn parse_escaped(input: &str) -> MyResult<&str, &str> {
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
fn parse_ml_basic_string(input: &str) -> MyResult<&str, String> {
    delimited(
        tuple((tag("\"\"\""), opt(parse_newline))),
        parse_ml_basic_body,
        tag("\"\"\""),
    )(input)
}

/// ml-basic-body = *mlb-content *( mlb-quotes 1*mlb-content ) [ mlb-quotes ]
fn parse_ml_basic_body(input: &str) -> MyResult<&str, String> {
    let (input, content) = many0(parse_mlb_content)(input)?;
    let (input, quotes_content) =
        many0(tuple((parse_mlb_quotes, many1(parse_mlb_content))))(input)?;
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
fn parse_mlb_quotes(input: &str) -> MyResult<&str, &str> {
    alt((
        recognize(tuple((tag("\""), not(tag("\""))))),
        recognize(tuple((tag("\"\""), not(tag("\""))))),
        recognize(tuple((tag("\""), peek(tag("\"\"\""))))),
        recognize(tuple((tag("\"\""), peek(tag("\"\"\""))))),
    ))(input)
}

/// mlb-content = basic-char / newline / mlb-escaped-nl
/// mlb-escaped-nl = escape ws newline *( wschar / newline )
fn parse_mlb_content(input: &str) -> MyResult<&str, &str> {
    let result = alt((parse_basic_char, parse_newline))(input);
    if result.is_ok() {
        return result;
    }
    let result = tuple((
        tag("\\"),
        parse_ws,
        parse_newline,
        many0(alt((tag(" "), tag("\t"), parse_newline))),
    ))(input);
    match result {
        Ok((input, _)) => Ok((input, "")),
        Err(e) => Err(e),
    }
}

/// literal-string = apostrophe *literal-char apostrophe
///
/// apostrophe = %x27 ; ' apostrophe
fn parse_literal_string(input: &str) -> MyResult<&str, String> {
    map(
        delimited(tag("'"), take_while(is_literal_char), tag("'")),
        &str::to_string,
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
fn parse_ml_literal_string(input: &str) -> MyResult<&str, String> {
    map(
        delimited(
            tuple((tag("'''"), opt(parse_newline))),
            parse_ml_literal_body,
            tag("'''"),
        ),
        &str::to_string,
    )(input)
}

/// ml-literal-body = *mll-content *( mll-quotes 1*mll-content ) [ mll-quotes ]
fn parse_ml_literal_body(input: &str) -> MyResult<&str, &str> {
    recognize(tuple((
        many0(parse_mll_content),
        many0(tuple((parse_mll_quotes, many1(parse_mll_content)))),
        opt(parse_mll_quotes),
    )))(input)
}

/// mll-content = literal-char / newline
fn parse_mll_content(input: &str) -> MyResult<&str, &str> {
    alt((take_while_m_n(1, 1, is_literal_char), parse_newline))(input)
}

/// mll-quotes = 1*2apostrophe
fn parse_mll_quotes(input: &str) -> MyResult<&str, &str> {
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
fn parse_integer(input: &str) -> MyResult<&str, TomlValue> {
    let (input, n) = alt((
        parse_hex_int,
        parse_oct_int,
        parse_bin_int,
        parse_dec_int, // Memo: dec-intを最後にしないと、prefixの0がパースされてしまう
    ))(input)?;
    Ok((input, TomlValue::Integer(n)))
}

/// dec-int = [ minus / plus ] unsigned-dec-int
fn parse_dec_int(input: &str) -> MyResult<&str, i64> {
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
fn parse_unsigned_dec_int(input: &str) -> MyResult<&str, i64> {
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
) -> MyResult<&'a str, i64> {
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
fn parse_hex_int(input: &str) -> MyResult<&str, i64> {
    parse_xxx_int(input, "0x", "0123456789ABCDEFabcdef", 16)
}

/// oct-int = oct-prefix digit0-7 *( digit0-7 / underscore digit0-7 )
fn parse_oct_int(input: &str) -> MyResult<&str, i64> {
    parse_xxx_int(input, "0o", "01234567", 8)
}

/// bin-int = bin-prefix digit0-1 *( digit0-1 / underscore digit0-1 )
fn parse_bin_int(input: &str) -> MyResult<&str, i64> {
    parse_xxx_int(input, "0b", "01", 2)
}

/// float = float-int-part ( exp / frac [ exp ] )
/// float =/ special-float
///
/// float-int-part = dec-int
fn parse_float(input: &str) -> MyResult<&str, TomlValue> {
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
fn parse_frac(input: &str) -> MyResult<&str, i64> {
    let (input, _) = tag(".")(input)?;
    parse_zero_prefixable_int(input)
}

/// zero-prefixable-int = DIGIT *( DIGIT / underscore DIGIT )
fn parse_zero_prefixable_int(input: &str) -> MyResult<&str, i64> {
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
fn parse_exp(input: &str) -> MyResult<&str, i64> {
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
fn parse_special_float(input: &str) -> MyResult<&str, f64> {
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
fn parse_boolean(input: &str) -> MyResult<&str, TomlValue> {
    let (input, value) = alt((tag("true"), tag("false")))(input)?;
    Ok((input, TomlValue::Boolean(value == "true")))
}

/// date-time      = offset-date-time / local-date-time / local-date / local-time
fn parse_date_time(input: &str) -> MyResult<&str, TomlValue> {
    alt((
        parse_offset_date_time,
        parse_local_date_time,
        parse_local_date,
        parse_local_time,
    ))(input)
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
fn parse_partial_time(input: &str) -> MyResult<&str, &str> {
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
fn parse_full_date(input: &str) -> MyResult<&str, &str> {
    recognize(tuple((
        many_m_n(4, 4, one_of("0123456789")),
        tag("-"),
        many_m_n(2, 2, one_of("0123456789")),
        tag("-"),
        many_m_n(2, 2, one_of("0123456789")),
    )))(input)
}

/// full-time      = partial-time time-offset
fn parse_full_time(input: &str) -> MyResult<&str, &str> {
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
fn parse_offset_date_time(input: &str) -> MyResult<&str, TomlValue> {
    map(
        tuple((parse_full_date, one_of("Tt "), parse_full_time)),
        |(full_date, _, full_time)| {
            let s = format!("{}T{}", full_date, full_time);
            TomlValue::OffsetDateTime(DateTime::parse_from_rfc3339(&s).unwrap())
        },
    )(input)
}

/// local-date-time = full-date time-delim partial-time
fn parse_local_date_time(input: &str) -> MyResult<&str, TomlValue> {
    map(
        tuple((parse_full_date, one_of("Tt "), parse_partial_time)),
        |(full_date, _, partial_time)| {
            let s = format!("{}T{}", full_date, partial_time);
            let mut result = NaiveDateTime::parse_from_str(&s, "%Y-%m-%dT%H:%M:%S");
            if result.is_err() {
                result = NaiveDateTime::parse_from_str(&s, "%Y-%m-%dT%H:%M:%S%.f");
            }
            TomlValue::LocalDateTime(result.unwrap())
        },
    )(input)
}

/// local-date = full-date
fn parse_local_date(input: &str) -> MyResult<&str, TomlValue> {
    map(parse_full_date, |s| {
        TomlValue::LocalDate(NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap())
    })(input)
}

/// local-time = partial-time
fn parse_local_time(input: &str) -> MyResult<&str, TomlValue> {
    map(parse_partial_time, |s| {
        TomlValue::LocalTime(NaiveTime::parse_from_str(s, "%H:%M:%S%.f").unwrap())
    })(input)
}

/// array = array-open [ array-values ] ws-comment-newline array-close
///
/// array-open =  %x5B ; [
/// array-close = %x5D ; ]
fn parse_array(input: &str) -> MyResult<&str, TomlValue> {
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
fn parse_array_values(input: &str) -> MyResult<&str, Vec<TomlValue>> {
    let parse_array_value = delimited(
        parse_ws_comment_newline,
        parse_val,
        tuple((parse_ws_comment_newline, opt(tag(",")))),
    );
    many1(parse_array_value)(input)
}

/// ws-comment-newline = *( wschar / [ comment ] newline )
fn parse_ws_comment_newline(input: &str) -> MyResult<&str, &str> {
    recognize(many0(alt((
        recognize(one_of(" \t")),
        recognize(tuple((opt(parse_comment), parse_newline))),
    ))))(input)
}

/// table = std-table / array-table
fn parse_table(input: &str) -> MyResult<&str, TomlExpression> {
    alt((parse_std_table, parse_array_table))(input)
}

/// std-table = std-table-open key std-table-close
///
/// std-table-open  = %x5B ws     ; [ Left square bracket
/// std-table-close = ws %x5D     ; ] Right square bracket
fn parse_std_table(input: &str) -> MyResult<&str, TomlExpression> {
    map(
        delimited(tag("["), parse_key, tuple((parse_ws, tag("]")))),
        TomlExpression::StdTable,
    )(input)
}

/// inline-table = inline-table-open [ inline-table-keyvals ] ws-comment-newline inline-table-close
///
/// inline-table-open  = %x7B  ; {
/// inline-table-close = %x7D  ; }
/// inline-table-sep   = %x2C  ; , Comma
fn parse_inline_table(input: &str) -> MyResult<&str, TomlValue> {
    map(
        delimited(
            tag("{"),
            opt(parse_inline_table_keyvals),
            tuple((parse_ws_comment_newline, tag("}"))),
        ),
        |t| TomlValue::InlineTable(t.unwrap_or_default()),
    )(input)
}

/// inline-table-keyvals =  ws-comment-newline keyval ws-comment-newline inline-table-sep inline-table-keyvals
/// inline-table-keyvals =/ ws-comment-newline keyval ws-comment-newline [ inline-table-sep ]
fn parse_inline_table_keyvals(input: &str) -> MyResult<&str, HashMap<String, TomlValue>> {
    let parse_inline_table_keyval = delimited(
        parse_ws_comment_newline,
        _parse_keyval,
        tuple((parse_ws_comment_newline, opt(tag(",")))),
    );
    let (input, v) = many1(parse_inline_table_keyval)(input)?;
    let mut result = HashMap::new();
    for (k, v) in v {
        add_to_toml(&mut result, k, v)?;
    }
    Ok((input, result))
}

/// array-table = array-table-open key array-table-close
/// array-table-open  = %x5B.5B ws  ; [[ Double left square bracket
/// array-table-close = ws %x5D.5D  ; ]] Double right square bracket
fn parse_array_table(input: &str) -> MyResult<&str, TomlExpression> {
    map(
        delimited(tag("[["), parse_key, tuple((parse_ws, tag("]]")))),
        TomlExpression::ArrayTable,
    )(input)
}

// ALPHA = %x41-5A / %x61-7A ; A-Z / a-z
// DIGIT = %x30-39 ; 0-9
// HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

#[cfg(test)]
mod tests {
    use super::*;
    use maplit::hashmap;

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
        assert_eq!(
            parse_key("key"),
            Ok(("", TomlKey::SimpleKey("key".to_string())))
        );
        assert_eq!(
            parse_key("bare_key"),
            Ok(("", TomlKey::SimpleKey("bare_key".to_string())))
        );
        assert_eq!(
            parse_key("bare-key"),
            Ok(("", TomlKey::SimpleKey("bare-key".to_string())))
        );
        assert_eq!(
            parse_key("1234"),
            Ok(("", TomlKey::SimpleKey("1234".to_string())))
        );

        // quoted-key
        assert_eq!(
            parse_key("\"127.0.0.1\""),
            Ok(("", TomlKey::SimpleKey("127.0.0.1".to_string())))
        );
        assert_eq!(
            parse_key("\"character encoding\""),
            Ok(("", TomlKey::SimpleKey("character encoding".to_string())))
        );
        assert_eq!(
            parse_key("\"ʎǝʞ\""),
            Ok(("", TomlKey::SimpleKey("ʎǝʞ".to_string())))
        );
        assert_eq!(
            parse_key("'key2'"),
            Ok(("", TomlKey::SimpleKey("key2".to_string())))
        );
        assert_eq!(
            parse_key("'quoted \"value\"'"),
            Ok(("", TomlKey::SimpleKey("quoted \"value\"".to_string())))
        );
        assert_eq!(
            parse_key("\"\""),
            Ok(("", TomlKey::SimpleKey("".to_string())))
        );
        assert_eq!(
            parse_key("''"),
            Ok(("", TomlKey::SimpleKey("".to_string())))
        );

        // dotted-key
        assert_eq!(
            parse_key("physical.color"),
            Ok((
                "",
                TomlKey::DottedKey(vec!["physical".to_string(), "color".to_string()])
            ))
        );
        assert_eq!(
            parse_key("physical.shape"),
            Ok((
                "",
                TomlKey::DottedKey(vec!["physical".to_string(), "shape".to_string()])
            ))
        );
        assert_eq!(
            parse_key("site.\"google.com\""),
            Ok((
                "",
                TomlKey::DottedKey(vec!["site".to_string(), "google.com".to_string()])
            ))
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
            parse_string(
                "\"\"\"\nThe quick brown \\\n\n\n  fox jumps over \\\n    the lazy dog.\"\"\""
            ),
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
                TomlValue::String(
                    "\"This,\" she said, \"is just a pointless statement.\"".to_string()
                ),
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
            Ok(("", TomlValue::String("<\\i\\c*\\s*>".to_string())))
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
                TomlValue::String(
                    "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"".to_string()
                ),
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
    #[allow(clippy::approx_constant)]
    #[allow(clippy::excessive_precision)]
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
    fn test_parse_offset_date_time() {
        assert_eq!(
            parse_date_time("1979-05-27T07:32:00Z"),
            Ok((
                "",
                TomlValue::OffsetDateTime(
                    DateTime::parse_from_rfc3339("1979-05-27T07:32:00Z").unwrap()
                )
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00-07:00"),
            Ok((
                "",
                TomlValue::OffsetDateTime(
                    DateTime::parse_from_rfc3339("1979-05-27T00:32:00-07:00").unwrap()
                )
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00-07:00"),
            Ok((
                "",
                TomlValue::OffsetDateTime(
                    DateTime::parse_from_rfc3339("1979-05-27T00:32:00-07:00").unwrap()
                )
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00.999999-07:00"),
            Ok((
                "",
                TomlValue::OffsetDateTime(
                    DateTime::parse_from_rfc3339("1979-05-27T00:32:00.999999-07:00").unwrap()
                )
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27 07:32:00Z"),
            Ok((
                "",
                TomlValue::OffsetDateTime(
                    DateTime::parse_from_rfc3339("1979-05-27T07:32:00Z").unwrap()
                )
            ))
        );
    }

    #[test]
    fn test_parse_local_date_time() {
        assert_eq!(
            parse_date_time("1979-05-27T07:32:00"),
            Ok((
                "",
                TomlValue::LocalDateTime(
                    NaiveDateTime::parse_from_str("1979-05-27T07:32:00", "%Y-%m-%dT%H:%M:%S")
                        .unwrap()
                )
            ))
        );
        assert_eq!(
            parse_date_time("1979-05-27T00:32:00.999999"),
            Ok((
                "",
                TomlValue::LocalDateTime(
                    NaiveDateTime::parse_from_str(
                        "1979-05-27T00:32:00.999999",
                        "%Y-%m-%dT%H:%M:%S%.f"
                    )
                    .unwrap()
                )
            ))
        );
    }

    #[test]
    fn test_parse_local_date() {
        assert_eq!(
            parse_date_time("1979-05-27"),
            Ok((
                "",
                TomlValue::LocalDate(NaiveDate::parse_from_str("1979-05-27", "%Y-%m-%d").unwrap())
            ))
        );
    }

    #[test]
    fn test_parse_local_time() {
        assert_eq!(
            parse_date_time("07:32:00"),
            Ok((
                "",
                TomlValue::LocalTime(NaiveTime::parse_from_str("07:32:00", "%H:%M:%S").unwrap())
            ))
        );
        assert_eq!(
            parse_date_time("00:32:00.999999"),
            Ok((
                "",
                TomlValue::LocalTime(
                    NaiveTime::parse_from_str("00:32:00.999999", "%H:%M:%S%.f").unwrap()
                )
            ))
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
                    TomlValue::Array(vec![TomlValue::Integer(1), TomlValue::Integer(2)]),
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
                    TomlValue::Array(vec![TomlValue::Integer(1), TomlValue::Integer(2)]),
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
        assert_eq!(
            parse_array("[\n\t\"Foo Bar <foo@example.com>\",\n\t{ name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }\n]"),
            Ok((
                "",
                TomlValue::Array(vec![
                    TomlValue::String("Foo Bar <foo@example.com>".to_string()),
                    TomlValue::InlineTable(
                        hashmap!{
                            "name".to_string() => TomlValue::String("Baz Qux".to_string()),
                            "email".to_string() => TomlValue::String("bazqux@example.com".to_string()),
                            "url".to_string() => TomlValue::String("https://example.com/bazqux".to_string()),
                        }
                    )
                ])
            ))
        );
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
                TomlValue::Array(vec![TomlValue::Integer(1), TomlValue::Integer(2)])
            ))
        );
    }

    #[test]
    fn test_parse_inline_table() {
        assert_eq!(
            parse_inline_table("{ first = \"Tom\", last = \"Preston-Werner\" }"),
            Ok((
                "",
                TomlValue::InlineTable(hashmap! {
                    "first".to_string() => TomlValue::String("Tom".to_string()),
                    "last".to_string() => TomlValue::String("Preston-Werner".to_string())
                })
            ))
        );
        assert_eq!(
            parse_inline_table("{ x = 1, y = 2 }"),
            Ok((
                "",
                TomlValue::InlineTable(hashmap! {
                    "x".to_string() => TomlValue::Integer(1),
                    "y".to_string() => TomlValue::Integer(2),
                })
            ))
        );
        assert_eq!(
            parse_inline_table("{ type.name = \"pug\" }"),
            Ok((
                "",
                TomlValue::InlineTable(hashmap! {
                    "type".to_string() => TomlValue::Table(hashmap!{
                        "name".to_string() => TomlValue::String("pug".to_string())
                    })
                })
            ))
        );
    }

    #[test]
    fn test_parse_std_table() {
        assert_eq!(
            parse_table("[table]"),
            Ok((
                "",
                TomlExpression::StdTable(TomlKey::SimpleKey("table".to_string()))
            )),
        );
        assert_eq!(
            parse_toml(
                r#"
            [table-1]
            key1 = "some string"
            key2 = 123

            [table_2]
            key1 = "another string"
            key2 = 456
            "#
            ),
            Ok((
                "",
                hashmap! {
                    "table-1".to_string() => TomlValue::Table(hashmap!{
                        "key1".to_string() => TomlValue::String("some string".to_string()),
                        "key2".to_string() => TomlValue::Integer(123)
                    }),
                    "table_2".to_string() => TomlValue::Table(hashmap!{
                        "key1".to_string() => TomlValue::String("another string".to_string()),
                        "key2".to_string() => TomlValue::Integer(456)
                    })
                }
            ))
        );
        assert_eq!(
            parse_toml(
                r#"
            [dog."tater.man"]
            type.name = "pug"
            "#
            ),
            Ok((
                "",
                hashmap! {
                    "dog".to_string() => TomlValue::Table(hashmap!{
                        "tater.man".to_string() => TomlValue::Table(hashmap!{
                            "type".to_string() => TomlValue::Table(hashmap!{
                                "name".to_string() => TomlValue::String("pug".to_string())
                            })
                        })
                    })
                }
            ))
        )
    }

    #[test]
    fn test_parse_array_table() {
        assert_eq!(
            parse_toml(
                r#"
            [[products]]
            name = "Hammer"
            sku = 738594937

            [[products]]

            [[products]]
            name = "Nail"
            sku = 284758393

            color = "gray"
            "#
            ),
            Ok((
                "",
                hashmap! {
                    "products".to_string() => TomlValue::ArrayTable(vec![
                        hashmap!{
                            "name".to_string() => TomlValue::String("Hammer".to_string()),
                            "sku".to_string() => TomlValue::Integer(738594937)
                        },
                        hashmap!{},
                        hashmap!{
                            "name".to_string() => TomlValue::String("Nail".to_string()),
                            "sku".to_string() => TomlValue::Integer(284758393),
                            "color".to_string() => TomlValue::String("gray".to_string())
                        }
                    ])
                }
            ))
        )
    }

    #[test]
    fn test_parse_nested_array_table() {
        assert_eq!(
            parse_toml(
                r#"
                [[fruit]]
                name = "apple"
            
                [fruit.physical] # テーブル
                color = "red"
                shape = "round"
            
                [[fruit.variety]] # ネストされたテーブルの配列
                name = "red delicious"
            
                [[fruit.variety]]
                name = "granny smith"
            
            [[fruit]]
                name = "banana"
            
                [[fruit.variety]]
                name = "plantain"
            "#
            ),
            Ok((
                "",
                hashmap! {
                    "fruit".to_string() => TomlValue::ArrayTable(vec![
                        hashmap!{
                            "name".to_string() => TomlValue::String("apple".to_string()),
                            "physical".to_string() => TomlValue::Table(hashmap!{
                                "color".to_string() => TomlValue::String("red".to_string()),
                                "shape".to_string() => TomlValue::String("round".to_string())
                            }),
                            "variety".to_string() => TomlValue::ArrayTable(vec![
                                hashmap!{
                                    "name".to_string() => TomlValue::String("red delicious".to_string())
                                },
                                hashmap!{
                                    "name".to_string() => TomlValue::String("granny smith".to_string())
                                }
                            ])
                        },
                        hashmap!{
                            "name".to_string() => TomlValue::String("banana".to_string()),
                            "variety".to_string() => TomlValue::ArrayTable(vec![
                                hashmap!{
                                    "name".to_string() => TomlValue::String("plantain".to_string())
                                }
                            ])
                        }
                    ])
                }
            ))
        )
    }

    #[test]
    fn test_invalid_cases() {
        assert!(parse_toml("key = ").is_err());
        assert!(parse_toml("first = \"Tom\" last = \"Preston-Werner\"").is_err());
        assert!(parse_toml("= \"no key name\"").is_err());
    }

    #[test]
    fn test_duplicated_key() {
        assert_eq!(
            parse_toml(
                r#"
                name = "Tom"
                name = "Pradyun"
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key name is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_duplicated_key_quoted() {
        assert_eq!(
            parse_toml(
                r#"
                spelling = "favorite"
                "spelling" = "favourite"
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key spelling is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_duplicated_key_dotted() {
        assert_eq!(
            parse_toml(
                r#"
                fruit.apple = 1
                fruit.apple.smooth = true
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit.apple is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_duplicated_table_name() {
        assert_eq!(
            parse_toml(
                r#"
                [a]
                b = 1
                
                [a]
                c = 2
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key a is already defined".to_string()
            )))
        );
        assert_eq!(
            parse_toml(
                r#"
                [a]
                b = 1
                
                [a.b]
                c = 2
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key a.b is already defined".to_string()
            )))
        );
        assert_eq!(
            parse_toml(
                r#"
                [fruit]
                apple.color = "red"
                apple.taste.sweet = true
                
                [fruit.apple]
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit.apple is already defined".to_string()
            )))
        );
        assert_eq!(
            parse_toml(
                r#"
                [fruit]
                apple.color = "red"
                apple.taste.sweet = true
                
                [fruit.apple.taste]
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit.apple.taste is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_inline_table_and_sub_table() {
        assert_eq!(
            parse_toml(
                r#"
                [product]
                type = { name = "Nail" }
                type.edible = false
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key type is already defined".to_string()
            )))
        );
        assert_eq!(
            parse_toml(
                r#"
                [product]
                type.name = "Nail"
                type = { edible = false }
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key type is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_sub_table_and_array_table() {
        assert_eq!(
            parse_toml(
                r#"
                [fruit.physical]
                color = "red"
                shape = "round"
                
                [[fruit]]
                name = "apple"
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_array_and_array_table() {
        assert_eq!(
            parse_toml(
                r#"
                fruit = []
                
                [[fruit]]
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_array_table_table() {
        assert_eq!(
            parse_toml(
                r#"
                [[fruit]]
                name = "apple"
                
                [[fruit.variety]]
                name = "red delicious"
                
                [fruit.variety]
                name = "granny smith"
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit.variety is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_table_array_table() {
        assert_eq!(
            parse_toml(
                r#"
                [fruit.physical]
                color = "red"
                shape = "round"
                
                [[fruit.physical]]
                color = "green"
                "#,
            ),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key fruit.physical is already defined".to_string()
            )))
        );
    }

    #[test]
    fn test_duplicated_keys_in_inline_table() {
        assert_eq!(
            parse_toml("inline_table = { a = 1, a = 2 }"),
            Err(nom::Err::Failure(TomlParserError::DuplicationError(
                "key a is already defined".to_string()
            )))
        )
    }
}
