pub fn fmt_quoted_bytes(
    mut bytes: &[u8],
    f: &mut std::fmt::Formatter,
    quote: Option<char>,
) -> std::fmt::Result {
    loop {
        match std::str::from_utf8(bytes) {
            Ok(text) => {
                fmt_quoted_str(text, f, quote)?;
                break;
            }
            Err(error) => {
                let (valid, after_valid) = bytes.split_at(error.valid_up_to());
                unsafe {
                    fmt_quoted_str(std::str::from_utf8_unchecked(valid), f, quote)?;
                }
                let invalid = if let Some(invalid_sequence_length) = error.error_len() {
                    &after_valid[..invalid_sequence_length]
                } else {
                    after_valid
                };
                for byte in invalid {
                    std::fmt::Display::fmt(&byte.escape_ascii(), f)?;
                }
                if let Some(invalid_sequence_length) = error.error_len() {
                    bytes = &after_valid[invalid_sequence_length..];
                } else {
                    break;
                }
            }
        }
    }
    Ok(())
}

pub fn fmt_quoted_str(
    mut text: &str,
    f: &mut std::fmt::Formatter,
    quote: Option<char>,
) -> std::fmt::Result {
    while let Some(index) = text.find(|x| Some(x) == quote || x == '\\' || x.is_ascii_control()) {
        if index != 0 {
            f.write_str(&text[..index])?;
        }
        text = &text[index..];
        debug_assert!(!text.is_empty());
        match quote {
            Some(quote @ '\u{80}'..)
                if !text
                    .as_bytes()
                    .first()
                    .is_some_and(|x| x.is_ascii_control() || *x == b'\\') =>
            {
                std::fmt::Display::fmt(&quote.escape_default(), f)?;
                text = &text[quote.len_utf8()..];
            }
            _ => {
                std::fmt::Display::fmt(&text.as_bytes()[0].escape_ascii(), f)?;
                text = &text[1..];
            }
        }
    }
    f.write_str(text)
}

/// escapes all characters except `"A"..."Z", "a"..."z", "0"..."9", "-", "_", ".", "!", "~", "*", "\'", "(", ")"`
///
/// into url escapes `"/" -> "%2F"`, `" " -> "%20"`
///
/// ```
/// # use server_utils::convert::encode_component;
/// assert_eq!(encode_component(b"https://google.com/search?q=dogs"), "https%3A%2F%2Fgoogle.com%2Fsearch%3Fq%3Ddogs");
/// ```
pub fn encode_component(path: &[u8]) -> String {
    if path.is_empty() {
        return String::new();
    }
    let mut buf = String::with_capacity(path.len() + 4);
    for c in path {
        if matches!(c, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'!' | b'~' | b'*' | b'\'' | b'(' | b')')
        {
            buf.push(*c as char);
        } else {
            buf.push('%');
            if (c >> 4) >= 10 {
                buf.push(((c >> 4) + (b'A' - 10)) as char);
            } else {
                buf.push(((c >> 4) + b'0') as char);
            }
            if (c & 0xF) >= 10 {
                buf.push(((c & 0xF) + (b'A' - 10)) as char);
            } else {
                buf.push(((c & 0xF) + b'0') as char);
            }
        }
    }
    buf
}

pub fn encode_path(path: &[&[u8]]) -> String {
    if path.is_empty() {
        return String::new();
    }
    let mut buf = String::with_capacity(path.iter().map(|x| x.len() + 4).sum());
    for (index, &path) in path.iter().enumerate() {
        if index != 0 {
            buf.push('/');
        }
        for c in path {
            if matches!(c, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'!' | b'~' | b'*' | b'\'' | b'(' | b')')
            {
                buf.push(*c as char);
            } else {
                buf.push('%');
                if (c >> 4) >= 10 {
                    buf.push(((c >> 4) + (b'A' - 10)) as char);
                } else {
                    buf.push(((c >> 4) + b'0') as char);
                }
                if (c & 0xF) >= 10 {
                    buf.push(((c & 0xF) + (b'A' - 10)) as char);
                } else {
                    buf.push(((c & 0xF) + b'0') as char);
                }
            }
        }
    }
    buf
}
