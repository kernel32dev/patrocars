use std::{
    fmt::Write,
    ops::Range,
    path::{Path, PathBuf},
};

use crate::{
    syntax::{self, source::Source},
    runtime,
};

#[derive(Debug)]
pub struct PathedIoError {
    pub path: Box<Path>,
    pub error: std::io::Error,
}

const STYLE: &str = "<style>
    .html-temple-error {
        font-size: 1rem;
        font-weight: normal;
        * {
            font-size: inherit;
        }
        max-width: max-content;
        display: block;
        border: 1px solid currentColor;
        padding: 0.5em;
        margin: 0;
        font-family: 'Consolas', 'Courier New', Courier, monospace;
        overflow: hidden;
        u {
            text-decoration: underline wavy blue;
            font-weight: bold;
        }
        span {
            white-space: pre;
            display: block;
        }
        code {
            white-space: pre;
        }
        p {
            padding: 0;
            margin: 0;
            span {
                overflow: hidden;
                white-space: nowrap;
                text-overflow: ellipsis;
            }
        }
        div, a {
            display: inline-flex;
            overflow: hidden;
            white-space: nowrap;
            text-overflow: ellipsis;
            max-width: 100%;
            div {
                display: inline-block;
                max-width: min-content;
                flex: 1 0;
            }
        }
    }
</style>";

impl PathedIoError {
    pub(crate) fn to_html(&self, source: Option<(&PathBuf, &str, Range<usize>)>) -> String {
        let desc = match self.error.kind() {
            std::io::ErrorKind::NotFound => "arquivo não encontrado",
            std::io::ErrorKind::PermissionDenied => "permissão negada",
            std::io::ErrorKind::AlreadyExists => "arquivo já existe",
            std::io::ErrorKind::TimedOut => "time out",
            std::io::ErrorKind::Interrupted => "interrompido",
            std::io::ErrorKind::Unsupported => "não suportado",
            std::io::ErrorKind::UnexpectedEof => "fim do arquivo inesperado",
            std::io::ErrorKind::OutOfMemory => "sem memória",
            std::io::ErrorKind::Other => "outro erro",
            _ => "erro do sistema operacional",
        };
        let mut output = String::new();
        output.push_str("<div class=\"html-temple-error\"><p><b>erro</b>: ");
        output.push_str(desc);
        output.push_str("</p><p>");
        output.push_as_html(
            &self
                .path
                .display()
                .to_string()
                .trim_start_matches("\\\\?\\"),
        );
        output.push_str("</p><p>");
        output.push_as_html(&self.error.to_string());
        output.push_str("</p>");
        if let Some((file, code, range)) = source {
            let line_column = source_to_html(&mut output, code, range);
            make_vscode_link(&mut output, file, line_column);
        }
        output.push_str(STYLE);
        output.push_str("</div>");
        output
    }
}

impl syntax::error::Error<'_> {
    pub(crate) fn to_html(&self, source: Option<(&PathBuf, &str, Range<usize>)>) -> String {
        let mut output = String::new();
        output.push_str("<div class=\"html-temple-error\"><p><b>erro</b>: ");
        output.push_as_html(&self.to_string());
        output.push_str("</p>");
        if let Some((file, code, range)) = source {
            let line_column = source_to_html(&mut output, code, range);
            make_vscode_link(&mut output, file, line_column);
        }
        output.push_str(STYLE);
        output.push_str("</div>");
        output
    }
}

impl<'a> runtime::error::Error<'a> {
    pub fn to_html<'b>(
        &self,
        sources: impl Fn(Source) -> Option<(&'b PathBuf, &'b str, Range<usize>)>,
    ) -> String {
        let vec = match self {
            runtime::error::Error::Runtime(vec) => vec,
            runtime::error::Error::Io(error) => return error.to_html(None),
        };
        let mut output = format!("<div class=\"html-temple-error\">");
        let mut links: Vec<(&PathBuf, (usize, usize))> = Vec::new();
        for (index, (description, source, value)) in vec.iter().enumerate() {
            if index == 0 {
                output.push_str("<p><b>erro</b>: ");
            } else {
                output.push_str("<hr><p><b>info</b>: ");
            }
            output.push_str(description);
            output.push_str("</p>");
            if let Some((file, code, range)) = sources(*source) {
                let line_col = source_to_html(&mut output, code, range);
                match links.iter_mut().find(|(k, _)| *k == file) {
                    Some((_, value)) => *value = (*value).min(line_col),
                    None => links.push((file, line_col)),
                }
            }
            if let Some(value) = value {
                output.push_str("<p><b>valor</b>: <code>");
                output.push_as_html(&format!("{value:#}"));
                output.push_str("</code></p>");
            }
            output.push_str("</p>");
        }
        for (filename, line_column) in links {
            make_vscode_link(&mut output, filename, line_column);
        }
        output.push_str(STYLE);
        output.push_str("</div>");
        output
    }
}

fn source_to_html(output: &mut String, code: &str, range: Range<usize>) -> (usize, usize) {
    let Range { start, end } = range;
    let line_start = code[..start].bytes().filter(|x| *x == b'\n').count();
    let line_end = line_start + code[start..end].bytes().filter(|x| *x == b'\n').count();
    let mut lines = code.split('\n');
    let mut first_line_pos = 0;
    for _ in 0..line_start {
        let Some(next) = lines.next() else { break };
        first_line_pos += next.len() + 1;
    }
    let line_col = (line_start + 1, 1 + start - first_line_pos);
    if line_start == line_end {
        let line_number = line_start;
        let line_text = lines.next().expect("TODO!");
        let trim_start = line_text.len() - line_text.trim_start().len();
        output
            .write_fmt(format_args!("<span><b>{} | </b>", line_number + 1))
            .ok();
        output.push_as_html(&line_text[trim_start..start - first_line_pos]);
        output.push_str("<u>");
        output.push_as_html(&line_text[start - first_line_pos..end - first_line_pos]);
        output.push_str("</u>");
        output.push_as_html(&line_text[end - first_line_pos..]);
        output.push_str("</span>");
    } else {
        let line_number_padding = (line_end + 1).checked_ilog10().unwrap_or(0) as usize + 1;
        let trim_start = lines
            .clone()
            .take(line_end - line_start + 1)
            .map(|x| x.len() - x.trim_start().len())
            .min()
            .unwrap_or(0);
        for (line_number, line_text) in (line_start..=line_end).zip(lines) {
            if line_number == line_start {
                output
                    .write_fmt(format_args!(
                        "<span><b>{:1$} | </b>",
                        line_number + 1,
                        line_number_padding
                    ))
                    .ok();
                output.push_as_html(&line_text[trim_start..start - first_line_pos]);
                output.push_str("<u>");
                output.push_as_html(&line_text[start - first_line_pos..]);
                output.push_str("</u></span>");
            } else if line_number < line_end {
                output
                    .write_fmt(format_args!(
                        "<span><b>{:1$} | </b><u>",
                        line_number + 1,
                        line_number_padding
                    ))
                    .ok();
                output.push_as_html(&line_text[trim_start..]);
                output.push_str("</u></span>");
            } else {
                output
                    .write_fmt(format_args!(
                        "<span><b>{:1$} | </b><u>",
                        line_number + 1,
                        line_number_padding
                    ))
                    .ok();
                output.push_as_html(&line_text[trim_start..end - first_line_pos]);
                output.push_str("</u>");
                output.push_as_html(&line_text[end - first_line_pos..]);
                output.push_str("</span>");
            }
            first_line_pos += line_text.len() + 1;
        }
    }
    line_col
}

fn make_vscode_link(output: &mut String, filename: &Path, line_column: (usize, usize)) {
    let (line, column) = line_column;
    let filename = filename.display().to_string();
    let filename = filename.trim_start_matches("\\\\?\\");
    output.push_str("<a href=\"vscode://file/");
    output.push_as_html(filename);
    output.write_fmt(format_args!(":{}:{}", line, column)).ok();
    output.push_str("\">");
    // TODO!
    // <div>C:\workspace-root</div><b>\src\main.html</b>:31
    output.push_as_html(filename);
    output.write_fmt(format_args!(":{}", line)).ok();
    // <div>C:\workspace-root</div><b>\src\main.html</b>:31
    output.push_str("</a>");
}

trait PushAsHtml {
    fn push_as_html(&mut self, text: &str);
}
impl PushAsHtml for String {
    fn push_as_html(&mut self, mut text: &str) {
        while let Some(index) = text.find(|x| matches!(x, '<' | '>' | '&' | '"' | '\'')) {
            self.push_str(&text[..index]);
            let entity = match &text[index..index + 1] {
                "<" => "&lt;",
                ">" => "&gt;",
                "&" => "&amp;",
                "\"" => "&quot;",
                "'" => "&#39;",
                _ => unreachable!(),
            };
            self.push_str(entity);
            text = &text[index + 1..];
        }
        self.push_str(text)
    }
}
