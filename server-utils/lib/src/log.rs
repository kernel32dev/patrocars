use log::Log;
use std::cell::RefCell;

thread_local! {
    pub(crate) static LOGGER_CAPTURE: RefCell<Option<String>> = const { RefCell::new(None) };
}

pub(crate) static LOGGER_INITIALIZED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub struct PactLogger<W, F> {
    wrapped: W,
    level: log::LevelFilter,
    fmt: F,
}

pub const fn setup() -> PactLogger<impl Log + 'static, impl Fn(&mut String, &log::Record)> {
    PactLogger {
        wrapped: NopLogger,
        level: log::LevelFilter::Trace,
        fmt: default_pact_log_formatter,
    }
}

impl PactLogger<NopLogger, fn(&mut String, &log::Record)> {
    pub const fn new() -> Self {
        Self {
            wrapped: NopLogger,
            level: log::LevelFilter::Trace,
            fmt: default_pact_log_formatter,
        }
    }
}
impl<W, F> PactLogger<W, F> {
    pub fn wrap<T>(self, wrapped: T) -> PactLogger<T, F>
    where
        T: Log + 'static,
    {
        let Self {
            wrapped: _,
            level,
            fmt,
        } = self;
        PactLogger {
            wrapped,
            level,
            fmt,
        }
    }
    pub fn level(mut self, level: log::LevelFilter) -> Self {
        self.level = level;
        self
    }
    pub fn fmt<T>(self, fmt: T) -> PactLogger<W, T>
    where
        T: Fn(&mut String, &log::Record) + Send + Sync + 'static,
    {
        let Self {
            wrapped,
            level,
            fmt: _,
        } = self;
        PactLogger {
            wrapped,
            level,
            fmt,
        }
    }
    pub fn init(self) -> Result<(), log::SetLoggerError>
    where
        Self: Log + 'static,
    {
        let result = log::set_logger(Box::leak(Box::new(self)));
        if result.is_ok() {
            LOGGER_INITIALIZED.store(true, std::sync::atomic::Ordering::SeqCst);
        }
        result
    }
    pub unsafe fn init_racy(self) -> Result<(), log::SetLoggerError>
    where
        Self: Log + 'static,
    {
        let result = log::set_logger_racy(Box::leak(Box::new(self)));
        if result.is_ok() {
            LOGGER_INITIALIZED.store(true, std::sync::atomic::Ordering::SeqCst);
        }
        result
    }
}

#[derive(Clone, Copy)]
struct NopLogger;
impl Log for NopLogger {
    fn enabled(&self, _: &log::Metadata) -> bool {
        false
    }
    fn log(&self, _: &log::Record) {}
    fn flush(&self) {}
}

impl<W, F> Log for PactLogger<W, F>
where
    W: Log + 'static,
    F: Fn(&mut String, &log::Record) + Send + Sync + 'static,
{
    fn log(&self, record: &log::Record) {
        if record.level().to_level_filter() <= self.level {
            LOGGER_CAPTURE.with_borrow_mut(|x: &mut Option<String>| {
                if let Some(buffer) = x {
                    (self.fmt)(buffer, record);
                }
            });
        }
        self.wrapped.log(record)
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        self.wrapped.enabled(metadata) || metadata.level().to_level_filter() <= self.level
    }
    fn flush(&self) {
        self.wrapped.flush()
    }
}

fn default_pact_log_formatter(buffer: &mut String, record: &log::Record) {
    use std::fmt::Write;
    let _ = write!(
        buffer,
        "{:5} [{}:{}] {}\n",
        record.level(),
        record.module_path().unwrap_or("?"),
        record.line().unwrap_or(0),
        record.args()
    );
}

/// executes func, while capturing any logs it made during the call, if called inside another call to logger_capture, it just calls the func normally and adds nothing to the buffer
pub fn capture<T>(buffer: &mut String, func: impl FnOnce() -> T) -> T {
    let start_result = LOGGER_CAPTURE.with_borrow_mut(|x: &mut Option<String>| {
        if x.is_none() {
            *x = Some(std::mem::take(buffer));
            Ok(())
        } else {
            Err(())
        }
    });
    if start_result.is_err() {
        return func();
    }
    let result = func();
    LOGGER_CAPTURE.with_borrow_mut(|x: &mut Option<String>| {
        let Some(x) = x.take() else { unreachable!() };
        *buffer = x;
    });
    result
}

/// same as logger_capture, but polls a future instead of calling a function
pub async fn capture_async<T>(
    buffer: &mut String,
    future: impl std::future::Future<Output = T>,
) -> T {
    let mut future = std::pin::pin!(future);
    std::future::poll_fn(|cx| capture(buffer, || future.as_mut().poll(cx))).await
}
