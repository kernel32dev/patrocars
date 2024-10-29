mod app;
mod config;
mod db;

type BoxedError = Box<dyn std::error::Error + Send + Sync + 'static>;

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), BoxedError> {
    server_utils::log::setup()
        .wrap(simple_logger::SimpleLogger::new().with_local_timestamps())
        .init()
        .unwrap();

    log::set_max_level(log::LevelFilter::Debug);

    #[cfg(not(debug_assertions))]
    std::env::current_exe().and_then(|mut x| {
        x.pop();
        std::env::set_current_dir(x)
    })?;

    let config = config::Config::load("patrocars.toml")?;

    db::configure(config.database);

    db::connect().await?;

    log::info!("Testando conecção ao banco");

    log::info!("Escutando em {:?}", config.http.addrs);

    server_utils::server::setup(|req| async {
        let path = req.path.clone();
        app::app(req, path.as_slice()).await
    })
    .addr(config.http.addrs.as_slice())
    .shutdown_on_ctrl_c()
    .enable_websocket()
    .serve()
    .await?;

    Ok(())
}
