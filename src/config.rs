use std::{borrow::Cow, path::Path};

use crate::BoxedError;

#[derive(Debug, Clone, serde::Deserialize)]
pub struct Config {
    pub http: HttpConfig,
    pub database: DatabaseConfig,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct HttpConfig {
    #[serde(default)]
    pub addrs: Vec<std::net::SocketAddr>,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct DatabaseConfig {
    pub name: Cow<'static, str>,
    pub host: std::net::IpAddr,
    pub port: u16,
    pub user: Cow<'static, str>,
    pub pass: Cow<'static, str>,
}

impl Config {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, BoxedError> {
        use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6};
        let config = std::fs::read_to_string(path)?;
        let mut config = toml::from_str::<Config>(&config)?;
        if config.http.addrs.is_empty() {
            config.http.addrs = vec![
                SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 80)),
                SocketAddr::V6(SocketAddrV6::new(Ipv6Addr::UNSPECIFIED, 80, 0, 0)),
            ];
        }
        Ok(config)
    }
}

impl DatabaseConfig {
    #[allow(unused)]
    pub const fn new() -> Self {
        Self {
            name: Cow::Borrowed("DATABASE.FDB"),
            host: std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST),
            port: 3050,
            user: Cow::Borrowed("SYSDBA"),
            pass: Cow::Borrowed("masterkey"),
        }
    }
}
