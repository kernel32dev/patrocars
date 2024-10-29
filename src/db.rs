use std::sync::{Arc, RwLock};

use rsfbclient::{Execute, PureRustConnectionBuilder, Queryable};

use crate::{config::DatabaseConfig, BoxedError};

static DATABASE_BUILDER: RwLock<Option<PureRustConnectionBuilder>> = RwLock::new(None);

pub fn configure(config: DatabaseConfig) {
    let mut builder = rsfbclient::builder_pure_rust();
    builder
        .db_name(config.name)
        .host(config.host.to_string())
        .port(config.port)
        .user(config.user)
        .pass(config.pass);
    *DATABASE_BUILDER.write().unwrap() = Some(builder);
}

pub struct Connection {
    connection: Option<rsfbclient::SimpleConnection>,
}

pub async fn connect() -> Result<Connection, BoxedError> {
    tokio::task::spawn_blocking(|| {
        DATABASE_BUILDER
            .read()
            .unwrap()
            .as_ref()
            .expect("database not configured")
            .connect()
            .map(|x| Connection {
                connection: Some(x.into()),
            })
            .map_err(From::from)
    })
    .await
    .unwrap()
}

#[allow(unused)]
impl Connection {
    async fn asyncify<T>(
        &mut self,
        conn: impl for<'a> FnOnce(&'a mut rsfbclient::SimpleConnection) -> T + Send + 'static,
    ) -> T
    where
        T: Send + 'static,
    {
        let mut connection = self.connection.take().unwrap();
        let (connection, result) = tokio::task::spawn_blocking(move || {
            let result = conn(&mut connection);
            (connection, result)
        })
        .await
        .unwrap();
        self.connection = Some(connection);
        result
    }

    pub async fn begin_transaction(&mut self) -> Result<(), BoxedError> {
        self.asyncify(|conn| Ok(conn.begin_transaction()?)).await
    }
    pub async fn commit(&mut self) -> Result<(), BoxedError> {
        self.asyncify(|conn| Ok(conn.commit()?)).await
    }
    pub async fn rollback(&mut self) -> Result<(), BoxedError> {
        self.asyncify(|conn| Ok(conn.rollback()?)).await
    }

    pub async fn query<R, P>(
        &mut self,
        sql: impl Into<Arc<str>>,
        params: P,
    ) -> Result<Vec<R>, QueryError>
    where
        P: rsfbclient::IntoParams + Send + 'static,
        R: rsfbclient::FromRow + Send + 'static,
    {
        let sql = sql.into();
        let query = sql.clone();
        self.asyncify(move |conn| Ok(conn.query(&query, params)?))
            .await
            .map_err(|error| QueryError { sql, error })
    }

    pub async fn query_first<R, P>(
        &mut self,
        sql: impl Into<Arc<str>>,
        params: P,
    ) -> Result<Option<R>, QueryError>
    where
        P: rsfbclient::IntoParams + Send + 'static,
        R: rsfbclient::FromRow + Send + 'static,
    {
        let sql = sql.into();
        let query = sql.clone();
        self.asyncify(move |conn| Ok(conn.query_first(&query, params)?))
            .await
            .map_err(|error| QueryError { sql, error })
    }
    pub async fn execute<P>(
        &mut self,
        sql: impl Into<Arc<str>>,
        params: P,
    ) -> Result<usize, QueryError>
    where
        P: rsfbclient::IntoParams + Send + 'static,
    {
        let sql = sql.into();
        let query = sql.clone();
        self.asyncify(move |conn| Ok(conn.execute(&query, params)?))
            .await
            .map_err(|error| QueryError { sql, error })
    }

    pub async fn execute_returnable<R, P>(
        &mut self,
        sql: impl Into<Arc<str>>,
        params: P,
    ) -> Result<R, QueryError>
    where
        P: rsfbclient::IntoParams + Send + 'static,
        R: rsfbclient::FromRow + Send + 'static,
    {
        let sql = sql.into();
        let query = sql.clone();
        self.asyncify(move |conn| Ok(conn.execute_returnable(&query, params)?))
            .await
            .map_err(|error| QueryError { sql, error })
    }
}

#[derive(Debug)]
pub struct QueryError {
    #[allow(unused)]
    pub sql: Arc<str>,
    #[allow(unused)]
    pub error: BoxedError,
}
impl std::fmt::Display for QueryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}
impl std::error::Error for QueryError {}
impl server_utils::ReplyError for QueryError {
    fn error_status(&self) -> server_utils::Status {
        server_utils::Status::INTERNAL_SERVER_ERROR
    }
}
