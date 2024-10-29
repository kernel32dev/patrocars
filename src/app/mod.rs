use intuple::{Intuple, IntupleRef};
use serde::{Deserialize, Serialize};
use server_utils::{Request, Response};

use crate::db;

pub async fn app(req: Request, path: &[&[u8]]) -> Result<(), Response> {
    match path {
        [b"assets", path @ ..] if req.is_get() => Err(Response::dir("assets", path).await),
        [b""] | [b"index.html"] if req.is_get() => Err(Response::html(html_temple::use_template!(
            "templates/index.html"
        ))),
        [b"montadora", rest @ ..] => montadora(req, rest).await,
        [b"modelo", rest @ ..] => modelo(req, rest).await,
        [b"veiculo", rest @ ..] => veiculo(req, rest).await,
        _ => Err(Response::not_found()),
    }
}

async fn montadora(mut req: Request, path: &[&[u8]]) -> Result<(), Response> {
    #[derive(Debug, Serialize, Deserialize, Intuple)]
    struct Montadora {
        /// ULID as ASCII char (26)
        #[serde(default)]
        id: String,
        /// Manufacturer name
        nome: String,
        /// Country
        pais: String,
        /// Year of foundation
        ano_fundacao: i32,
    }
    match path {
        [] | [b""] if req.is_get() => {
            let rows = db::connect()
                .await?
                .query("SELECT id, nome, pais, ano_fundacao FROM Montadora", ())
                .await?
                .into_iter()
                .map(Montadora::from_tuple)
                .collect::<Vec<_>>();
            Err(Response::html(html_temple::use_template!(
                "templates/montadora.html",
                rows
            )))
        }
        [b"new"] if req.is_get() => Err(Response::html(html_temple::use_template!(
            "templates/montadora-new.html",
        ))),
        [b"new"] if req.is_post() => {
            let mut montadora = req.take_form::<Montadora>().await?;
            montadora.id = ulid::Ulid::new().to_string();
            db::connect()
                .await?
                .execute(
                    "INSERT INTO Montadora (id, nome, pais, ano_fundacao) VALUES (?, ?, ?, ?)",
                    montadora.into_tuple(),
                )
                .await?;
            Err(Response::found("/montadora"))
        }
        [b"delete", ulid] if req.is_post() => {
            let ulid = ulid.to_vec();
            db::connect()
                .await?
                .execute("DELETE FROM Montadora WHERE id = ?", (ulid,))
                .await?;
            Err(Response::found("/montadora"))
        }
        _ => Err(Response::not_found()),
    }
}

async fn modelo(mut req: Request, path: &[&[u8]]) -> Result<(), Response> {
    #[derive(Debug, Serialize, Deserialize, Intuple)]
    struct ModeloVeiculo {
        /// ULID as ASCII char (26)
        #[serde(default)]
        id: String,
        /// Model name
        nome: String,
        /// Reference to Montadora ID
        montadora_id: String,
        /// Reference price
        valor_referencia: f64,
        /// Engine capacity (e.g., 2.0, 3.5)
        motorizacao: f32,
        /// Turbo indicator
        #[serde(default)]
        turbo: i16,
        /// Automatic transmission indicator
        #[serde(default)]
        automatico: i16,
    }
    match path {
        [] | [b""] if req.is_get() => {
            let rows = db::connect()
                .await?
                .query("SELECT id, nome, (SELECT M.nome FROM Montadora M WHERE MV.montadora_id = M.id) AS montadora_id, valor_referencia, motorizacao, turbo, automatico FROM ModeloVeiculo MV", ())
                .await?
                .into_iter()
                .map(ModeloVeiculo::from_tuple)
                .collect::<Vec<_>>();
            Err(Response::html(html_temple::use_template!(
                "templates/modelo.html",
                rows
            )))
        }
        [b"new"] if req.is_get() => {
            let montadoras: Vec<(String, String)> = db::connect()
                .await?
                .query("SELECT id, nome FROM Montadora", ())
                .await?;

            Err(Response::html(html_temple::use_template!(
                "templates/modelo-new.html",
                montadoras
            )))
        }
        [b"new"] if req.is_post() => {
            let mut modelo = req.take_form::<ModeloVeiculo>().await?;
            modelo.id = ulid::Ulid::new().to_string();
            db::connect()
                .await?
                .execute(
                    "INSERT INTO ModeloVeiculo (id, nome, montadora_id, valor_referencia, motorizacao, turbo, automatico) VALUES (?, ?, ?, ?, ?, ?, ?)",
                    modelo.into_tuple(),
                )
                .await?;
            Err(Response::found("/modelo"))
        }
        [b"delete", ulid] if req.is_post() => {
            let ulid = ulid.to_vec();
            db::connect()
                .await?
                .execute("DELETE FROM ModeloVeiculo WHERE id = ?", (ulid,))
                .await?;
            Err(Response::found("/modelo"))
        }
        _ => Err(Response::not_found()),
    }
}

async fn veiculo(mut req: Request, path: &[&[u8]]) -> Result<(), Response> {
    #[derive(Debug, Serialize, Deserialize, Intuple)]
    struct Veiculo {
        /// ULID as ASCII char (26)
        #[serde(default)]
        id: String,
        /// Reference to ModeloVeiculo ID
        modelo_id: String,
        /// Color
        cor: String,
        /// Year of manufacture
        ano_fabricacao: i32,
        /// Model year
        ano_modelo: i32,
        /// Price
        valor: f64,
        /// License plate
        placa: String,
        /// Sold indicator
        #[serde(default)]
        vendido: i16,
    }
    match path {
        [] | [b""] if req.is_get() => {
            let rows = db::connect()
                .await?
                .query("SELECT id, COALESCE((SELECT MV.nome FROM ModeloVeiculo MV WHERE V.modelo_id = MV.id), '?') AS modelo_id, cor, ano_fabricacao, ano_modelo, valor, placa, vendido FROM Veiculo V", ())
                .await?
                .into_iter()
                .map(Veiculo::from_tuple)
                .collect::<Vec<_>>();
            Err(Response::html(html_temple::use_template!(
                "templates/veiculo.html",
                rows
            )))
        }
        [b"new"] if req.is_get() => {
            let modelos: Vec<(String, String)> = db::connect()
                .await?
                .query("SELECT id, nome FROM ModeloVeiculo", ())
                .await?;
            Err(Response::html(html_temple::use_template!(
                "templates/veiculo-new.html",
                modelos
            )))
        }
        [b"new"] if req.is_post() => {
            let mut veiculo = req.take_form::<Veiculo>().await?;
            veiculo.id = ulid::Ulid::new().to_string();
            db::connect()
                .await?
                .execute(
                    "INSERT INTO Veiculo (id, modelo_id, cor, ano_fabricacao, ano_modelo, valor, placa, vendido) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                    veiculo.into_tuple(),
                )
                .await?;
            Err(Response::found("/veiculo"))
        }
        [b"delete", ulid] if req.is_post() => {
            let ulid = ulid.to_vec();
            db::connect()
                .await?
                .execute("DELETE FROM Veiculo WHERE id = ?", (ulid,))
                .await?;
            Err(Response::found("/veiculo"))
        }
        _ => Err(Response::not_found()),
    }
}
