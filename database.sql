CREATE DOMAIN BOOLEAN AS SMALLINT;

-- Table Montadora
CREATE TABLE Montadora (
    id CHAR(26) CHARACTER SET ASCII NOT NULL, -- ULID as 26-character ASCII
    nome VARCHAR(100),                        -- Name of the manufacturer
    pais VARCHAR(50),                         -- Country
    ano_fundacao INTEGER,                     -- Year of foundation
    CONSTRAINT pk_montadora PRIMARY KEY (id)
);

-- Table ModeloVeiculo
CREATE TABLE ModeloVeiculo (
    id CHAR(26) CHARACTER SET ASCII NOT NULL,  -- ULID as 26-character ASCII
    nome VARCHAR(100),                         -- Model name
    montadora_id CHAR(26) CHARACTER SET ASCII, -- Reference to Montadora ID
    valor_referencia NUMERIC(15, 2),           -- Reference price
    motorizacao NUMERIC(5, 2),                 -- Engine capacity (e.g., 2.0, 3.5)
    turbo BOOLEAN,                             -- Turbo indicator
    automatico BOOLEAN,                        -- Automatic transmission indicator
    CONSTRAINT pk_modeloveiculo PRIMARY KEY (id),
    CONSTRAINT fk_modeloveiculo_montadora FOREIGN KEY (montadora_id) REFERENCES Montadora (id)
);

-- Table Veiculo
CREATE TABLE Veiculo (
    id CHAR(26) CHARACTER SET ASCII NOT NULL, -- ULID as 26-character ASCII
    modelo_id CHAR(26) CHARACTER SET ASCII,   -- Reference to ModeloVeiculo ID
    cor VARCHAR(30),                          -- Color
    ano_fabricacao INTEGER,                   -- Year of manufacture
    ano_modelo INTEGER,                       -- Model year
    valor NUMERIC(15, 2),                     -- Price
    placa VARCHAR(10),                        -- License plate
    vendido BOOLEAN,                          -- Sold indicator
    CONSTRAINT pk_veiculo PRIMARY KEY (id),
    CONSTRAINT fk_veiculo_modeloveiculo FOREIGN KEY (modelo_id) REFERENCES ModeloVeiculo (id)
);

-- Inserindo dados de teste na tabela Montadora
INSERT INTO Montadora (id, nome, pais, ano_fundacao) VALUES ('01ARZ3NDEKTSV4RRFFQ69G5FAV', 'Toyota', 'Japão', 1937);
INSERT INTO Montadora (id, nome, pais, ano_fundacao) VALUES ('01ARZ3NDEKTSV4RRFFQ69G5FAN', 'Ford', 'Estados Unidos', 1903);
INSERT INTO Montadora (id, nome, pais, ano_fundacao) VALUES ('01ARZ3NDEKTSV4RRFFQ69G5FAQ', 'Volkswagen', 'Alemanha', 1937);

-- Inserindo dados de teste na tabela ModeloVeiculo
INSERT INTO ModeloVeiculo (id, nome, montadora_id, valor_referencia, motorizacao, turbo, automatico) 
VALUES ('01ARZ4NDEKTSV4RRFFQ69G5FBB', 'Corolla', '01ARZ3NDEKTSV4RRFFQ69G5FAV', 90000.00, 2.0, 0, 1);
INSERT INTO ModeloVeiculo (id, nome, montadora_id, valor_referencia, motorizacao, turbo, automatico) 
VALUES ('01ARZ4NDEKTSV4RRFFQ69G5FBC', 'Mustang', '01ARZ3NDEKTSV4RRFFQ69G5FAN', 300000.00, 5.0, 1, 1);
INSERT INTO ModeloVeiculo (id, nome, montadora_id, valor_referencia, motorizacao, turbo, automatico) 
VALUES ('01ARZ4NDEKTSV4RRFFQ69G5FBD', 'Golf', '01ARZ3NDEKTSV4RRFFQ69G5FAQ', 120000.00, 1.4, 1, 0);

-- Inserindo dados de teste na tabela Veiculo
INSERT INTO Veiculo (id, modelo_id, cor, ano_fabricacao, ano_modelo, valor, placa, vendido)
VALUES ('01ARZ5NDEKTSV4RRFFQ69G5FCA', '01ARZ4NDEKTSV4RRFFQ69G5FBB', 'Branco', 2022, 2023, 95000.00, 'ABC1D23', 0);
INSERT INTO Veiculo (id, modelo_id, cor, ano_fabricacao, ano_modelo, valor, placa, vendido)
VALUES ('01ARZ5NDEKTSV4RRFFQ69G5FCB', '01ARZ4NDEKTSV4RRFFQ69G5FBC', 'Vermelho', 2021, 2021, 310000.00, 'XYZ2E45', 1);
INSERT INTO Veiculo (id, modelo_id, cor, ano_fabricacao, ano_modelo, valor, placa, vendido)
VALUES ('01ARZ5NDEKTSV4RRFFQ69G5FCC', '01ARZ4NDEKTSV4RRFFQ69G5FBD', 'Azul', 2020, 2021, 125000.00, 'LMN3F67', 0);
