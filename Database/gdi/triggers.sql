CREATE OR REPLACE TRIGGER FoneClinicaFK
  BEFORE INSERT ON FONE_CLINICA
  FOR EACH ROW
DECLARE
  value CLINICA.ID%TYPE;
BEGIN
  IF :NEW.ID IS NULL THEN
    SELECT ID INTO value FROM
      ( SELECT ID FROM CLINICA
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.ID := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER FoneFuncionarioFK
  BEFORE INSERT ON FONE_FUNCIONARIO
  FOR EACH ROW
DECLARE
  value FUNCIONARIO.CPF%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO value FROM
      ( SELECT CPF FROM FUNCIONARIO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.CPF := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER FonePacienteFK
  BEFORE INSERT ON FONE_PACIENTE
  FOR EACH ROW
DECLARE
  value PACIENTE.CPF%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO value FROM
      ( SELECT CPF FROM PACIENTE
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.CPF := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER EmailFuncionarioFK
  BEFORE INSERT ON EMAIL_FUNCIONARIO
  FOR EACH ROW
DECLARE
  value FUNCIONARIO.CPF%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO value FROM
      ( SELECT CPF FROM FUNCIONARIO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.CPF := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER EmailPacienteFK
  BEFORE INSERT ON EMAIL_PACIENTE
  FOR EACH ROW
DECLARE
  value PACIENTE.CPF%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO value FROM
      ( SELECT CPF FROM PACIENTE
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.CPF := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER idFuncionarioFK
  BEFORE INSERT ON FUNCIONARIO
  FOR EACH ROW
DECLARE
  value CLINICA.ID%TYPE;
BEGIN
  IF :NEW.ID_CLINICA IS NULL THEN
    SELECT ID INTO value FROM
      ( SELECT ID FROM CLINICA
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.ID_CLINICA := value;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER MedicosFK
  BEFORE INSERT ON MEDICOS
  FOR EACH ROW
DECLARE
  value FUNCIONARIO.CPF%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO value 
    FROM FUNCIONARIO
    WHERE CPF NOT IN (
      SELECT CPF 
      FROM MEDICOS
    ) AND ROWNUM = 1;
    :NEW.CPF := value;
  END IF;

  IF :NEW.SUPERVISOR IS NULL THEN
    SELECT CPF INTO value FROM
      ( SELECT CPF FROM MEDICOS
      ORDER BY dbms_random.value )
      WHERE rownum = 1;
    
    IF value IS NOT NULL THEN
      :NEW.SUPERVISOR := value;
    END IF;
  END IF;
END;
/

-- Aqui está dando problema:
-- Provavelmente está ferindo o príncipio
-- De 1 : N

CREATE OR REPLACE TRIGGER PagaFK
  BEFORE INSERT ON PAGA_FUNCIONARIO
  FOR EACH ROW
DECLARE
  new_cpf FUNCIONARIO.CPF%TYPE;
  new_id CLINICA.ID%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF INTO new_cpf 
    FROM FUNCIONARIO
    WHERE CPF NOT IN (
      SELECT CPF 
      FROM MEDICOS
    ) AND ROWNUM = 1;

    :NEW.CPF := new_cpf;
  END IF;

  IF :NEW.ID_CLINICA IS NULL THEN
    SELECT ID INTO new_id FROM
      ( SELECT ID FROM CLINICA
      ORDER BY dbms_random.value )
      WHERE rownum = 1;
    
    IF new_id IS NOT NULL THEN
      :NEW.ID_CLINICA := new_id;
    END IF;
  END IF;
END;
/


CREATE OR REPLACE TRIGGER tipoBeneFK
  BEFORE INSERT ON TIPO_BENEFICIO
  FOR EACH ROW
DECLARE
  cod BENEFICIO.COD%TYPE;
BEGIN
  IF :NEW.COD_BENEFICIO IS NULL THEN
    SELECT COD INTO cod
      FROM ( SELECT COD 
      FROM BENEFICIO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.COD_BENEFICIO := cod;
  END IF;
END;
/

CREATE OR REPLACE TRIGGER FormularioFK
  BEFORE INSERT ON FORMULARIO
  FOR EACH ROW
DECLARE
  cod AGENDAMENTO.COD%TYPE;
BEGIN
  IF :NEW.COD IS NULL THEN
    SELECT COD INTO cod
      FROM ( SELECT COD 
      FROM AGENDAMENTO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.COD := cod;
  END IF;
END;
/

CREATE OR REPLACE PROCEDURER FillTernary
DECLARE
  clinica CLINICA.ID_CLINICA%TYPE;
  terceirizado EMPRESA_TERCEIRIZADA.CNPJ%TYPE;
  contrato CONTRATO.COD%TYPE; 
  contagem INT;
BEGIN
  -- Verify how many inserts to do:

  SELECT COUNT(*) INTO contagem FROM CLINICA, CONTRATO, EMPRESA_TERCEIRIZADA;

  FOR i IN 1..contagem LOOP
    SELECT ID_CLINICA INTO clinica FROM
      ( SELECT ID_CLINICA FROM CLINICA
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    SELECT CNPJ INTO terceirizado FROM
      ( SELECT CNPJ FROM EMPRESA_TERCEIRIZADA
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    SELECT COD INTO contrato FROM
      ( SELECT COD FROM CONTRATO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    INSERT INTO SERVICO (ID_SERVICO, CNPJ, COD) 
      VALUES (clinica, terceirizado, contrato)
  END LOOP

END;
/

CREATE OR REPLACE PROCEDURER addRecepcionista

CREATE OR REPLACE PROCEDURER addFazServico

CREATE OR REPLACE TRIGGER beneficioFK
  BEFORE INSERT ON BENEFICIO
  FOR EACH ROW
DECLARE
  new_cpf FUNCIONARIO.CPF%TYPE;
  new_id CLINICA.ID%TYPE;
BEGIN
  IF :NEW.CPF IS NULL THEN
    SELECT CPF, ID_CLINICA 
      INTO new_cpf, new_id 
      FROM ( SELECT CPF, ID_CLINICA 
      FROM PAGA_FUNCIONARIO
      ORDER BY dbms_random.value )
      WHERE rownum = 1;

    :NEW.CPF := new_cpf;
    :NEW.ID_CLINICA := new_id;
  END IF;
END;
/