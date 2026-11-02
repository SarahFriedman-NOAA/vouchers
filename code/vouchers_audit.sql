-- =============================================================================
  -- AUDIT TABLE + TRIGGER FOR RACE_DATA.VOUCHERS
-- =============================================================================
  -- Run as RACE_DATA (or a user who can create tables/sequences/triggers in that
                       -- schema). Run the sections in order. Written for Oracle 11g+ (sequence, not an
                                                                                       -- identity column). I could not run this against Oracle from here, so test it in
-- a dev schema / copy of the table first.
--
  -- What it records, per row changed in VOUCHERS:
  --   * I / U / D, timestamp, DB user, OS user, host, IP, module, client info
--   * full OLD_* and NEW_* row images (OLD is NULL on insert, NEW on delete)
--   * CHANGED_COLUMNS (updates only), e.g. 'NEW_ID,TAXONOMIST'
--   * UPDATEs that change nothing are NOT logged
--
  -- Not captured: TRUNCATE, DROP/ALTER (DDL), anyone who disables the trigger.
-- If the VOUCHERS columns change, the audit table and trigger must be updated.
-- =============================================================================
  
  
  -- -----------------------------------------------------------------------------
  -- 0. Check the column types (the audit table copies them automatically in step 2)
-- -----------------------------------------------------------------------------
  -- SELECT column_name, data_type, data_length, nullable
-- FROM   all_tab_columns
-- WHERE  owner = 'RACE_DATA' AND table_name = 'VOUCHERS'
-- ORDER  BY column_id;


-- -----------------------------------------------------------------------------
  -- 1. Sequence for the audit primary key
-- -----------------------------------------------------------------------------
  CREATE SEQUENCE race_data.vouchers_audit_seq START WITH 1 INCREMENT BY 1 CACHE 20;


-- -----------------------------------------------------------------------------
  -- 2. Audit table
--    OLD_/NEW_ columns are created with CREATE TABLE AS SELECT ... WHERE 1 = 0 so
--    they get exactly the same datatypes/lengths as VOUCHERS. This matters: if an
--    audit column were too small, the trigger would fail and block the voucher
--    write itself.
-- -----------------------------------------------------------------------------
  CREATE TABLE race_data.vouchers_audit AS
SELECT
CAST(NULL AS NUMBER)           AS audit_id,
CAST(NULL AS VARCHAR2(1))      AS action,           -- I / U / D
CAST(NULL AS TIMESTAMP(6))     AS audit_ts,
CAST(NULL AS VARCHAR2(128))    AS db_user,
CAST(NULL AS VARCHAR2(128))    AS os_user,
CAST(NULL AS VARCHAR2(256))    AS host,
CAST(NULL AS VARCHAR2(64))     AS ip_address,
CAST(NULL AS VARCHAR2(128))    AS module,
CAST(NULL AS VARCHAR2(64))     AS client_info,      -- set from R, see step 6
CAST(NULL AS VARCHAR2(4000))   AS changed_columns,
hauljoin             AS old_hauljoin,
new_id               AS old_new_id,
new_species_code     AS old_new_species_code,
catalog_num          AS old_catalog_num,
region               AS old_region,
cruise               AS old_cruise,
year                 AS old_year,
vessel_id            AS old_vessel_id,
haul                 AS old_haul,
voucher              AS old_voucher,
field_id             AS old_field_id,
field_species_code   AS old_field_species_code,
n_specimens          AS old_n_specimens,
standard_length_mm   AS old_standard_length_mm,
weight_kg            AS old_weight_kg,
preservative         AS old_preservative,
collected_by         AS old_collected_by,
id_date              AS old_id_date,
taxonomist           AS old_taxonomist,
notes                AS old_notes,
hauljoin             AS new_hauljoin,
new_id               AS new_new_id,
new_species_code     AS new_new_species_code,
catalog_num          AS new_catalog_num,
region               AS new_region,
cruise               AS new_cruise,
year                 AS new_year,
vessel_id            AS new_vessel_id,
haul                 AS new_haul,
voucher              AS new_voucher,
field_id             AS new_field_id,
field_species_code   AS new_field_species_code,
n_specimens          AS new_n_specimens,
standard_length_mm   AS new_standard_length_mm,
weight_kg            AS new_weight_kg,
preservative         AS new_preservative,
collected_by         AS new_collected_by,
id_date              AS new_id_date,
taxonomist           AS new_taxonomist,
notes                AS new_notes
FROM race_data.vouchers
WHERE 1 = 0;

ALTER TABLE race_data.vouchers_audit MODIFY (audit_id NOT NULL, action NOT NULL, audit_ts NOT NULL);
ALTER TABLE race_data.vouchers_audit ADD CONSTRAINT vouchers_audit_pk PRIMARY KEY (audit_id);
ALTER TABLE race_data.vouchers_audit ADD CONSTRAINT vouchers_audit_action_ck CHECK (action IN ('I','U','D'));
CREATE INDEX race_data.vouchers_audit_ts_ix ON race_data.vouchers_audit (audit_ts);

COMMENT ON TABLE race_data.vouchers_audit IS 'Row-level change log for RACE_DATA.VOUCHERS, maintained by trigger TRG_VOUCHERS_AUDIT. Do not edit.';


-- -----------------------------------------------------------------------------
  -- 3. Trigger
--    Deliberately does NOT swallow errors: if the audit insert fails, the voucher
--    change fails too, so there are no unaudited changes.
-- -----------------------------------------------------------------------------
  CREATE OR REPLACE TRIGGER race_data.trg_vouchers_audit
AFTER INSERT OR UPDATE OR DELETE ON race_data.vouchers
FOR EACH ROW
DECLARE
v_action  VARCHAR2(1);
v_changed VARCHAR2(4000);

-- NULL-safe "did the value change?" helpers, overloaded by datatype
FUNCTION diff(a NUMBER, b NUMBER) RETURN BOOLEAN IS
BEGIN
IF a IS NULL AND b IS NULL THEN RETURN FALSE;
ELSIF a IS NULL OR b IS NULL THEN RETURN TRUE;
ELSE RETURN a <> b;
END IF;
END;

FUNCTION diff(a VARCHAR2, b VARCHAR2) RETURN BOOLEAN IS
BEGIN
IF a IS NULL AND b IS NULL THEN RETURN FALSE;
ELSIF a IS NULL OR b IS NULL THEN RETURN TRUE;
ELSE RETURN a <> b;
END IF;
END;

FUNCTION diff(a DATE, b DATE) RETURN BOOLEAN IS
BEGIN
IF a IS NULL AND b IS NULL THEN RETURN FALSE;
ELSIF a IS NULL OR b IS NULL THEN RETURN TRUE;
ELSE RETURN a <> b;
END IF;
END;
BEGIN
IF INSERTING THEN
v_action := 'I';
ELSIF UPDATING THEN
v_action := 'U';
ELSE
v_action := 'D';
END IF;

IF UPDATING THEN
IF diff(:OLD.hauljoin, :NEW.hauljoin) THEN v_changed := v_changed || 'HAULJOIN,'; END IF;
IF diff(:OLD.new_id, :NEW.new_id) THEN v_changed := v_changed || 'NEW_ID,'; END IF;
IF diff(:OLD.new_species_code, :NEW.new_species_code) THEN v_changed := v_changed || 'NEW_SPECIES_CODE,'; END IF;
IF diff(:OLD.catalog_num, :NEW.catalog_num) THEN v_changed := v_changed || 'CATALOG_NUM,'; END IF;
IF diff(:OLD.region, :NEW.region) THEN v_changed := v_changed || 'REGION,'; END IF;
IF diff(:OLD.cruise, :NEW.cruise) THEN v_changed := v_changed || 'CRUISE,'; END IF;
IF diff(:OLD.year, :NEW.year) THEN v_changed := v_changed || 'YEAR,'; END IF;
IF diff(:OLD.vessel_id, :NEW.vessel_id) THEN v_changed := v_changed || 'VESSEL_ID,'; END IF;
IF diff(:OLD.haul, :NEW.haul) THEN v_changed := v_changed || 'HAUL,'; END IF;
IF diff(:OLD.voucher, :NEW.voucher) THEN v_changed := v_changed || 'VOUCHER,'; END IF;
IF diff(:OLD.field_id, :NEW.field_id) THEN v_changed := v_changed || 'FIELD_ID,'; END IF;
IF diff(:OLD.field_species_code, :NEW.field_species_code) THEN v_changed := v_changed || 'FIELD_SPECIES_CODE,'; END IF;
IF diff(:OLD.n_specimens, :NEW.n_specimens) THEN v_changed := v_changed || 'N_SPECIMENS,'; END IF;
IF diff(:OLD.standard_length_mm, :NEW.standard_length_mm) THEN v_changed := v_changed || 'STANDARD_LENGTH_MM,'; END IF;
IF diff(:OLD.weight_kg, :NEW.weight_kg) THEN v_changed := v_changed || 'WEIGHT_KG,'; END IF;
IF diff(:OLD.preservative, :NEW.preservative) THEN v_changed := v_changed || 'PRESERVATIVE,'; END IF;
IF diff(:OLD.collected_by, :NEW.collected_by) THEN v_changed := v_changed || 'COLLECTED_BY,'; END IF;
IF diff(:OLD.id_date, :NEW.id_date) THEN v_changed := v_changed || 'ID_DATE,'; END IF;
IF diff(:OLD.taxonomist, :NEW.taxonomist) THEN v_changed := v_changed || 'TAXONOMIST,'; END IF;
IF diff(:OLD.notes, :NEW.notes) THEN v_changed := v_changed || 'NOTES,'; END IF;

-- UPDATE that changed nothing: don't log it
    IF v_changed IS NULL THEN
      RETURN;
    END IF;
    v_changed := RTRIM(v_changed, ',');
  END IF;

  INSERT INTO race_data.vouchers_audit (
    audit_id, action, audit_ts, db_user, os_user, host, ip_address, module,
    client_info, changed_columns,
    old_hauljoin, old_new_id, old_new_species_code, old_catalog_num, old_region, old_cruise, old_year, old_vessel_id, old_haul, old_voucher, old_field_id, old_field_species_code, old_n_specimens, old_standard_length_mm, old_weight_kg, old_preservative, old_collected_by, old_id_date, old_taxonomist, old_notes,
    new_hauljoin, new_new_id, new_new_species_code, new_catalog_num, new_region, new_cruise, new_year, new_vessel_id, new_haul, new_voucher, new_field_id, new_field_species_code, new_n_specimens, new_standard_length_mm, new_weight_kg, new_preservative, new_collected_by, new_id_date, new_taxonomist, new_notes
  ) VALUES (
    race_data.vouchers_audit_seq.NEXTVAL,
    v_action,
    SYSTIMESTAMP,
    SUBSTR(SYS_CONTEXT('USERENV', 'SESSION_USER'), 1, 128),
    SUBSTR(SYS_CONTEXT('USERENV', 'OS_USER'),      1, 128),
    SUBSTR(SYS_CONTEXT('USERENV', 'HOST'),         1, 256),
    SUBSTR(SYS_CONTEXT('USERENV', 'IP_ADDRESS'),   1, 64),
    SUBSTR(SYS_CONTEXT('USERENV', 'MODULE'),       1, 128),
    SUBSTR(SYS_CONTEXT('USERENV', 'CLIENT_INFO'),  1, 64),
    v_changed,
    :OLD.hauljoin, :OLD.new_id, :OLD.new_species_code, :OLD.catalog_num, :OLD.region, :OLD.cruise, :OLD.year, :OLD.vessel_id, :OLD.haul, :OLD.voucher, :OLD.field_id, :OLD.field_species_code, :OLD.n_specimens, :OLD.standard_length_mm, :OLD.weight_kg, :OLD.preservative, :OLD.collected_by, :OLD.id_date, :OLD.taxonomist, :OLD.notes,
    :NEW.hauljoin, :NEW.new_id, :NEW.new_species_code, :NEW.catalog_num, :NEW.region, :NEW.cruise, :NEW.year, :NEW.vessel_id, :NEW.haul, :NEW.voucher, :NEW.field_id, :NEW.field_species_code, :NEW.n_specimens, :NEW.standard_length_mm, :NEW.weight_kg, :NEW.preservative, :NEW.collected_by, :NEW.id_date, :NEW.taxonomist, :NEW.notes
  );
END;
/


-- -----------------------------------------------------------------------------
-- 4. Permissions
--    The trigger writes to the audit table as the schema owner, so people who
--    edit VOUCHERS need no privileges on VOUCHERS_AUDIT. Give others read-only.
-- -----------------------------------------------------------------------------
-- GRANT SELECT ON race_data.vouchers_audit TO <your_read_role_or_user>;
-- Do NOT grant INSERT/UPDATE/DELETE on it.


-- -----------------------------------------------------------------------------
-- 5. Test (dev first, or run and ROLLBACK; audit rows roll back with the change)
-- -----------------------------------------------------------------------------
-- UPDATE race_data.vouchers SET notes = notes WHERE ROWNUM = 1;        -- no-op: no audit row
-- UPDATE race_data.vouchers SET taxonomist = 'TEST' WHERE ROWNUM = 1;  -- logs a 'U'
-- SELECT audit_id, action, audit_ts, db_user, changed_columns, old_taxonomist, new_taxonomist
-- FROM (SELECT * FROM race_data.vouchers_audit ORDER BY audit_id DESC)
-- WHERE ROWNUM <= 5;
-- ROLLBACK;


-- -----------------------------------------------------------------------------
-- 6. Tagging changes made from R (optional but handy)
--    In R, before writing:
--      RODBC::sqlQuery(channel,
--        "BEGIN DBMS_APPLICATION_INFO.SET_CLIENT_INFO('vouchers_to_oracle.R'); END;")
--    CLIENT_INFO then shows up in the audit table, so you can tell script
--    updates from manual SQL Developer edits.
-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- 7. Handy queries
-- -----------------------------------------------------------------------------
-- History of one voucher:
-- SELECT audit_ts, action, db_user, client_info, changed_columns
-- FROM   race_data.vouchers_audit
-- WHERE  COALESCE(new_cruise, old_cruise)         = 202501
--   AND  COALESCE(new_vessel_id, old_vessel_id)   = 148
--   AND  COALESCE(new_haul, old_haul)             = 12
-- ORDER  BY audit_id;
--
-- Everything changed in the last 7 days, by user:
-- SELECT db_user, action, COUNT(*)
-- FROM   race_data.vouchers_audit
-- WHERE  audit_ts > SYSTIMESTAMP - INTERVAL '7' DAY
-- GROUP  BY db_user, action;
--
-- Every change to a given column:
-- SELECT audit_ts, old_new_id, new_new_id, db_user
-- FROM   race_data.vouchers_audit
-- WHERE  action = 'U' AND changed_columns LIKE '%NEW_ID%';
--
-- Undelete / revert: the OLD_* columns hold the full prior row, so an
-- INSERT ... SELECT old_* FROM vouchers_audit WHERE audit_id = :id restores it.