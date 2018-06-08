--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.6
-- Dumped by pg_dump version 10.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: ussr; Type: DATABASE; Schema: -; Owner: ks
--

CREATE DATABASE ussr WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'en_US.UTF-8' LC_CTYPE = 'en_US.UTF-8';


ALTER DATABASE ussr OWNER TO ks;

\connect ussr

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: archive_service(); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION archive_service() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  INSERT INTO service_archived (id_service, service_code, service_discount_amount
    , service_discount_percent, client, notes
    , created_datetime, created_by, is_confirmed
    , confirmed_datetime, confirmed_by, company_branch
    , is_deleted)

  VALUES (OLD.id_service, OLD.service_code, OLD.service_discount_amount
    , OLD.service_discount_percent, OLD.client, OLD.notes
    , OLD.created_datetime, OLD.created_by, OLD.is_confirmed
    , OLD.confirmed_datetime, OLD.confirmed_by, OLD.company_branch
    , TRUE);

  RETURN OLD;

END;
$$;


ALTER FUNCTION public.archive_service() OWNER TO ks;

--
-- Name: get_calendar(character varying, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_calendar(_service character varying, _start timestamp without time zone, _end timestamp without time zone) RETURNS TABLE(from_time timestamp without time zone, to_time timestamp without time zone, free_workers text, free_machines text, free_locations text)
    LANGUAGE plpgsql
    AS $$
DECLARE
  ts_len            INTEGER;
  sr_duration       INTEGER;
  empty_sr          RECORD;
  worker_required   INTEGER; --BOOLEAN;
  location_required INTEGER;
  machine_required  INTEGER; --BOOLEAN;

BEGIN
  ts_len := (SELECT time_slot_minutes
             FROM time_slot_params
             LIMIT 1);

  sr_duration := (SELECT SUM(time_minutes)
                  FROM se_requirement
                  WHERE se_requirement.service_code = _service);

  worker_required := (SELECT CASE WHEN MAX(s_r.worker_ability) IS NULL
    THEN 0
                             ELSE 1
                             END
                      FROM se_requirement s_r
                      WHERE s_r.service_code = _service
                      LIMIT 1);
--raise notice 'wr %', worker_required;
  machine_required := (SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
    THEN 0
                              ELSE 1
                              END
                       FROM se_requirement s_r
                       WHERE s_r.service_code = _service
                       LIMIT 1);
--raise notice 'mr %', machine_required;
  location_required := (SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
    THEN 0
                               ELSE 1
                               END
                        FROM se_requirement s_r
                        WHERE s_r.service_code = _service
                        LIMIT 1);
--raise notice 'lr %', location_required;
  RETURN QUERY

  WITH
    --       empty_sr AS (
    --         SELECT
    --           MAX(s_r.worker_ability) worker_ability,
    --           MAX(s_r.machine_type)   machine_type,
    --           MAX(s_r.location_type)  location_type
    --         FROM se_requirement s_r
    --         WHERE s_r.service_code = _service
    --     ),

      sr_workers AS (
        SELECT w.id_worker
        FROM se_requirement s_r
          JOIN wo_ability w_a
            ON w_a.worker_ability = s_r.worker_ability
          JOIN worker w
            ON w_a.worker = w.id_worker
        WHERE s_r.service_code = _service
              AND w.active = TRUE
    ),

      sr_machines AS (
        SELECT m.id_machine
--           CASE WHEN machine_required = 0 THEN -1
--             ELSE m.id_machine
--             END id_machine
        FROM se_requirement s_r
          JOIN machine m
            ON s_r.machine_type = m.machine_type
        WHERE s_r.service_code = _service
              AND m.is_operational = TRUE
      UNION ALL
          SELECT -1
      WHERE location_required = 0
    ),

      sr_location AS (
        SELECT l.id_location
--           CASE WHEN location_required = 0 THEN -1
--             ELSE l.id_location
--             END id_location
        FROM se_requirement s_r
          JOIN location l
            ON s_r.location_type = l.location_type
        WHERE s_r.service_code = _service
              AND l.is_operational = TRUE
      UNION ALL
          SELECT -1
      WHERE location_required = 0
    ),

      timeslots AS (
        SELECT
          w_c.id_workday_calendar date,
          gen_ser.timeslot_start
        FROM workday_calendar w_c
          , LATERAL (SELECT generate_series :: TIMESTAMP timeslot_start
                     FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
                     , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
                     , INTERVAL '1min' * ts_len)) gen_ser
        WHERE w_c.is_workday = TRUE
        ORDER BY date, timeslot_start
    )

  SELECT
    ts2.timeslot_start,
    ts2.timeslot_start + INTERVAL '1min' * ts_len,
    STRING_AGG(DISTINCT (CAST(id_worker AS TEXT)), ', ')   free_workers,
    STRING_AGG(DISTINCT (CAST(id_machine AS TEXT)), ', ')  free_machines,
    STRING_AGG(DISTINCT (CAST(id_location AS TEXT)), ', ') free_locations

  FROM timeslots ts1
    LEFT JOIN timeslots ts2
      ON ts2.timeslot_start = ts1.timeslot_start
    CROSS JOIN sr_workers  srw
    CROSS JOIN sr_machines srm
    CROSS JOIN sr_location srl

  WHERE
    --(NOT worker_required
    --OR
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.worker = srw.id_worker
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
    )
    --)

    AND
    (machine_required = 0
     OR
     NOT EXISTS(
         SELECT 'x'
         FROM resources_usage r_u
         WHERE r_u.machine = srm.id_machine
               AND r_u.finish_timestamp > ts2.timeslot_start
               AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
     )
    )

    AND
    (worker_required = 0
     OR
     NOT EXISTS(
         SELECT 'x'
         FROM resources_usage r_u
         WHERE r_u.location = srl.id_location
               AND r_u.finish_timestamp > ts2.timeslot_start
               AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
     )
    )

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

  GROUP BY ts2.timeslot_start
  ORDER BY ts2.timeslot_start ASC
  LIMIT 10000;
END;

$$;


ALTER FUNCTION public.get_calendar(_service character varying, _start timestamp without time zone, _end timestamp without time zone) OWNER TO ks;

--
-- Name: get_calendar_old(timestamp without time zone, timestamp without time zone, character varying); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_calendar_old(_start timestamp without time zone, _end timestamp without time zone, _service character varying) RETURNS TABLE(from_time timestamp without time zone, to_time timestamp without time zone, free_workers text, free_machines text, free_locations text)
    LANGUAGE plpgsql
    AS $$

DECLARE
  ts_len      INTEGER;
  sr_duration INTEGER;

BEGIN
  ts_len := (SELECT time_slot_minutes
             FROM time_slot_params
             LIMIT 1);

  sr_duration := (SELECT SUM(time_minutes)
                  FROM se_requirement
                  WHERE se_requirement.service_code = _service);

  RETURN QUERY

  WITH
      sr_workers AS (
        SELECT w.id_worker
        FROM se_requirement s_r
          JOIN wo_ability w_a
            ON w_a.worker_ability = s_r.worker_ability
          JOIN worker w
            ON w_a.worker = w.id_worker
        WHERE s_r.service_code = _service
              AND w.active = TRUE
    ),

      sr_machines AS (
        SELECT m.id_machine
        FROM se_requirement s_r
          JOIN machine m
            ON s_r.machine_type = m.machine_type
        WHERE s_r.service_code = _service
              AND m.is_operational = TRUE
    ),

      sr_location AS (
        SELECT l.id_location
        FROM se_requirement s_r
          JOIN location l
            ON s_r.location_type = l.location_type
        WHERE s_r.service_code = _service
              AND l.is_operational = TRUE
    ),

      timeslots AS (
        SELECT
          w_c.id_workday_calendar date,
          gen_ser.timeslot_start
        FROM workday_calendar w_c
          , LATERAL (SELECT generate_series :: TIMESTAMP timeslot_start
                     FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
                     , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
                     , INTERVAL '1min' * ts_len)) gen_ser
        WHERE w_c.is_workday = TRUE
        ORDER BY date, timeslot_start
    )

  SELECT
    ts2.timeslot_start,
    ts2.timeslot_start + INTERVAL '1min' * ts_len,
    STRING_AGG(DISTINCT (CAST(id_worker AS TEXT)), ', ')   free_workers,
    STRING_AGG(DISTINCT (CAST(id_machine AS TEXT)), ', ')  free_machines,
    STRING_AGG(DISTINCT (CAST(id_location AS TEXT)), ', ') free_locations

  FROM timeslots ts1
    LEFT JOIN timeslots ts2
      ON ts2.timeslot_start = ts1.timeslot_start
    CROSS JOIN sr_workers srw
    CROSS JOIN sr_machines srm
    CROSS JOIN sr_location srl

  WHERE
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.worker = srw.id_worker
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.machine = srm.id_machine
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.location = srl.id_location
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

  GROUP BY ts2.timeslot_start
  ORDER BY ts2.timeslot_start ASC;
END;

$$;


ALTER FUNCTION public.get_calendar_old(_start timestamp without time zone, _end timestamp without time zone, _service character varying) OWNER TO ks;

--
-- Name: get_calendar_old2(character varying, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_calendar_old2(_service character varying, _start timestamp without time zone, _end timestamp without time zone) RETURNS TABLE(from_time timestamp without time zone, to_time timestamp without time zone, free_workers text, free_machines text, free_locations text)
    LANGUAGE plpgsql
    AS $$
DECLARE
  ts_len      INTEGER;
  sr_duration INTEGER;

BEGIN
  ts_len := (SELECT time_slot_minutes
             FROM time_slot_params
             LIMIT 1);

  sr_duration := (SELECT SUM(time_minutes)
                  FROM se_requirement
                  WHERE se_requirement.service_code = _service);

  RETURN QUERY

  WITH
      sr_workers AS (
        SELECT w.id_worker
        FROM se_requirement s_r
          JOIN wo_ability w_a
            ON w_a.worker_ability = s_r.worker_ability
          JOIN worker w
            ON w_a.worker = w.id_worker
        WHERE s_r.service_code = _service
              AND w.active = TRUE
    ),

      sr_machines AS (
        SELECT m.id_machine
        FROM se_requirement s_r
          JOIN machine m
            ON s_r.machine_type = m.machine_type
        WHERE s_r.service_code = _service
              AND m.is_operational = TRUE
    ),

      sr_location AS (
        SELECT l.id_location
        FROM se_requirement s_r
          JOIN location l
            ON s_r.location_type = l.location_type
        WHERE s_r.service_code = _service
              AND l.is_operational = TRUE
    ),

      timeslots AS (
        SELECT
          w_c.id_workday_calendar date,
          gen_ser.timeslot_start
        FROM workday_calendar w_c
          , LATERAL (SELECT generate_series :: TIMESTAMP timeslot_start
                     FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
                     , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
                     , INTERVAL '1min' * ts_len)) gen_ser
        WHERE w_c.is_workday = TRUE
        ORDER BY date, timeslot_start
    )

  SELECT
    ts2.timeslot_start,
    ts2.timeslot_start + INTERVAL '1min' * ts_len,
    STRING_AGG(DISTINCT (CAST(id_worker AS TEXT)), ', ')   free_workers,
    STRING_AGG(DISTINCT (CAST(id_machine AS TEXT)), ', ')  free_machines,
    STRING_AGG(DISTINCT (CAST(id_location AS TEXT)), ', ') free_locations

  FROM timeslots ts1
    LEFT JOIN timeslots ts2
      ON ts2.timeslot_start = ts1.timeslot_start
    CROSS JOIN sr_workers srw
    CROSS JOIN sr_machines srm
    CROSS JOIN sr_location srl

  WHERE
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.worker = srw.id_worker
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.machine = srm.id_machine
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.location = srl.id_location
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

  GROUP BY ts2.timeslot_start
  ORDER BY ts2.timeslot_start ASC
  LIMIT 10000;
END;

$$;


ALTER FUNCTION public.get_calendar_old2(_service character varying, _start timestamp without time zone, _end timestamp without time zone) OWNER TO ks;

--
-- Name: get_calendar_test(character varying, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_calendar_test(_service character varying, _start timestamp without time zone, _end timestamp without time zone) RETURNS TABLE(from_time timestamp without time zone, to_time timestamp without time zone, free_workers text, free_machines text, free_locations text)
    LANGUAGE plpgsql
    AS $$
DECLARE
  ts_len            INTEGER;
  sr_duration       INTEGER;
  empty_sr          RECORD;
  worker_required   INTEGER; --BOOLEAN;
  location_required INTEGER;
  machine_required  INTEGER; --BOOLEAN;

BEGIN
  ts_len := (SELECT time_slot_minutes
             FROM time_slot_params
             LIMIT 1);

  sr_duration := (SELECT SUM(time_minutes)
                  FROM se_requirement
                  WHERE se_requirement.service_code = _service);

  worker_required := (SELECT CASE WHEN MAX(s_r.worker_ability) IS NULL
    THEN 0
                             ELSE 1
                             END
                      FROM se_requirement s_r
                      WHERE s_r.service_code = _service
                      LIMIT 1);
--raise notice 'wr %', worker_required;
  machine_required := (SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
    THEN 0
                              ELSE 1
                              END
                       FROM se_requirement s_r
                       WHERE s_r.service_code = _service
                       LIMIT 1);
--raise notice 'mr %', machine_required;
  location_required := (SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
    THEN 0
                               ELSE 1
                               END
                        FROM se_requirement s_r
                        WHERE s_r.service_code = _service
                        LIMIT 1);
--raise notice 'lr %', location_required;
  RETURN QUERY

  WITH
    --       empty_sr AS (
    --         SELECT
    --           MAX(s_r.worker_ability) worker_ability,
    --           MAX(s_r.machine_type)   machine_type,
    --           MAX(s_r.location_type)  location_type
    --         FROM se_requirement s_r
    --         WHERE s_r.service_code = _service
    --     ),

      sr_workers AS (
        SELECT w.id_worker
        FROM se_requirement s_r
          JOIN wo_ability w_a
            ON w_a.worker_ability = s_r.worker_ability
          JOIN worker w
            ON w_a.worker = w.id_worker
        WHERE s_r.service_code = _service
              AND w.active = TRUE
    ),

      sr_machines AS (
        SELECT m.id_machine
--           CASE WHEN machine_required = 0 THEN -1
--             ELSE m.id_machine
--             END id_machine
        FROM se_requirement s_r
          JOIN machine m
            ON s_r.machine_type = m.machine_type
        WHERE s_r.service_code = _service
              AND m.is_operational = TRUE
      UNION ALL
          SELECT -1
      WHERE location_required = 0
    ),

      sr_location AS (
        SELECT l.id_location
--           CASE WHEN location_required = 0 THEN -1
--             ELSE l.id_location
--             END id_location
        FROM se_requirement s_r
          JOIN location l
            ON s_r.location_type = l.location_type
        WHERE s_r.service_code = _service
              AND l.is_operational = TRUE
      UNION ALL
          SELECT -1
      WHERE location_required = 0
    ),

      timeslots AS (
        SELECT
          w_c.id_workday_calendar date,
          gen_ser.timeslot_start
        FROM workday_calendar w_c
          , LATERAL (SELECT generate_series :: TIMESTAMP timeslot_start
                     FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
                     , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
                     , INTERVAL '1min' * ts_len)) gen_ser
        WHERE w_c.is_workday = TRUE
        ORDER BY date, timeslot_start
    )

  SELECT
    ts2.timeslot_start,
    ts2.timeslot_start + INTERVAL '1min' * ts_len,
    STRING_AGG(DISTINCT (CAST(id_worker AS TEXT)), ', ')   free_workers,
    STRING_AGG(DISTINCT (CAST(id_machine AS TEXT)), ', ')  free_machines,
    STRING_AGG(DISTINCT (CAST(id_location AS TEXT)), ', ') free_locations

  FROM timeslots ts1
    LEFT JOIN timeslots ts2
      ON ts2.timeslot_start = ts1.timeslot_start
    CROSS JOIN sr_workers  srw
    CROSS JOIN sr_machines srm
    CROSS JOIN sr_location srl

  WHERE
    --(NOT worker_required
    --OR
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.worker = srw.id_worker
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
    )
    --)

    AND
    (machine_required = 0
     OR
     NOT EXISTS(
         SELECT 'x'
         FROM resources_usage r_u
         WHERE r_u.machine = srm.id_machine
               AND r_u.finish_timestamp > ts2.timeslot_start
               AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
     )
    )

    AND
    (worker_required = 0
     OR
     NOT EXISTS(
         SELECT 'x'
         FROM resources_usage r_u
         WHERE r_u.location = srl.id_location
               AND r_u.finish_timestamp > ts2.timeslot_start
               AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration
     )
    )

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

  GROUP BY ts2.timeslot_start
  ORDER BY ts2.timeslot_start ASC
  LIMIT 10000;
END;

$$;


ALTER FUNCTION public.get_calendar_test(_service character varying, _start timestamp without time zone, _end timestamp without time zone) OWNER TO ks;

--
-- Name: get_calendar_with_worker(character varying, integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_calendar_with_worker(_service character varying, _worker integer, _start timestamp without time zone, _end timestamp without time zone) RETURNS TABLE(from_time timestamp without time zone, to_time timestamp without time zone, free_workers text, free_machines text, free_locations text)
    LANGUAGE plpgsql
    AS $$
DECLARE
  ts_len      INTEGER;
  sr_duration INTEGER;

BEGIN
  ts_len := (SELECT time_slot_minutes
             FROM time_slot_params
             LIMIT 1);

  sr_duration := (SELECT SUM(time_minutes)
                  FROM se_requirement
                  WHERE se_requirement.service_code = _service);

  RETURN QUERY

  WITH
    /*
        sr_workers AS (
          SELECT w.id_worker
          FROM se_requirement s_r
            JOIN wo_ability w_a
              ON w_a.worker_ability = s_r.worker_ability
            JOIN worker w
              ON w_a.worker = w.id_worker
          WHERE s_r.service_code = _service
                AND w.active = TRUE
      ),
  */
      sr_machines AS (
        SELECT m.id_machine
        FROM se_requirement s_r
          JOIN machine m
            ON s_r.machine_type = m.machine_type
        WHERE s_r.service_code = _service
              AND m.is_operational = TRUE
    ),

      sr_location AS (
        SELECT l.id_location
        FROM se_requirement s_r
          JOIN location l
            ON s_r.location_type = l.location_type
        WHERE s_r.service_code = _service
              AND l.is_operational = TRUE
    ),

      timeslots AS (
        SELECT
          w_c.id_workday_calendar date,
          gen_ser.timeslot_start
        FROM workday_calendar w_c
          , LATERAL (SELECT generate_series :: TIMESTAMP timeslot_start
                     FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
                     , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
                     , INTERVAL '1min' * ts_len)) gen_ser
        WHERE w_c.is_workday = TRUE
        ORDER BY date, timeslot_start
    )

  SELECT
    ts2.timeslot_start,
    ts2.timeslot_start + INTERVAL '1min' * ts_len,
    CAST(_worker AS TEXT) free_workers,
    --STRING_AGG(DISTINCT (CAST(id_worker AS TEXT)), ', ')   free_workers,
    STRING_AGG(DISTINCT (CAST(id_machine AS TEXT)), ', ')  free_machines,
    STRING_AGG(DISTINCT (CAST(id_location AS TEXT)), ', ') free_locations

  FROM timeslots ts1
    LEFT JOIN timeslots ts2
      ON ts2.timeslot_start = ts1.timeslot_start
    --CROSS JOIN sr_workers srw
    CROSS JOIN sr_machines srm
    CROSS JOIN sr_location srl

  WHERE
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.worker = _worker
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.machine = srm.id_machine
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )
    AND
    NOT EXISTS(
        SELECT 'x'
        FROM resources_usage r_u
        WHERE r_u.location = srl.id_location
              AND r_u.finish_timestamp > ts2.timeslot_start
              AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration --ts_len --'1h'
    )

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

  GROUP BY ts2.timeslot_start
  ORDER BY ts2.timeslot_start ASC
  LIMIT 10000;
END;

$$;


ALTER FUNCTION public.get_calendar_with_worker(_service character varying, _worker integer, _start timestamp without time zone, _end timestamp without time zone) OWNER TO ks;

--
-- Name: get_worker_calendar(integer, date, date); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION get_worker_calendar(_worker integer, _start date, _end date) RETURNS TABLE(fromtime timestamp without time zone, totime timestamp without time zone, location text, machine text, client integer, service integer, service_code text)
    LANGUAGE plpgsql
    AS $$
BEGIN
  RETURN QUERY
  SELECT
    min(r_u.start_timestamp),
    max(r_u.finish_timestamp),
    string_agg(cast(r_u.location AS TEXT), ', '),
    string_agg(cast(r_u.machine AS TEXT), ', '),
    min(s.client),
    s.id_service,
    min(s.service_code)

  FROM service s
    JOIN resources_usage r_u
      ON s.id_service = r_u.service
  WHERE r_u.worker = _worker
        AND cast(r_u.finish_timestamp AS DATE) >= _start
        AND cast(r_u.start_timestamp AS DATE) <= _end
  GROUP BY s.id_service
  ORDER BY min(r_u.start_timestamp) ASC;

END;
$$;


ALTER FUNCTION public.get_worker_calendar(_worker integer, _start date, _end date) OWNER TO ks;

--
-- Name: prepare_calendar(date, date, time without time zone, time without time zone, character varying); Type: FUNCTION; Schema: public; Owner: ks
--

CREATE FUNCTION prepare_calendar(_start_day date, _end_day date, _start_time time without time zone, _end_time time without time zone, _company_branch character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$

BEGIN

  INSERT INTO workday_calendar (id_workday_calendar, is_workday
    , work_start, work_end, company_branch)

    SELECT
      gsd day_date,
      CASE WHEN (EXTRACT(ISODOW FROM gsd) IN (6, 7))
        THEN FALSE
      ELSE TRUE
      END workday,
      _start_time,
      _end_time,
      _company_branch

    FROM generate_series(_start_day
    , _end_day
    , '1 day' :: INTERVAL) gsd;

END;
$$;


ALTER FUNCTION public.prepare_calendar(_start_day date, _end_day date, _start_time time without time zone, _end_time time without time zone, _company_branch character varying) OWNER TO ks;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: address; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE address (
    id_address integer NOT NULL,
    prefered_contact_type character varying(10),
    email character varying(50),
    phone character varying(20),
    street character varying(100),
    house_no character varying(5),
    apartment_no character varying(5),
    city character varying(50),
    zip character varying(10),
    country character varying(100),
    notes character varying(400)
);


ALTER TABLE address OWNER TO ks;

--
-- Name: auth_group; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_group (
    id integer NOT NULL,
    name character varying(80) NOT NULL
);


ALTER TABLE auth_group OWNER TO ks;

--
-- Name: auth_group_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_group_id_seq OWNER TO ks;

--
-- Name: auth_group_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_group_id_seq OWNED BY auth_group.id;


--
-- Name: auth_group_permissions; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_group_permissions (
    id integer NOT NULL,
    group_id integer NOT NULL,
    permission_id integer NOT NULL
);


ALTER TABLE auth_group_permissions OWNER TO ks;

--
-- Name: auth_group_permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_group_permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_group_permissions_id_seq OWNER TO ks;

--
-- Name: auth_group_permissions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_group_permissions_id_seq OWNED BY auth_group_permissions.id;


--
-- Name: auth_permission; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_permission (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    content_type_id integer NOT NULL,
    codename character varying(100) NOT NULL
);


ALTER TABLE auth_permission OWNER TO ks;

--
-- Name: auth_permission_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_permission_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_permission_id_seq OWNER TO ks;

--
-- Name: auth_permission_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_permission_id_seq OWNED BY auth_permission.id;


--
-- Name: auth_user; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_user (
    id integer NOT NULL,
    password character varying(128) NOT NULL,
    last_login timestamp with time zone,
    is_superuser boolean NOT NULL,
    username character varying(150) NOT NULL,
    first_name character varying(30) NOT NULL,
    last_name character varying(30) NOT NULL,
    email character varying(254) NOT NULL,
    is_staff boolean NOT NULL,
    is_active boolean NOT NULL,
    date_joined timestamp with time zone NOT NULL
);


ALTER TABLE auth_user OWNER TO ks;

--
-- Name: auth_user_groups; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_user_groups (
    id integer NOT NULL,
    user_id integer NOT NULL,
    group_id integer NOT NULL
);


ALTER TABLE auth_user_groups OWNER TO ks;

--
-- Name: auth_user_groups_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_user_groups_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_user_groups_id_seq OWNER TO ks;

--
-- Name: auth_user_groups_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_user_groups_id_seq OWNED BY auth_user_groups.id;


--
-- Name: auth_user_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_user_id_seq OWNER TO ks;

--
-- Name: auth_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_user_id_seq OWNED BY auth_user.id;


--
-- Name: auth_user_user_permissions; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE auth_user_user_permissions (
    id integer NOT NULL,
    user_id integer NOT NULL,
    permission_id integer NOT NULL
);


ALTER TABLE auth_user_user_permissions OWNER TO ks;

--
-- Name: auth_user_user_permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE auth_user_user_permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE auth_user_user_permissions_id_seq OWNER TO ks;

--
-- Name: auth_user_user_permissions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE auth_user_user_permissions_id_seq OWNED BY auth_user_user_permissions.id;


--
-- Name: cl_blocked_reason_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_blocked_reason_dict (
    id_cl_blocked_reason_dict character varying(10) NOT NULL,
    blocked_reason_name character varying(200)
);


ALTER TABLE cl_blocked_reason_dict OWNER TO ks;

--
-- Name: cl_communication_log; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_communication_log (
    id_cl_communication_log integer NOT NULL,
    client integer,
    communication_reason character varying(10),
    service integer,
    contact_type character varying(10) NOT NULL,
    contact_address character varying(100),
    message_body text,
    minutes_before_action integer,
    notes integer,
    created_datetime timestamp without time zone DEFAULT now(),
    created_by character varying(10) DEFAULT "current_user"(),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE cl_communication_log OWNER TO ks;

--
-- Name: cl_communication_log_id_cl_communication_log_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_communication_log_id_cl_communication_log_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_communication_log_id_cl_communication_log_seq OWNER TO ks;

--
-- Name: cl_communication_log_id_cl_communication_log_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_communication_log_id_cl_communication_log_seq OWNED BY cl_communication_log.id_cl_communication_log;


--
-- Name: cl_communication_reason; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_communication_reason (
    id_client_communication_reason character varying(10) NOT NULL,
    reason_name character varying(100)
);


ALTER TABLE cl_communication_reason OWNER TO ks;

--
-- Name: cl_discount; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_discount (
    id_cl_discount integer NOT NULL,
    client integer NOT NULL,
    discount character varying(10) NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying NOT NULL
);


ALTER TABLE cl_discount OWNER TO ks;

--
-- Name: cl_discount_id_cl_discount_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_discount_id_cl_discount_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_discount_id_cl_discount_seq OWNER TO ks;

--
-- Name: cl_discount_id_cl_discount_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_discount_id_cl_discount_seq OWNED BY cl_discount.id_cl_discount;


--
-- Name: cl_params; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_params (
    max_debt money NOT NULL,
    allow_new_no_contact boolean NOT NULL,
    default_reminder_sms_minutes integer NOT NULL,
    default_reminder_email_minutes integer NOT NULL,
    default_finished_info_sms boolean NOT NULL,
    default_finished_info_email boolean NOT NULL,
    max_worktime_wo_conf_minutes integer NOT NULL,
    default_currency character varying(10) NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying,
    cl_params_id integer NOT NULL
);


ALTER TABLE cl_params OWNER TO ks;

--
-- Name: cl_params_cl_params_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_params_cl_params_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_params_cl_params_id_seq OWNER TO ks;

--
-- Name: cl_params_cl_params_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_params_cl_params_id_seq OWNED BY cl_params.cl_params_id;


--
-- Name: cl_payment; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_payment (
    id_cl_payment integer NOT NULL,
    is_closed boolean,
    payment_name character varying(200),
    client integer NOT NULL,
    is_invoice boolean,
    contact integer NOT NULL,
    invoice_voucher character varying(15),
    payment_sum money,
    paid_amount money,
    currency character varying(10) DEFAULT 'PLN'::character varying NOT NULL,
    paid_datetime timestamp without time zone,
    posted_datetime timestamp without time zone,
    due_date date,
    notes character varying(400),
    created_datetime timestamp without time zone DEFAULT now(),
    created_by character varying(10),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE cl_payment OWNER TO ks;

--
-- Name: cl_payment_id_cl_payment_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_payment_id_cl_payment_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_payment_id_cl_payment_seq OWNER TO ks;

--
-- Name: cl_payment_id_cl_payment_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_payment_id_cl_payment_seq OWNED BY cl_payment.id_cl_payment;


--
-- Name: cl_payment_line; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_payment_line (
    id_cl_payment_line integer NOT NULL,
    payment integer NOT NULL,
    service integer NOT NULL,
    text_on_invoice character varying(200) NOT NULL,
    qty real,
    final_price money,
    currency character varying(10) NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE cl_payment_line OWNER TO ks;

--
-- Name: cl_payment_line_id_cl_payment_line_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_payment_line_id_cl_payment_line_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_payment_line_id_cl_payment_line_seq OWNER TO ks;

--
-- Name: cl_payment_line_id_cl_payment_line_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_payment_line_id_cl_payment_line_seq OWNED BY cl_payment_line.id_cl_payment_line;


--
-- Name: cl_unconfirmed; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE cl_unconfirmed (
    cl_unconfirmed integer NOT NULL,
    first_name character varying(20),
    last_name character varying(20),
    client_name character varying(200),
    created_datetime timestamp without time zone DEFAULT now(),
    ip_address character varying(20),
    email character varying(50),
    phone character varying(20),
    default_company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE cl_unconfirmed OWNER TO ks;

--
-- Name: cl_unconfirmed_cl_unconfirmed_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE cl_unconfirmed_cl_unconfirmed_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cl_unconfirmed_cl_unconfirmed_seq OWNER TO ks;

--
-- Name: cl_unconfirmed_cl_unconfirmed_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE cl_unconfirmed_cl_unconfirmed_seq OWNED BY cl_unconfirmed.cl_unconfirmed;


--
-- Name: client; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE client (
    id_client integer NOT NULL,
    sex character(1),
    first_name character varying(20),
    last_name character varying(20),
    is_company boolean,
    client_name character varying(200),
    nip character varying(11),
    address integer,
    is_blocked boolean,
    blocked_reason_id character varying(10),
    blocked_notes character varying(400),
    default_invoice boolean,
    default_reminder_sms_minutes integer,
    is_confirmed boolean,
    is_rejected boolean,
    ip_address character varying(20),
    default_reminder_email_minutes integer,
    default_finished_info_sms integer,
    default_finished_info_email integer,
    client_discount_percent_sum real,
    notes character varying(400),
    default_company_branch character varying(10) DEFAULT 'main'::character varying,
    email character varying(50),
    phone character varying(20),
    prefered_contact_type character varying(10),
    client_user_login integer
);


ALTER TABLE client OWNER TO ks;

--
-- Name: client_block_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_block_v AS
 SELECT client.id_client,
    client.blocked_notes,
    client.blocked_reason_id
   FROM client;


ALTER TABLE client_block_v OWNER TO ks;

--
-- Name: client_blocked_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_blocked_list_v AS
 SELECT client.id_client,
    client.first_name,
    client.last_name,
    client.client_name,
    client.blocked_notes,
    cl_blocked_reason_dict.blocked_reason_name,
    sum(cl_payment.payment_sum) AS sum_payment_sum
   FROM ((client
     LEFT JOIN cl_payment ON ((client.id_client = cl_payment.client)))
     JOIN cl_blocked_reason_dict ON (((cl_blocked_reason_dict.id_cl_blocked_reason_dict)::text = (client.blocked_reason_id)::text)))
  WHERE (client.is_blocked = true)
  GROUP BY client.id_client, client.first_name, client.last_name, client.client_name, client.blocked_notes, cl_blocked_reason_dict.blocked_reason_name, cl_payment.is_closed
 HAVING (cl_payment.is_closed = false);


ALTER TABLE client_blocked_list_v OWNER TO ks;

--
-- Name: contact_type; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE contact_type (
    id_contact_type character varying(10) NOT NULL,
    contact_type_name character varying(100)
);


ALTER TABLE contact_type OWNER TO ks;

--
-- Name: se_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE se_dict (
    id_se_dict character varying(10) NOT NULL,
    se_dict_name character varying(200) NOT NULL,
    base_price numeric,
    location_type character varying(10),
    avg_time integer,
    continous boolean NOT NULL,
    notes character varying(400),
    se_group character varying(10)
);


ALTER TABLE se_dict OWNER TO ks;

--
-- Name: service; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE service (
    id_service integer NOT NULL,
    is_confirmed boolean,
    service_code character varying(10) NOT NULL,
    client integer NOT NULL,
    location integer,
    create_invoice boolean,
    service_discount_amount numeric,
    service_discount_percent real,
    min_start_datetime timestamp without time zone DEFAULT now(),
    planned_start timestamp without time zone,
    planned_end timestamp without time zone,
    real_start timestamp without time zone,
    real_end timestamp without time zone,
    reminder_sms_minutes integer,
    reminder_email_minutes integer,
    finished_info_sms boolean,
    finished_info_email boolean,
    notes character varying(400),
    created_datetime timestamp without time zone DEFAULT now(),
    created_by character varying(10) DEFAULT 'current_user'::character varying,
    confirmed_datetime timestamp without time zone,
    confirmed_by character varying(10),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE service OWNER TO ks;

--
-- Name: client_communication_log_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_communication_log_v AS
 SELECT cl_communication_reason.reason_name,
    client.first_name,
    client.last_name,
    client.client_name,
    contact_type.contact_type_name,
    address.email,
    address.phone,
    se_dict.se_dict_name,
    cl_communication_log.message_body,
    cl_communication_log.notes,
    cl_communication_log.created_by,
    cl_communication_log.created_datetime
   FROM ((((((cl_communication_log
     LEFT JOIN contact_type ON (((contact_type.id_contact_type)::text = (cl_communication_log.contact_type)::text)))
     JOIN address ON (((contact_type.id_contact_type)::text = (address.prefered_contact_type)::text)))
     LEFT JOIN client ON (((client.id_client = cl_communication_log.client) AND (address.id_address = client.address))))
     JOIN service ON ((service.id_service = cl_communication_log.service)))
     JOIN se_dict ON (((se_dict.id_se_dict)::text = (service.service_code)::text)))
     LEFT JOIN cl_communication_reason ON (((cl_communication_reason.id_client_communication_reason)::text = (cl_communication_log.communication_reason)::text)));


ALTER TABLE client_communication_log_v OWNER TO ks;

--
-- Name: client_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_details_v AS
 SELECT client.id_client,
    client.first_name,
    client.last_name,
    client.is_company,
    client.client_name,
    client.nip,
    client.sex,
    client.is_blocked,
    client.blocked_reason_id,
    client.blocked_notes,
    client.is_confirmed,
    client.is_rejected,
    client.default_invoice,
    client.default_reminder_sms_minutes,
    client.default_reminder_email_minutes,
    client.default_finished_info_sms,
    client.default_finished_info_email,
    client.client_discount_percent_sum,
    client.ip_address,
    client.notes AS client_notes,
    client.email,
    client.phone,
    address.prefered_contact_type,
    address.street,
    address.apartment_no,
    address.city,
    address.house_no,
    address.zip,
    address.country,
    address.notes AS contact_notes
   FROM (client
     LEFT JOIN address ON ((address.id_address = client.address)));


ALTER TABLE client_details_v OWNER TO ks;

--
-- Name: client_id_client_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE client_id_client_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE client_id_client_seq OWNER TO ks;

--
-- Name: client_id_client_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE client_id_client_seq OWNED BY client.id_client;


--
-- Name: client_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_list_v AS
 SELECT client.id_client,
    client.first_name,
    client.last_name,
    client.client_name,
    client.is_blocked,
    client.notes,
    sum(cl_payment.payment_sum) AS sum_payment_sum
   FROM (client
     LEFT JOIN cl_payment ON (((client.id_client = cl_payment.client) AND (cl_payment.is_closed = false))))
  WHERE ((client.is_confirmed = true) AND (client.is_rejected = false))
  GROUP BY client.id_client, client.first_name, client.last_name, client.client_name, client.is_blocked, client.notes;


ALTER TABLE client_list_v OWNER TO ks;

--
-- Name: client_not_yet_accepted_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_not_yet_accepted_list_v AS
 SELECT client.id_client,
    client.first_name,
    client.last_name,
    client.client_name,
    client.is_rejected,
    client.prefered_contact_type,
    client.email,
    client.phone,
    client.ip_address
   FROM client
  WHERE ((client.is_rejected = false) AND (client.is_confirmed = false));


ALTER TABLE client_not_yet_accepted_list_v OWNER TO ks;

--
-- Name: client_params_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_params_v AS
 SELECT cl_params.max_debt,
    cl_params.default_reminder_sms_minutes,
    cl_params.default_reminder_email_minutes,
    cl_params.default_finished_info_sms,
    cl_params.default_finished_info_email,
    cl_params.max_worktime_wo_conf_minutes,
    cl_params.default_currency,
    cl_params.allow_new_no_contact
   FROM cl_params;


ALTER TABLE client_params_v OWNER TO ks;

--
-- Name: client_payment_line_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_payment_line_v AS
 SELECT cl_payment_line.id_cl_payment_line,
    se_dict.se_dict_name,
    cl_payment_line.text_on_invoice,
    cl_payment_line.qty,
    cl_payment_line.final_price,
    se_dict.base_price,
    service.real_end,
    client.id_client,
    cl_payment.invoice_voucher
   FROM ((((cl_payment_line
     JOIN cl_payment ON ((cl_payment.id_cl_payment = cl_payment_line.payment)))
     JOIN client ON ((client.id_client = cl_payment.client)))
     JOIN service ON (((service.id_service = cl_payment_line.service) AND (client.id_client = service.client))))
     JOIN se_dict ON (((se_dict.id_se_dict)::text = (service.service_code)::text)));


ALTER TABLE client_payment_line_v OWNER TO ks;

--
-- Name: client_payment_list_all_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_payment_list_all_v AS
 SELECT cl_payment.id_cl_payment,
    cl_payment.is_closed,
    client.first_name,
    client.last_name,
    client.client_name,
    cl_payment.is_invoice,
    cl_payment.invoice_voucher,
    cl_payment.payment_name,
    cl_payment.payment_sum,
    cl_payment.posted_datetime,
    cl_payment.due_date,
    cl_payment.paid_datetime,
    cl_payment.notes
   FROM (cl_payment
     LEFT JOIN client ON ((client.id_client = cl_payment.client)));


ALTER TABLE client_payment_list_all_v OWNER TO ks;

--
-- Name: client_payment_list_closed_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_payment_list_closed_v AS
 SELECT cl_payment.id_cl_payment,
    cl_payment.is_closed,
    client.first_name,
    client.last_name,
    client.client_name,
    cl_payment.payment_name,
    cl_payment.is_invoice,
    cl_payment.invoice_voucher,
    cl_payment.payment_sum,
    cl_payment.posted_datetime,
    cl_payment.due_date,
    cl_payment.paid_datetime,
    cl_payment.notes
   FROM (cl_payment
     LEFT JOIN client ON ((client.id_client = cl_payment.client)))
  WHERE (cl_payment.is_closed = true);


ALTER TABLE client_payment_list_closed_v OWNER TO ks;

--
-- Name: client_payment_list_posted_unpaid_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_payment_list_posted_unpaid_v AS
 SELECT cl_payment.id_cl_payment,
    cl_payment.is_closed,
    client.first_name,
    client.last_name,
    client.client_name,
    cl_payment.payment_name,
    cl_payment.is_invoice,
    cl_payment.invoice_voucher,
    cl_payment.payment_sum,
    cl_payment.posted_datetime,
    cl_payment.due_date,
    cl_payment.paid_datetime,
    cl_payment.notes
   FROM (cl_payment
     LEFT JOIN client ON ((client.id_client = cl_payment.client)))
  WHERE ((cl_payment.posted_datetime IS NOT NULL) AND (cl_payment.paid_datetime IS NULL));


ALTER TABLE client_payment_list_posted_unpaid_v OWNER TO ks;

--
-- Name: client_payment_list_unposted_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW client_payment_list_unposted_v AS
 SELECT cl_payment.id_cl_payment,
    cl_payment.is_closed,
    client.first_name,
    client.last_name,
    client.client_name,
    cl_payment.payment_name,
    cl_payment.is_invoice,
    cl_payment.invoice_voucher,
    cl_payment.payment_sum,
    cl_payment.posted_datetime,
    cl_payment.due_date,
    cl_payment.paid_datetime,
    cl_payment.notes
   FROM (cl_payment
     LEFT JOIN client ON ((client.id_client = cl_payment.client)))
  WHERE (cl_payment.posted_datetime IS NULL);


ALTER TABLE client_payment_list_unposted_v OWNER TO ks;

--
-- Name: company_branch; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE company_branch (
    id_company_branch character varying(10) NOT NULL,
    is_main boolean,
    company_name character varying(200),
    nip character varying(11),
    address integer NOT NULL,
    email character varying(50),
    phone character varying(10),
    longitude double precision,
    latitude double precision
);


ALTER TABLE company_branch OWNER TO ks;

--
-- Name: company_description; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE company_description (
    id_company_description character varying(10) NOT NULL,
    description_long text,
    description_short text
);


ALTER TABLE company_description OWNER TO ks;

--
-- Name: contact_id_contact_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE contact_id_contact_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE contact_id_contact_seq OWNER TO ks;

--
-- Name: contact_id_contact_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE contact_id_contact_seq OWNED BY address.id_address;


--
-- Name: country_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE country_dict (
    country character varying(100) NOT NULL
);


ALTER TABLE country_dict OWNER TO ks;

--
-- Name: currrency; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE currrency (
    id_currency character varying(10) NOT NULL,
    currency_name character varying(100),
    ratio_to_main_currency real
);


ALTER TABLE currrency OWNER TO ks;

--
-- Name: discount_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE discount_dict (
    id_discount_dict character varying(10) NOT NULL,
    discount_name character varying(100),
    scope character varying(10) NOT NULL,
    discount_amount real,
    discount_percent real,
    valid_from date,
    valid_to date
);


ALTER TABLE discount_dict OWNER TO ks;

--
-- Name: discount_scope; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE discount_scope (
    id_discount_scope character varying(10) NOT NULL
);


ALTER TABLE discount_scope OWNER TO ks;

--
-- Name: discount_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW discount_v AS
 SELECT discount_dict.id_discount_dict,
    discount_dict.scope,
    discount_dict.discount_name,
    discount_dict.discount_amount,
    discount_dict.discount_percent,
    discount_dict.valid_from,
    discount_dict.valid_to
   FROM discount_dict;


ALTER TABLE discount_v OWNER TO ks;

--
-- Name: django_admin_log; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE django_admin_log (
    id integer NOT NULL,
    action_time timestamp with time zone NOT NULL,
    object_id text,
    object_repr character varying(200) NOT NULL,
    action_flag smallint NOT NULL,
    change_message text NOT NULL,
    content_type_id integer,
    user_id integer NOT NULL,
    CONSTRAINT django_admin_log_action_flag_check CHECK ((action_flag >= 0))
);


ALTER TABLE django_admin_log OWNER TO ks;

--
-- Name: django_admin_log_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE django_admin_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE django_admin_log_id_seq OWNER TO ks;

--
-- Name: django_admin_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE django_admin_log_id_seq OWNED BY django_admin_log.id;


--
-- Name: django_content_type; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE django_content_type (
    id integer NOT NULL,
    app_label character varying(100) NOT NULL,
    model character varying(100) NOT NULL
);


ALTER TABLE django_content_type OWNER TO ks;

--
-- Name: django_content_type_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE django_content_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE django_content_type_id_seq OWNER TO ks;

--
-- Name: django_content_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE django_content_type_id_seq OWNED BY django_content_type.id;


--
-- Name: django_migrations; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE django_migrations (
    id integer NOT NULL,
    app character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    applied timestamp with time zone NOT NULL
);


ALTER TABLE django_migrations OWNER TO ks;

--
-- Name: django_migrations_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE django_migrations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE django_migrations_id_seq OWNER TO ks;

--
-- Name: django_migrations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE django_migrations_id_seq OWNED BY django_migrations.id;


--
-- Name: django_session; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE django_session (
    session_key character varying(40) NOT NULL,
    session_data text NOT NULL,
    expire_date timestamp with time zone NOT NULL
);


ALTER TABLE django_session OWNER TO ks;

--
-- Name: location; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE location (
    id_location integer NOT NULL,
    location_name character varying(200),
    location_type character varying(10) NOT NULL,
    is_operational boolean,
    notes character varying(400),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE location OWNER TO ks;

--
-- Name: location_id_location_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE location_id_location_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE location_id_location_seq OWNER TO ks;

--
-- Name: location_id_location_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE location_id_location_seq OWNED BY location.id_location;


--
-- Name: location_type; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE location_type (
    id_location_type character varying(10) NOT NULL,
    location_type_name character varying(200),
    location_capacity integer DEFAULT 1
);


ALTER TABLE location_type OWNER TO ks;

--
-- Name: location_type_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW location_type_details_v AS
 SELECT location_type.id_location_type,
    location_type.location_type_name,
    location_type.location_capacity
   FROM location_type;


ALTER TABLE location_type_details_v OWNER TO ks;

--
-- Name: location_type_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW location_type_list_v AS
 SELECT location_type.id_location_type,
    location_type.location_type_name,
    location_type.location_capacity,
    count(location.id_location) AS count_id_location
   FROM (location_type
     LEFT JOIN location ON (((location_type.id_location_type)::text = (location.location_type)::text)))
  GROUP BY location_type.id_location_type, location_type.location_type_name, location_type.location_capacity;


ALTER TABLE location_type_list_v OWNER TO ks;

--
-- Name: location_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW location_v AS
 SELECT location.id_location,
    location.location_name,
    location_type.location_type_name,
    location_type.location_capacity,
    location.is_operational,
    location.notes
   FROM (location
     LEFT JOIN location_type ON (((location_type.id_location_type)::text = (location.location_type)::text)));


ALTER TABLE location_v OWNER TO ks;

--
-- Name: machine; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE machine (
    id_machine integer NOT NULL,
    machine_name character varying(200) NOT NULL,
    machine_type character varying(10) NOT NULL,
    service_interval integer,
    last_service date,
    is_operational boolean,
    notes character varying(400),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE machine OWNER TO ks;

--
-- Name: machine_id_machine_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE machine_id_machine_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE machine_id_machine_seq OWNER TO ks;

--
-- Name: machine_id_machine_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE machine_id_machine_seq OWNED BY machine.id_machine;


--
-- Name: machine_type; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE machine_type (
    id_machine_type character varying(10) NOT NULL,
    machine_type_name character varying(200) NOT NULL
);


ALTER TABLE machine_type OWNER TO ks;

--
-- Name: machine_incoming_services_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW machine_incoming_services_v AS
 SELECT machine.id_machine,
    machine_type.machine_type_name,
    machine.last_service,
    machine.service_interval,
    machine.notes
   FROM (machine
     JOIN machine_type ON (((machine_type.id_machine_type)::text = (machine.machine_type)::text)));


ALTER TABLE machine_incoming_services_v OWNER TO ks;

--
-- Name: machine_type_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW machine_type_details_v AS
 SELECT machine_type.id_machine_type,
    machine_type.machine_type_name
   FROM machine_type;


ALTER TABLE machine_type_details_v OWNER TO ks;

--
-- Name: machine_type_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW machine_type_list_v AS
 SELECT machine_type.id_machine_type,
    machine_type.machine_type_name,
    count(machine.id_machine) AS count_id_machine
   FROM (machine_type
     LEFT JOIN machine ON (((machine_type.id_machine_type)::text = (machine.machine_type)::text)))
  GROUP BY machine_type.id_machine_type, machine_type.machine_type_name;


ALTER TABLE machine_type_list_v OWNER TO ks;

--
-- Name: machine_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW machine_v AS
 SELECT machine.id_machine,
    machine.machine_name,
    machine_type.machine_type_name,
    machine.last_service,
    machine.service_interval,
    machine.is_operational,
    machine.notes
   FROM (machine
     LEFT JOIN machine_type ON (((machine_type.id_machine_type)::text = (machine.machine_type)::text)));


ALTER TABLE machine_v OWNER TO ks;

--
-- Name: payment_list_overdue_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW payment_list_overdue_v AS
 SELECT cl_payment.id_cl_payment,
    cl_payment.is_closed,
    client.first_name,
    client.last_name,
    client.client_name,
    cl_payment.payment_name,
    cl_payment.is_invoice,
    cl_payment.invoice_voucher,
    cl_payment.payment_sum,
    cl_payment.posted_datetime,
    cl_payment.due_date,
    cl_payment.paid_datetime,
    cl_payment.notes
   FROM (cl_payment
     LEFT JOIN client ON ((client.id_client = cl_payment.client)))
  WHERE ((cl_payment.is_closed = false) AND (cl_payment.due_date < ('now'::text)::date));


ALTER TABLE payment_list_overdue_v OWNER TO ks;

--
-- Name: resources_usage; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE resources_usage (
    id_resources_usage integer NOT NULL,
    service integer,
    machine integer,
    worker integer,
    time_slot timestamp without time zone,
    calendar_date date,
    company_branch character varying(10) DEFAULT 'main'::character varying,
    start_timestamp timestamp without time zone NOT NULL,
    finish_timestamp timestamp without time zone NOT NULL,
    location integer
);


ALTER TABLE resources_usage OWNER TO ks;

--
-- Name: resources_usage_id_resources_usage_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE resources_usage_id_resources_usage_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE resources_usage_id_resources_usage_seq OWNER TO ks;

--
-- Name: resources_usage_id_resources_usage_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE resources_usage_id_resources_usage_seq OWNED BY resources_usage.id_resources_usage;


--
-- Name: worker; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE worker (
    id_worker integer NOT NULL,
    first_name character varying(20),
    last_name character varying(20),
    worker_title character varying(200),
    active boolean,
    address integer,
    notes character varying(400),
    company_branch character varying(10) DEFAULT 'main'::character varying NOT NULL,
    email character varying(50),
    phone character varying(20),
    prefered_contact_type character varying(10),
    worker_user_login integer
);


ALTER TABLE worker OWNER TO ks;

--
-- Name: resources_usage_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW resources_usage_list_v AS
 SELECT resources_usage.calendar_date,
    max(resources_usage.time_slot) AS max_time_slot,
    min(resources_usage.time_slot) AS min_time_slot,
    client.last_name AS client_last_name,
    client.first_name AS client_first_name,
    client.client_name,
    se_dict.se_dict_name,
    worker.first_name AS worker_first_name,
    worker.last_name AS worker_last_name,
    machine.machine_name
   FROM ((((((resources_usage
     LEFT JOIN service ON ((service.id_service = resources_usage.service)))
     LEFT JOIN client ON ((client.id_client = service.client)))
     LEFT JOIN worker ON ((worker.id_worker = resources_usage.worker)))
     LEFT JOIN location ON ((location.id_location = service.location)))
     LEFT JOIN machine ON ((machine.id_machine = resources_usage.machine)))
     LEFT JOIN se_dict ON (((se_dict.id_se_dict)::text = (service.service_code)::text)))
  GROUP BY resources_usage.calendar_date, client.last_name, client.first_name, client.client_name, se_dict.se_dict_name, worker.first_name, worker.last_name, machine.machine_name;


ALTER TABLE resources_usage_list_v OWNER TO ks;

--
-- Name: resources_usage_params; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE resources_usage_params (
    allow_using_machine_without_service_days_before_date integer NOT NULL,
    resources_usage_params_id integer NOT NULL
);


ALTER TABLE resources_usage_params OWNER TO ks;

--
-- Name: resources_usage_params_resources_usage_params_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE resources_usage_params_resources_usage_params_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE resources_usage_params_resources_usage_params_id_seq OWNER TO ks;

--
-- Name: resources_usage_params_resources_usage_params_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE resources_usage_params_resources_usage_params_id_seq OWNED BY resources_usage_params.resources_usage_params_id;


--
-- Name: resources_usage_params_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW resources_usage_params_v AS
 SELECT resources_usage_params.allow_using_machine_without_service_days_before_date
   FROM resources_usage_params;


ALTER TABLE resources_usage_params_v OWNER TO ks;

--
-- Name: se_discount; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE se_discount (
    id_se_discount integer NOT NULL,
    discount character varying(10) NOT NULL,
    service integer NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying NOT NULL
);


ALTER TABLE se_discount OWNER TO ks;

--
-- Name: se_discount_id_se_discount_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE se_discount_id_se_discount_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE se_discount_id_se_discount_seq OWNER TO ks;

--
-- Name: se_discount_id_se_discount_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE se_discount_id_se_discount_seq OWNED BY se_discount.id_se_discount;


--
-- Name: se_group_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE se_group_dict (
    id_se_group_dict character varying(10) NOT NULL,
    se_group_dict_name text NOT NULL
);


ALTER TABLE se_group_dict OWNER TO ks;

--
-- Name: se_requirement; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE se_requirement (
    id_se_requirement integer NOT NULL,
    service_code character varying(10) NOT NULL,
    machine_type character varying(10),
    worker_ability character varying(10),
    qty real DEFAULT 1,
    activity_sort_order integer,
    activity_name character varying(200),
    time_minutes integer,
    location_type character varying(10)
);


ALTER TABLE se_requirement OWNER TO ks;

--
-- Name: se_requirement_id_se_requirement_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE se_requirement_id_se_requirement_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE se_requirement_id_se_requirement_seq OWNER TO ks;

--
-- Name: se_requirement_id_se_requirement_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE se_requirement_id_se_requirement_seq OWNED BY se_requirement.id_se_requirement;


--
-- Name: service_accept_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW service_accept_v AS
 SELECT service.id_service,
    se_dict.se_dict_name,
    client.first_name,
    client.last_name,
    client.client_name,
    service.location,
    se_dict.base_price,
    service.service_discount_amount,
    service.service_discount_percent,
    cl_payment_line.final_price,
    service.create_invoice,
    string_agg((worker.first_name)::text, ', '::text) AS worker_first_name,
    string_agg((worker.last_name)::text, ', '::text) AS worker_last_name,
    service.planned_start,
    service.created_datetime,
    service.planned_end,
    count(cl_payment.id_cl_payment) AS count_id_cl_payment
   FROM ((((((service
     JOIN cl_payment_line ON ((service.id_service = cl_payment_line.service)))
     JOIN se_dict ON (((se_dict.id_se_dict)::text = (service.service_code)::text)))
     JOIN client ON ((client.id_client = service.client)))
     JOIN resources_usage ON ((service.id_service = resources_usage.service)))
     JOIN worker ON ((worker.id_worker = resources_usage.worker)))
     LEFT JOIN cl_payment cl_payment ON ((client.id_client = cl_payment.client)))
  GROUP BY service.id_service, se_dict.se_dict_name, client.first_name, client.last_name, client.client_name, service.location, se_dict.base_price, service.service_discount_amount, service.service_discount_percent, cl_payment_line.final_price, service.create_invoice, service.planned_start, service.created_datetime, service.planned_end;


ALTER TABLE service_accept_v OWNER TO ks;

--
-- Name: service_archived; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE service_archived (
    id_service_archived integer NOT NULL,
    id_service integer NOT NULL,
    service_code character varying(10) NOT NULL,
    service_name character varying(200),
    service_discount_amount real,
    service_discount_percent real,
    client integer NOT NULL,
    client_first_name character varying(20),
    client_last_name character varying(20),
    client_name character varying(200),
    client_discount real,
    location integer,
    location_name character varying(200),
    planned_start date,
    planned_end date,
    real_start date,
    real_end date,
    reminder_sms_minutes integer,
    reminder_email_minutes integer,
    finished_info_sms boolean,
    finished_info_email boolean,
    notes character varying(400),
    created_datetime timestamp without time zone DEFAULT now(),
    created_by character varying(10) DEFAULT 'current_user'::character varying,
    is_confirmed boolean,
    confirmed_datetime timestamp without time zone,
    confirmed_by character varying(10),
    is_deleted boolean,
    deleted_timestamp timestamp without time zone,
    deleted_by character varying(10),
    deleted_reason character varying(400),
    company_branch character varying(10) NOT NULL,
    min_start_datetime timestamp without time zone
);


ALTER TABLE service_archived OWNER TO ks;

--
-- Name: service_archived_id_service_archived_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE service_archived_id_service_archived_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE service_archived_id_service_archived_seq OWNER TO ks;

--
-- Name: service_archived_id_service_archived_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE service_archived_id_service_archived_seq OWNED BY service_archived.id_service_archived;


--
-- Name: service_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW service_details_v AS
 SELECT service.service_code,
    service.is_confirmed,
    service.client,
    service.min_start_datetime,
    service.service_discount_amount,
    service.service_discount_percent,
    service.reminder_sms_minutes,
    service.reminder_email_minutes,
    service.finished_info_sms,
    service.finished_info_email,
    service.create_invoice,
    service.notes
   FROM service;


ALTER TABLE service_details_v OWNER TO ks;

--
-- Name: service_dict_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW service_dict_details_v AS
 SELECT se_dict.id_se_dict,
    se_dict.se_dict_name,
    se_dict.location_type,
    se_dict.avg_time,
    se_dict.continous,
    se_dict.base_price,
    se_dict.notes,
    se_requirement.qty,
    se_requirement.machine_type,
    se_requirement.worker_ability
   FROM (se_dict
     LEFT JOIN se_requirement ON (((se_dict.id_se_dict)::text = (se_requirement.service_code)::text)));


ALTER TABLE service_dict_details_v OWNER TO ks;

--
-- Name: service_dict_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW service_dict_list_v AS
 SELECT se_dict.id_se_dict,
    se_dict.se_dict_name,
    se_dict.location_type,
    se_dict.continous,
    se_dict.avg_time,
    se_dict.base_price,
    se_dict.notes
   FROM se_dict;


ALTER TABLE service_dict_list_v OWNER TO ks;

--
-- Name: service_id_service_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE service_id_service_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE service_id_service_seq OWNER TO ks;

--
-- Name: service_id_service_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE service_id_service_seq OWNED BY service.id_service;


--
-- Name: service_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW service_list_v AS
 SELECT service.id_service,
    se_dict.se_dict_name,
    client.first_name,
    client.last_name,
    client.client_name,
    location.location_name,
    service.create_invoice,
    cl_payment.invoice_voucher,
    se_dict.base_price,
    cl_payment_line.final_price,
    service.planned_start,
    service.planned_end,
    service.real_start,
    service.real_end,
    cl_payment.paid_datetime,
    service.confirmed_by AS service_notes,
    service.notes AS confirmed_by
   FROM (((((service
     LEFT JOIN client ON ((client.id_client = service.client)))
     JOIN se_dict ON (((se_dict.id_se_dict)::text = (service.service_code)::text)))
     LEFT JOIN location ON ((location.id_location = service.location)))
     LEFT JOIN cl_payment_line ON ((service.id_service = cl_payment_line.service)))
     LEFT JOIN cl_payment ON ((cl_payment.id_cl_payment = cl_payment_line.payment)));


ALTER TABLE service_list_v OWNER TO ks;

--
-- Name: sex_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE sex_dict (
    id_sex_dict character(1) NOT NULL,
    sex character varying(10)
);


ALTER TABLE sex_dict OWNER TO ks;

--
-- Name: time_slot_list; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE time_slot_list (
    id_time_slot timestamp without time zone NOT NULL
);


ALTER TABLE time_slot_list OWNER TO ks;

--
-- Name: time_slot_params; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE time_slot_params (
    time_slot_minutes integer NOT NULL,
    time_slot_params_id integer NOT NULL
);


ALTER TABLE time_slot_params OWNER TO ks;

--
-- Name: time_slot_params_time_slot_params_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE time_slot_params_time_slot_params_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE time_slot_params_time_slot_params_id_seq OWNER TO ks;

--
-- Name: time_slot_params_time_slot_params_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE time_slot_params_time_slot_params_id_seq OWNED BY time_slot_params.time_slot_params_id;


--
-- Name: time_slot_params_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW time_slot_params_v AS
 SELECT time_slot_params.time_slot_minutes
   FROM time_slot_params;


ALTER TABLE time_slot_params_v OWNER TO ks;

--
-- Name: wo_ability; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_ability (
    id_wo_ability integer NOT NULL,
    worker integer NOT NULL,
    worker_ability character varying(10) NOT NULL,
    notes character varying(400),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE wo_ability OWNER TO ks;

--
-- Name: wo_ability_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_ability_dict (
    id_wo_ability_dict character varying(10) NOT NULL,
    ability_name character varying(200) NOT NULL,
    ability_group character varying(10)
);


ALTER TABLE wo_ability_dict OWNER TO ks;

--
-- Name: wo_ability_group_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_ability_group_dict (
    id_wo_ablility_group_dict character varying(10) NOT NULL,
    ability_group_name character varying(200) NOT NULL
);


ALTER TABLE wo_ability_group_dict OWNER TO ks;

--
-- Name: wo_ability_id_wo_ability_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE wo_ability_id_wo_ability_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE wo_ability_id_wo_ability_seq OWNER TO ks;

--
-- Name: wo_ability_id_wo_ability_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE wo_ability_id_wo_ability_seq OWNED BY wo_ability.id_wo_ability;


--
-- Name: wo_absence; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_absence (
    id_wo_absence integer NOT NULL,
    worker integer NOT NULL,
    absence_type character varying(10) NOT NULL,
    start_datetime timestamp without time zone,
    end_datetime timestamp without time zone,
    workdays integer,
    hours real,
    notes character varying(400),
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE wo_absence OWNER TO ks;

--
-- Name: wo_absence_type; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_absence_type (
    id_wo_absence_type character varying(10) NOT NULL,
    absence_name character varying(200)
);


ALTER TABLE wo_absence_type OWNER TO ks;

--
-- Name: wo_group; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_group (
    id_wo_group integer NOT NULL,
    worker integer NOT NULL,
    worker_group character varying(10) NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying
);


ALTER TABLE wo_group OWNER TO ks;

--
-- Name: wo_group_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_group_dict (
    id_wo_group_dict character varying(10) NOT NULL,
    worker_group_name character varying(200)
);


ALTER TABLE wo_group_dict OWNER TO ks;

--
-- Name: wo_group_id_wo_group_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE wo_group_id_wo_group_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE wo_group_id_wo_group_seq OWNER TO ks;

--
-- Name: wo_group_id_wo_group_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE wo_group_id_wo_group_seq OWNED BY wo_group.id_wo_group;


--
-- Name: wo_group_privilege; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_group_privilege (
    id_wo_group_privilege integer NOT NULL,
    worker_group character varying(10) NOT NULL,
    privilege_id integer NOT NULL,
    view_id character varying(100),
    privilege_level character varying(10) NOT NULL
);


ALTER TABLE wo_group_privilege OWNER TO ks;

--
-- Name: wo_group_privilege_id_wo_group_privilege_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE wo_group_privilege_id_wo_group_privilege_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE wo_group_privilege_id_wo_group_privilege_seq OWNER TO ks;

--
-- Name: wo_group_privilege_id_wo_group_privilege_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE wo_group_privilege_id_wo_group_privilege_seq OWNED BY wo_group_privilege.id_wo_group_privilege;


--
-- Name: wo_notification; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_notification (
    id_wo_notification integer NOT NULL,
    worker integer NOT NULL,
    worker_group character varying(10),
    notification_subject character varying(200),
    notification_text character varying(400),
    severity integer,
    marked_as_read boolean,
    company_branch character varying(10) DEFAULT 'main'::character varying,
    from_user character varying(150)
);


ALTER TABLE wo_notification OWNER TO ks;

--
-- Name: wo_notification_id_wo_notification_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE wo_notification_id_wo_notification_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE wo_notification_id_wo_notification_seq OWNER TO ks;

--
-- Name: wo_notification_id_wo_notification_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE wo_notification_id_wo_notification_seq OWNED BY wo_notification.id_wo_notification;


--
-- Name: wo_privilege_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_privilege_dict (
    id_wo_privilige_dict integer NOT NULL,
    privilege_name character varying(200)
);


ALTER TABLE wo_privilege_dict OWNER TO ks;

--
-- Name: wo_privilege_level_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_privilege_level_dict (
    id_wo_privilege_level_dict character varying(10) NOT NULL
);


ALTER TABLE wo_privilege_level_dict OWNER TO ks;

--
-- Name: wo_user; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE wo_user (
    app_user character varying(10) NOT NULL,
    worker integer NOT NULL
);


ALTER TABLE wo_user OWNER TO ks;

--
-- Name: workday_calendar; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE workday_calendar (
    id_workday_calendar date NOT NULL,
    is_workday boolean NOT NULL,
    company_branch character varying(10) DEFAULT 'main'::character varying,
    work_start time without time zone,
    work_end time without time zone
);


ALTER TABLE workday_calendar OWNER TO ks;

--
-- Name: workday_calendar_params; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE workday_calendar_params (
    default_is_saturday_workday boolean NOT NULL,
    workday_calendar_params_id integer NOT NULL,
    default_workday_start_time time without time zone,
    default_workday_end_time time without time zone,
    default_saturday_start_time time without time zone,
    default_saturday_end_time time without time zone,
    days_to_display integer DEFAULT 61 NOT NULL
);


ALTER TABLE workday_calendar_params OWNER TO ks;

--
-- Name: workday_calendar_params_workday_calendar_params_id_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE workday_calendar_params_workday_calendar_params_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE workday_calendar_params_workday_calendar_params_id_seq OWNER TO ks;

--
-- Name: workday_calendar_params_workday_calendar_params_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE workday_calendar_params_workday_calendar_params_id_seq OWNED BY workday_calendar_params.workday_calendar_params_id;


--
-- Name: worker_ability_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_details_v AS
 SELECT wo_ability.worker,
    wo_ability.worker_ability,
    wo_ability.notes
   FROM wo_ability;


ALTER TABLE worker_ability_details_v OWNER TO ks;

--
-- Name: worker_ability_dict_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_dict_details_v AS
 SELECT wo_ability_dict.id_wo_ability_dict,
    wo_ability_dict.ability_name,
    wo_ability_group_dict.ability_group_name
   FROM (wo_ability_dict
     LEFT JOIN wo_ability_group_dict ON (((wo_ability_group_dict.id_wo_ablility_group_dict)::text = (wo_ability_dict.ability_group)::text)));


ALTER TABLE worker_ability_dict_details_v OWNER TO ks;

--
-- Name: worker_ability_dict_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_dict_list_v AS
 SELECT wo_ability_dict.id_wo_ability_dict,
    wo_ability_dict.ability_name,
    wo_ability_group_dict.ability_group_name,
    count(wo_ability.worker) AS count_worker
   FROM ((wo_ability_dict
     LEFT JOIN wo_ability_group_dict ON (((wo_ability_group_dict.id_wo_ablility_group_dict)::text = (wo_ability_dict.ability_group)::text)))
     LEFT JOIN wo_ability ON (((wo_ability_dict.id_wo_ability_dict)::text = (wo_ability.worker_ability)::text)))
  GROUP BY wo_ability_dict.id_wo_ability_dict, wo_ability_dict.ability_name, wo_ability_group_dict.ability_group_name;


ALTER TABLE worker_ability_dict_list_v OWNER TO ks;

--
-- Name: worker_ability_group_dict_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_group_dict_details_v AS
 SELECT wo_ability_group_dict.id_wo_ablility_group_dict,
    wo_ability_group_dict.ability_group_name
   FROM wo_ability_group_dict;


ALTER TABLE worker_ability_group_dict_details_v OWNER TO ks;

--
-- Name: worker_ability_group_dict_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_group_dict_list_v AS
 SELECT wo_ability_group_dict.ability_group_name,
    wo_ability_group_dict.id_wo_ablility_group_dict,
    count(wo_ability_dict.id_wo_ability_dict) AS count_id_wo_ability_dict
   FROM (wo_ability_group_dict
     JOIN wo_ability_dict ON (((wo_ability_group_dict.id_wo_ablility_group_dict)::text = (wo_ability_dict.ability_group)::text)))
  GROUP BY wo_ability_group_dict.ability_group_name, wo_ability_group_dict.id_wo_ablility_group_dict;


ALTER TABLE worker_ability_group_dict_list_v OWNER TO ks;

--
-- Name: worker_ability_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_ability_list_v AS
 SELECT worker.id_worker,
    worker.first_name,
    worker.last_name,
    wo_ability_dict.ability_name,
    wo_ability_group_dict.ability_group_name,
    wo_ability.notes
   FROM (((wo_ability
     LEFT JOIN worker ON ((worker.id_worker = wo_ability.worker)))
     LEFT JOIN wo_ability_dict ON (((wo_ability_dict.id_wo_ability_dict)::text = (wo_ability.worker_ability)::text)))
     LEFT JOIN wo_ability_group_dict ON (((wo_ability_group_dict.id_wo_ablility_group_dict)::text = (wo_ability_dict.ability_group)::text)))
  WHERE (worker.active = true);


ALTER TABLE worker_ability_list_v OWNER TO ks;

--
-- Name: worker_absence_all_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_all_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_all_v OWNER TO ks;

--
-- Name: worker_absence_calendar_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_calendar_v AS
 SELECT workday_calendar.id_workday_calendar,
    workday_calendar.is_workday,
    wo_absence.absence_type,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    worker.first_name,
    worker.last_name
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN workday_calendar ON ((workday_calendar.id_workday_calendar = wo_absence.start_datetime)));


ALTER TABLE worker_absence_calendar_v OWNER TO ks;

--
-- Name: worker_absence_current_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_current_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE ((wo_absence.start_datetime < now()) AND (wo_absence.end_datetime > now()))
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_current_v OWNER TO ks;

--
-- Name: worker_absence_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_details_v AS
 SELECT wo_absence.worker,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
     JOIN worker ON ((worker.id_worker = wo_absence.worker)));


ALTER TABLE worker_absence_details_v OWNER TO ks;

--
-- Name: worker_absence_ended_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_ended_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE (wo_absence.end_datetime < now())
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_ended_v OWNER TO ks;

--
-- Name: worker_absence_future_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_future_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE (wo_absence.start_datetime > now())
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_future_v OWNER TO ks;

--
-- Name: worker_absence_hour_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_hour_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE ((wo_absence_type.id_wo_absence_type)::text = 'GODZ'::text)
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_hour_v OWNER TO ks;

--
-- Name: worker_absence_other_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_other_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE (((wo_absence_type.id_wo_absence_type)::text <> 'CHO'::text) OR ((wo_absence_type.id_wo_absence_type)::text <> 'WYP'::text) OR ((wo_absence_type.id_wo_absence_type)::text <> 'GODZ'::text))
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_other_v OWNER TO ks;

--
-- Name: worker_absence_recreational_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_recreational_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     LEFT JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE ((wo_absence_type.id_wo_absence_type)::text = 'WYP'::text)
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_recreational_v OWNER TO ks;

--
-- Name: worker_absence_sick_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_absence_sick_v AS
 SELECT wo_absence.id_wo_absence,
    worker.first_name,
    worker.last_name,
    wo_absence_type.absence_name,
    wo_absence.start_datetime,
    wo_absence.end_datetime,
    wo_absence.notes
   FROM ((wo_absence
     JOIN worker ON ((worker.id_worker = wo_absence.worker)))
     JOIN wo_absence_type ON (((wo_absence_type.id_wo_absence_type)::text = (wo_absence.absence_type)::text)))
  WHERE ((wo_absence_type.id_wo_absence_type)::text = 'CHO'::text)
  ORDER BY wo_absence.start_datetime;


ALTER TABLE worker_absence_sick_v OWNER TO ks;

--
-- Name: worker_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_details_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    worker.notes,
    worker.prefered_contact_type,
    worker.email,
    worker.phone,
    address.street,
    address.house_no,
    address.apartment_no,
    address.city,
    address.zip,
    address.country,
    address.notes AS notes1
   FROM (worker
     JOIN address ON ((address.id_address = worker.address)));


ALTER TABLE worker_details_v OWNER TO ks;

--
-- Name: worker_group_dict_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_group_dict_details_v AS
 SELECT wo_group_dict.id_wo_group_dict,
    wo_group_dict.worker_group_name
   FROM wo_group_dict;


ALTER TABLE worker_group_dict_details_v OWNER TO ks;

--
-- Name: worker_group_dict_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_group_dict_list_v AS
 SELECT wo_group_dict.id_wo_group_dict,
    wo_group_dict.worker_group_name,
    count(wo_group.worker) AS count_worker
   FROM (wo_group_dict
     LEFT JOIN wo_group ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
  GROUP BY wo_group_dict.id_wo_group_dict, wo_group_dict.worker_group_name;


ALTER TABLE worker_group_dict_list_v OWNER TO ks;

--
-- Name: worker_group_privilege_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_group_privilege_v AS
 SELECT wo_group_privilege.view_id,
    wo_group_privilege.privilege_level,
    wo_group_dict.worker_group_name
   FROM (wo_group_privilege
     LEFT JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group_privilege.worker_group)::text)));


ALTER TABLE worker_group_privilege_v OWNER TO ks;

--
-- Name: worker_id_worker_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE worker_id_worker_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE worker_id_worker_seq OWNER TO ks;

--
-- Name: worker_id_worker_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE worker_id_worker_seq OWNED BY worker.id_worker;


--
-- Name: worker_list_active_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_list_active_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    string_agg((wo_group_dict.worker_group_name)::text, ', '::text) AS sum_worker_group_name,
    min(wo_absence.start_datetime) AS min_start_datetime,
    worker.notes
   FROM (((worker
     LEFT JOIN wo_group ON ((worker.id_worker = wo_group.worker)))
     JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
     LEFT JOIN wo_absence ON ((worker.id_worker = wo_absence.worker)))
  GROUP BY worker.id_worker, worker.active, worker.first_name, worker.last_name, worker.worker_title, worker.notes
 HAVING ((worker.active = true) AND (min(wo_absence.start_datetime) > ('now'::text)::date));


ALTER TABLE worker_list_active_v OWNER TO ks;

--
-- Name: worker_list_all_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_list_all_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    string_agg((wo_group_dict.worker_group_name)::text, ', '::text) AS sum_worker_group_name,
    min(wo_absence.start_datetime) AS min_start_datetime,
    worker.notes
   FROM (((worker
     LEFT JOIN wo_group ON ((worker.id_worker = wo_group.worker)))
     JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
     LEFT JOIN wo_absence ON ((worker.id_worker = wo_absence.worker)))
  GROUP BY worker.id_worker, worker.active, worker.first_name, worker.last_name, worker.worker_title, worker.notes
 HAVING (min(wo_absence.start_datetime) > ('now'::text)::date);


ALTER TABLE worker_list_all_v OWNER TO ks;

--
-- Name: worker_list_inactive_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_list_inactive_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    string_agg((wo_group_dict.worker_group_name)::text, ', '::text) AS sum_worker_group_name,
    min(wo_absence.start_datetime) AS min_start_datetime,
    worker.notes
   FROM (((worker
     LEFT JOIN wo_group ON ((worker.id_worker = wo_group.worker)))
     JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
     LEFT JOIN wo_absence ON ((worker.id_worker = wo_absence.worker)))
  GROUP BY worker.id_worker, worker.active, worker.first_name, worker.last_name, worker.worker_title, worker.notes
 HAVING ((worker.active = false) AND (min(wo_absence.start_datetime) > ('now'::text)::date));


ALTER TABLE worker_list_inactive_v OWNER TO ks;

--
-- Name: worker_list_incoming_leave_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_list_incoming_leave_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    string_agg((wo_group_dict.worker_group_name)::text, ', '::text) AS sum_worker_group_name,
    min(wo_absence.start_datetime) AS min_start_datetime,
    worker.notes
   FROM (((worker
     LEFT JOIN wo_group ON ((worker.id_worker = wo_group.worker)))
     JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
     LEFT JOIN wo_absence ON ((worker.id_worker = wo_absence.worker)))
  GROUP BY worker.id_worker, worker.active, worker.first_name, worker.last_name, worker.worker_title, worker.notes
 HAVING (min(wo_absence.start_datetime) > ('now'::text)::date);


ALTER TABLE worker_list_incoming_leave_v OWNER TO ks;

--
-- Name: worker_list_on_leave_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_list_on_leave_v AS
 SELECT worker.id_worker,
    worker.active,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    string_agg((wo_group_dict.worker_group_name)::text, ', '::text) AS sum_worker_group_name,
    min(wo_absence.start_datetime) AS min_start_datetime,
    wo_group_dict.worker_group_name,
    worker.notes
   FROM (((worker
     LEFT JOIN wo_group ON ((worker.id_worker = wo_group.worker)))
     JOIN wo_group_dict ON (((wo_group_dict.id_wo_group_dict)::text = (wo_group.worker_group)::text)))
     LEFT JOIN wo_absence ON ((worker.id_worker = wo_absence.worker)))
  WHERE ((wo_absence.start_datetime < now()) AND (wo_absence.end_datetime > now()))
  GROUP BY worker.id_worker, worker.active, worker.first_name, worker.last_name, worker.worker_title, wo_group_dict.worker_group_name, worker.notes;


ALTER TABLE worker_list_on_leave_v OWNER TO ks;

--
-- Name: worker_user_details_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_user_details_v AS
 SELECT wo_user.app_user,
    wo_user.worker
   FROM wo_user;


ALTER TABLE worker_user_details_v OWNER TO ks;

--
-- Name: worker_user_list_v; Type: VIEW; Schema: public; Owner: ks
--

CREATE VIEW worker_user_list_v AS
 SELECT wo_user.app_user,
    wo_user.worker,
    worker.first_name,
    worker.last_name,
    worker.worker_title,
    worker.active,
    worker.notes
   FROM (wo_user
     LEFT JOIN worker ON ((worker.id_worker = wo_user.worker)));


ALTER TABLE worker_user_list_v OWNER TO ks;

--
-- Name: zip_codes_dict; Type: TABLE; Schema: public; Owner: ks
--

CREATE TABLE zip_codes_dict (
    id_zip_codes_dict integer NOT NULL,
    country character varying(100) NOT NULL,
    zip character varying(10),
    city character varying(100)
);


ALTER TABLE zip_codes_dict OWNER TO ks;

--
-- Name: zip_codes_dict_id_zip_codes_dict_seq; Type: SEQUENCE; Schema: public; Owner: ks
--

CREATE SEQUENCE zip_codes_dict_id_zip_codes_dict_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE zip_codes_dict_id_zip_codes_dict_seq OWNER TO ks;

--
-- Name: zip_codes_dict_id_zip_codes_dict_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ks
--

ALTER SEQUENCE zip_codes_dict_id_zip_codes_dict_seq OWNED BY zip_codes_dict.id_zip_codes_dict;


--
-- Name: address id_address; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY address ALTER COLUMN id_address SET DEFAULT nextval('contact_id_contact_seq'::regclass);


--
-- Name: auth_group id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group ALTER COLUMN id SET DEFAULT nextval('auth_group_id_seq'::regclass);


--
-- Name: auth_group_permissions id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group_permissions ALTER COLUMN id SET DEFAULT nextval('auth_group_permissions_id_seq'::regclass);


--
-- Name: auth_permission id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_permission ALTER COLUMN id SET DEFAULT nextval('auth_permission_id_seq'::regclass);


--
-- Name: auth_user id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user ALTER COLUMN id SET DEFAULT nextval('auth_user_id_seq'::regclass);


--
-- Name: auth_user_groups id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_groups ALTER COLUMN id SET DEFAULT nextval('auth_user_groups_id_seq'::regclass);


--
-- Name: auth_user_user_permissions id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_user_permissions ALTER COLUMN id SET DEFAULT nextval('auth_user_user_permissions_id_seq'::regclass);


--
-- Name: cl_communication_log id_cl_communication_log; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log ALTER COLUMN id_cl_communication_log SET DEFAULT nextval('cl_communication_log_id_cl_communication_log_seq'::regclass);


--
-- Name: cl_discount id_cl_discount; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_discount ALTER COLUMN id_cl_discount SET DEFAULT nextval('cl_discount_id_cl_discount_seq'::regclass);


--
-- Name: cl_params cl_params_id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_params ALTER COLUMN cl_params_id SET DEFAULT nextval('cl_params_cl_params_id_seq'::regclass);


--
-- Name: cl_payment id_cl_payment; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment ALTER COLUMN id_cl_payment SET DEFAULT nextval('cl_payment_id_cl_payment_seq'::regclass);


--
-- Name: cl_payment_line id_cl_payment_line; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line ALTER COLUMN id_cl_payment_line SET DEFAULT nextval('cl_payment_line_id_cl_payment_line_seq'::regclass);


--
-- Name: cl_unconfirmed cl_unconfirmed; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_unconfirmed ALTER COLUMN cl_unconfirmed SET DEFAULT nextval('cl_unconfirmed_cl_unconfirmed_seq'::regclass);


--
-- Name: client id_client; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client ALTER COLUMN id_client SET DEFAULT nextval('client_id_client_seq'::regclass);


--
-- Name: django_admin_log id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_admin_log ALTER COLUMN id SET DEFAULT nextval('django_admin_log_id_seq'::regclass);


--
-- Name: django_content_type id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_content_type ALTER COLUMN id SET DEFAULT nextval('django_content_type_id_seq'::regclass);


--
-- Name: django_migrations id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_migrations ALTER COLUMN id SET DEFAULT nextval('django_migrations_id_seq'::regclass);


--
-- Name: location id_location; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location ALTER COLUMN id_location SET DEFAULT nextval('location_id_location_seq'::regclass);


--
-- Name: machine id_machine; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY machine ALTER COLUMN id_machine SET DEFAULT nextval('machine_id_machine_seq'::regclass);


--
-- Name: resources_usage id_resources_usage; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage ALTER COLUMN id_resources_usage SET DEFAULT nextval('resources_usage_id_resources_usage_seq'::regclass);


--
-- Name: resources_usage_params resources_usage_params_id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage_params ALTER COLUMN resources_usage_params_id SET DEFAULT nextval('resources_usage_params_resources_usage_params_id_seq'::regclass);


--
-- Name: se_discount id_se_discount; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_discount ALTER COLUMN id_se_discount SET DEFAULT nextval('se_discount_id_se_discount_seq'::regclass);


--
-- Name: se_requirement id_se_requirement; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement ALTER COLUMN id_se_requirement SET DEFAULT nextval('se_requirement_id_se_requirement_seq'::regclass);


--
-- Name: service id_service; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service ALTER COLUMN id_service SET DEFAULT nextval('service_id_service_seq'::regclass);


--
-- Name: service_archived id_service_archived; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service_archived ALTER COLUMN id_service_archived SET DEFAULT nextval('service_archived_id_service_archived_seq'::regclass);


--
-- Name: time_slot_params time_slot_params_id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY time_slot_params ALTER COLUMN time_slot_params_id SET DEFAULT nextval('time_slot_params_time_slot_params_id_seq'::regclass);


--
-- Name: wo_ability id_wo_ability; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability ALTER COLUMN id_wo_ability SET DEFAULT nextval('wo_ability_id_wo_ability_seq'::regclass);


--
-- Name: wo_group id_wo_group; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group ALTER COLUMN id_wo_group SET DEFAULT nextval('wo_group_id_wo_group_seq'::regclass);


--
-- Name: wo_group_privilege id_wo_group_privilege; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege ALTER COLUMN id_wo_group_privilege SET DEFAULT nextval('wo_group_privilege_id_wo_group_privilege_seq'::regclass);


--
-- Name: wo_notification id_wo_notification; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification ALTER COLUMN id_wo_notification SET DEFAULT nextval('wo_notification_id_wo_notification_seq'::regclass);


--
-- Name: workday_calendar_params workday_calendar_params_id; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY workday_calendar_params ALTER COLUMN workday_calendar_params_id SET DEFAULT nextval('workday_calendar_params_workday_calendar_params_id_seq'::regclass);


--
-- Name: worker id_worker; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY worker ALTER COLUMN id_worker SET DEFAULT nextval('worker_id_worker_seq'::regclass);


--
-- Name: zip_codes_dict id_zip_codes_dict; Type: DEFAULT; Schema: public; Owner: ks
--

ALTER TABLE ONLY zip_codes_dict ALTER COLUMN id_zip_codes_dict SET DEFAULT nextval('zip_codes_dict_id_zip_codes_dict_seq'::regclass);


--
-- Data for Name: address; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY address (id_address, prefered_contact_type, email, phone, street, house_no, apartment_no, city, zip, country, notes) FROM stdin;
79	\N	krzysztof@sazon.pl	999111222	Rycerska	23	21	Poznań	88-111	Polska	\N
26	\N	\N	\N	Gniewna	\N	\N	\N	\N	Polska	\N
27	\N	\N	\N	\N	\N	qweq	\N	\N	Polska	\N
80	\N	krzysztof.sazon@gmail.com	999222111	Koks	2	21	Kolo	22-333	Polska	\N
3	email	mechanik@buziaczek.eu	000 000 000	Szara	46	2	Kraków	30-820	Polska	\N
29	\N	\N	\N	\N	\N	\N	\N	\N	Polska	\N
30	\N	\N	\N	\N	\N	\N	\N	\N	Polska	\N
31	\N	\N	\N	\N	\N	\N	\N	\N	Polska	\N
32	\N	\N	\N	\N	\N	\N	\N	\N	Polska	\N
1	email	kokos@wp.pl	979 014 111	Stanisława Stojałowskiego	15	\N	Kraków	32-020	Polska	\N
56	\N	asd@wp.deasds	\N	\N	\N	\N	\N	\N	Polska	\N
57	\N	Lol@as.fg	\N	Szczurowska	3	\N	Poczdam	12-223	Polska	\N
6	email	janusz@buziczek.pl	\N	Grodzisko	37	87a	Szczutowo	90-300	Polska	\N
33	\N	krzychu@wp.pl	507661223	\N	\N	\N	\N	\N	Polska	\N
40	\N	asd@wp.de	\N	\N	\N	\N	\N	\N	Polska	\N
4	email	m@y.de	508552441	Lawinowa	116	23	Grzybno	52-444	Polska	\N
41	\N	as@wp.dp	\N	asda	\N	\N	\N	\N	Polska	\N
42	\N	asdasd@wp.pl	\N	\N	\N	\N	\N	\N	Polska	\N
43	\N	\N	\N	\N	\N	\N	\N	\N	Polska	\N
44	\N	Lolol@pw.de	\N	\N	\N	\N	\N	\N	Polska	\N
45	\N	bronio@wp.pl	\N	\N	\N	\N	\N	\N	Polska	\N
46	\N	bronioo@wp.pl	asdas	\N	\N	\N	\N	\N	Polska	\N
35	\N	\N	645335116	\N	\N	\N	\N	\N	Polska	\N
36	\N	\N	645335116	Szumwałdźka	\N	\N	\N	\N	Polska	\N
63	\N	ka@po.dl	\N	asdas	asdsa	\N	asdasd	asdasd	Polska	\N
16	\N	\N	\N	\N	\N	\N	Borkowo	\N	Polska	\N
48	\N	fsdfsdfsdf@w.gh	\N	\N	\N	\N	\N	\N	Polska	\N
5	\N	\N	\N	\N	47a	\N	Szczuka	\N	Polska	\N
19	\N	zlo@wp.de		Gniewna	34				Polska	\N
20	\N						ASDAS		Polska	\N
64	\N	testoster@ymail.com	888222333	Mocarsk	1	1	Rymcz	22-111	Polska	\N
23	\N	213#							Polska	\N
25	\N	as@wp.de							Polska	\N
39	\N	volatile.sm@gmail.com	\N	\N	\N	\N	\N	\N	Polska	\N
47	\N	asdasdasdas@e.gh	\N	\N	\N	\N	\N	\N	Polska	\N
65	\N	koko@onet.pl	\N	\N	\N	\N	\N	\N	Polska	\N
2	email	sredniwarsztat@wp.pl	909 322 888	Koszarówka	7	2	Kraków	30-383	Polska	\N
38	\N	ofeka	3	Wiejska	20	20	Lakie	99	Polska	\N
66	\N	asda@kij.lo	\N	\N	\N	\N	\N	\N	Polska	\N
67	\N	asaada@kij.lo	\N	\N	\N	\N	\N	\N	Polska	\N
51	\N	aaaaaa@as.as	\N	\N	\N	\N	\N	\N	Polska	\N
68	\N	boano@as.ld	\N	asd	\N	\N	asd	asdasd	Polska	\N
52	\N	b@bh.bh	\N	\N	\N	\N	\N	\N	Polska	\N
53	\N	bb@bh.bh	\N	\N	\N	\N	\N	\N	Polska	\N
50	\N	asdasda@aa.AA	\N	\N	\N	\N	\N	\N	Polska	\N
69	\N	bonaao@as.ld	\N	asd	\N	\N	asd	asdasd	Polska	\N
70	\N	baono@as.ld	\N	asd	\N	\N	asd	asdasd	Polska	\N
54	\N	nn@nn.ll	\N	\N	\N	\N	\N	\N	Polska	\N
49	\N	asaaaa@wp.pl	\N	\N	\N	\N	\N	\N	Polska	\N
62	\N	asdas@ad.kj	\N	qwe	qwe	\N	qwe	qwe	Polska	\N
17	\N	a.soch@wp.pl	\N	Browarnica	4	\N	Borkowo	\N	Polska	\N
71	\N	matthewcfc@gmail.com	\N	Bażyńskiego	22	\N	Skarszewy	83-250	Polska	\N
72	\N	bob@wp.pl	50655331	Lolo	23	\N	Błonie	12-222	Polska	\N
34	\N	Bonobo@as.sd	\N	\N	\N	\N	\N	\N	Polska	\N
28	\N	marekszczuk@gmail.com	505193455	Niewiadomska	\N	\N	\N	\N	Polska	\N
73	\N	bobo@pl.de	\N	\N	\N	\N	\N	\N	Polska	\N
37	\N	\N	\N	\N	11	\N	\N	\N	Polska	\N
55	\N	sazy@wp.pl	777222111	Gdańska	11	11	Żerom	88-111	Polska	\N
74	\N	Boobbb@tr.pa	\N	\N	\N	\N	\N	\N	Polska	\N
75	\N	bomba@wp.de	\N	\N	\N	\N	\N	\N	Polska	\N
76	\N	Boobbb@tr.paa	\N	\N	\N	\N	\N	\N	Polska	\N
77	\N	asd@qw.ui	\N	\N	\N	\N	\N	\N	Polska	\N
\.


--
-- Data for Name: auth_group; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_group (id, name) FROM stdin;
1	test_worker
2	workers
\.


--
-- Data for Name: auth_group_permissions; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_group_permissions (id, group_id, permission_id) FROM stdin;
1	1	16
2	1	17
3	2	82
\.


--
-- Data for Name: auth_permission; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_permission (id, name, content_type_id, codename) FROM stdin;
1	Can add log entry	1	add_logentry
2	Can change log entry	1	change_logentry
3	Can delete log entry	1	delete_logentry
4	Can add permission	2	add_permission
5	Can change permission	2	change_permission
6	Can delete permission	2	delete_permission
7	Can add group	3	add_group
8	Can change group	3	change_group
9	Can delete group	3	delete_group
10	Can add user	4	add_user
11	Can change user	4	change_user
12	Can delete user	4	delete_user
13	Can add content type	5	add_contenttype
14	Can change content type	5	change_contenttype
15	Can delete content type	5	delete_contenttype
16	Can add session	6	add_session
17	Can change session	6	change_session
18	Can delete session	6	delete_session
19	Can add Adres	31	add_address
20	Can change Adres	31	change_address
21	Can delete Adres	31	delete_address
22	Can add Powód blokady	32	add_clblockedreasondict
23	Can change Powód blokady	32	change_clblockedreasondict
24	Can delete Powód blokady	32	delete_clblockedreasondict
25	Can add Komunikacja z klientem	33	add_clcommunicationlog
26	Can change Komunikacja z klientem	33	change_clcommunicationlog
27	Can delete Komunikacja z klientem	33	delete_clcommunicationlog
28	Can add Powod komunikacji	34	add_clcommunicationreason
29	Can change Powod komunikacji	34	change_clcommunicationreason
30	Can delete Powod komunikacji	34	delete_clcommunicationreason
31	Can add Znizka dla klienta	35	add_cldiscount
32	Can change Znizka dla klienta	35	change_cldiscount
33	Can delete Znizka dla klienta	35	delete_cldiscount
34	Can add Parametry modulu klienta	22	add_clparams
35	Can change Parametry modulu klienta	22	change_clparams
36	Can delete Parametry modulu klienta	22	delete_clparams
37	Can add Platnosc klienta	30	add_clpayment
38	Can change Platnosc klienta	30	change_clpayment
39	Can delete Platnosc klienta	30	delete_clpayment
40	Can add Linia platnosci klienta	29	add_clpaymentline
41	Can change Linia platnosci klienta	29	change_clpaymentline
42	Can delete Linia platnosci klienta	29	delete_clpaymentline
43	Can add Niepotwierdzony klient	36	add_clunconfirmed
44	Can change Niepotwierdzony klient	36	change_clunconfirmed
45	Can delete Niepotwierdzony klient	36	delete_clunconfirmed
46	Can add Oddzial	13	add_companybranch
47	Can change Oddzial	13	change_companybranch
48	Can delete Oddzial	13	delete_companybranch
49	Can add Typ kontaktu	10	add_contacttype
50	Can change Typ kontaktu	10	change_contacttype
51	Can delete Typ kontaktu	10	delete_contacttype
52	Can add Panstwo	11	add_countrydict
53	Can change Panstwo	11	change_countrydict
54	Can delete Panstwo	11	delete_countrydict
55	Can add Waluta	23	add_currrency
56	Can change Waluta	23	change_currrency
57	Can delete Waluta	23	delete_currrency
58	Can add Znizka	37	add_discountdict
59	Can change Znizka	37	change_discountdict
60	Can delete Znizka	37	delete_discountdict
61	Can add discount scope	38	add_discountscope
62	Can change discount scope	38	change_discountscope
63	Can delete discount scope	38	delete_discountscope
64	Can add django migrations	39	add_djangomigrations
65	Can change django migrations	39	change_djangomigrations
66	Can delete django migrations	39	delete_djangomigrations
67	Can add Lokalizacja	17	add_location
68	Can change Lokalizacja	17	change_location
69	Can delete Lokalizacja	17	delete_location
70	Can add Typ lokacji	16	add_locationtype
71	Can change Typ lokacji	16	change_locationtype
72	Can delete Typ lokacji	16	delete_locationtype
73	Can add Maszyna	18	add_machine
74	Can change Maszyna	18	change_machine
75	Can delete Maszyna	18	delete_machine
76	Can add Typ maszyny	19	add_machinetype
77	Can change Typ maszyny	19	change_machinetype
78	Can delete Typ maszyny	19	delete_machinetype
79	Can add Uzycie zasobu	40	add_resourcesusage
80	Can change Uzycie zasobu	40	change_resourcesusage
81	Can delete Uzycie zasobu	40	delete_resourcesusage
82	Can add Parametry modulu wykorzystania zasobow	20	add_resourcesusageparams
83	Can change Parametry modulu wykorzystania zasobow	20	change_resourcesusageparams
84	Can delete Parametry modulu wykorzystania zasobow	20	delete_resourcesusageparams
85	Can add Serwis	25	add_sedict
86	Can change Serwis	25	change_sedict
87	Can delete Serwis	25	delete_sedict
88	Can add Znizka na serwis	41	add_sediscount
89	Can change Znizka na serwis	41	change_sediscount
90	Can delete Znizka na serwis	41	delete_sediscount
91	Can add Wymaganie serwisu	28	add_serequirement
92	Can change Wymaganie serwisu	28	change_serequirement
93	Can delete Wymaganie serwisu	28	delete_serequirement
94	Can add Serwis	15	add_service
95	Can change Serwis	15	change_service
96	Can delete Serwis	15	delete_service
97	Can add Serwis archiwalny	42	add_servicearchived
98	Can change Serwis archiwalny	42	change_servicearchived
99	Can delete Serwis archiwalny	42	delete_servicearchived
100	Can add Plec	7	add_sexdict
101	Can change Plec	7	change_sexdict
102	Can delete Plec	7	delete_sexdict
103	Can add Sloty czasowe	43	add_timeslotlist
104	Can change Sloty czasowe	43	change_timeslotlist
105	Can delete Sloty czasowe	43	delete_timeslotlist
106	Can add Parametry slotow czasowych	24	add_timeslotparams
107	Can change Parametry slotow czasowych	24	change_timeslotparams
108	Can delete Parametry slotow czasowych	24	delete_timeslotparams
109	Can add Umiejetnosc pracownika	27	add_woability
110	Can change Umiejetnosc pracownika	27	change_woability
111	Can delete Umiejetnosc pracownika	27	delete_woability
112	Can add Umiejetnosc	26	add_woabilitydict
113	Can change Umiejetnosc	26	change_woabilitydict
114	Can delete Umiejetnosc	26	delete_woabilitydict
115	Can add Grupa umiejetnosci	44	add_woabilitygroupdict
116	Can change Grupa umiejetnosci	44	change_woabilitygroupdict
117	Can delete Grupa umiejetnosci	44	delete_woabilitygroupdict
118	Can add Absencja pracownika	45	add_woabsence
119	Can change Absencja pracownika	45	change_woabsence
120	Can delete Absencja pracownika	45	delete_woabsence
121	Can add Absencja	46	add_woabsencetype
122	Can change Absencja	46	change_woabsencetype
123	Can delete Absencja	46	delete_woabsencetype
124	Can add Grupa - pracownik	47	add_wogroup
125	Can change Grupa - pracownik	47	change_wogroup
126	Can delete Grupa - pracownik	47	delete_wogroup
127	Can add Grupa (nazwa slownikowa)	48	add_wogroupdict
128	Can change Grupa (nazwa slownikowa)	48	change_wogroupdict
129	Can delete Grupa (nazwa slownikowa)	48	delete_wogroupdict
130	Can add Uprawnienie grupy	49	add_wogroupprivilege
131	Can change Uprawnienie grupy	49	change_wogroupprivilege
132	Can delete Uprawnienie grupy	49	delete_wogroupprivilege
133	Can add Alert pracownika	50	add_wonotification
134	Can change Alert pracownika	50	change_wonotification
135	Can delete Alert pracownika	50	delete_wonotification
136	Can add Uprawnienie pracownika	51	add_woprivilegedict
137	Can change Uprawnienie pracownika	51	change_woprivilegedict
138	Can delete Uprawnienie pracownika	51	delete_woprivilegedict
139	Can add Poziom uprawnienia pracownika	52	add_woprivilegeleveldict
140	Can change Poziom uprawnienia pracownika	52	change_woprivilegeleveldict
141	Can delete Poziom uprawnienia pracownika	52	delete_woprivilegeleveldict
142	Can add Kaledarz roboczy	14	add_workdaycalendar
143	Can change Kaledarz roboczy	14	change_workdaycalendar
144	Can delete Kaledarz roboczy	14	delete_workdaycalendar
145	Can add Paramatry kalendarza	21	add_workdaycalendarparams
146	Can change Paramatry kalendarza	21	change_workdaycalendarparams
147	Can delete Paramatry kalendarza	21	delete_workdaycalendarparams
148	Can add Pracownik	8	add_worker
149	Can change Pracownik	8	change_worker
150	Can delete Pracownik	8	delete_worker
151	Can add Pracownik - user	53	add_wouser
152	Can change Pracownik - user	53	change_wouser
153	Can delete Pracownik - user	53	delete_wouser
154	Can add Kod pocztowy	54	add_zipcodesdict
155	Can change Kod pocztowy	54	change_zipcodesdict
156	Can delete Kod pocztowy	54	delete_zipcodesdict
157	Can add Klinet	12	add_client
158	Can change Klinet	12	change_client
159	Can delete Klinet	12	delete_client
160	Can add Maszyna	55	add_machine
161	Can change Maszyna	55	change_machine
162	Can delete Maszyna	55	delete_machine
163	Can add Typ maszyny	56	add_machinetype
164	Can change Typ maszyny	56	change_machinetype
165	Can delete Typ maszyny	56	delete_machinetype
\.


--
-- Data for Name: auth_user; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_user (id, password, last_login, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined) FROM stdin;
11	pbkdf2_sha256$36000$YPTl6SAptB8H$oWOUpsAv5FYxb5mbJxiGlFPjTRarQWk4JcThQgOEAGE=	2018-01-05 13:46:58.82383+00	f	Janusz				f	t	2017-11-24 12:00:13.211229+00
8	pbkdf2_sha256$36000$QONb4Sjn1lL0$oPy3i0Fs/5NtqJdkDL6Gg2VVj0pQDJ3QAPPkcQBrbHc=	2018-01-05 13:47:18.51661+00	f	Dario				f	t	2017-11-24 11:05:32.580983+00
26	pbkdf2_sha256$36000$sd1wLLfw5VdX$gyhg4JfzgRBWYrDbj4zhe7qUnIC+AXwuwudjwFfNtbA=	2018-01-06 10:10:58.780427+00	f	bob				f	t	2018-01-06 10:10:48.863649+00
27	pbkdf2_sha256$36000$dn6eFFw6x0a7$GEuVULySr6xHUgJBAiJpk+EemWYa8eu+CgBXeEaMAOM=	2018-01-06 10:12:25.772409+00	f	boba				f	t	2018-01-06 10:12:13.319603+00
13	pbkdf2_sha256$36000$heHolwfq8ugl$AZZAuZRJap8otLPbtjSttICRcDCFX627ko8YYqKOhqA=	\N	f	klient				f	t	2017-12-16 10:25:23+00
29	pbkdf2_sha256$36000$iVi1RiN8KosT$EDLyMHq0clYcze0Vl3pwE9WeHUnUaDQOoUDZ6jU8ioE=	2018-01-10 10:05:29.636492+00	f	mati				f	t	2018-01-09 17:07:56.712864+00
28	pbkdf2_sha256$36000$cjHvWDawexyO$w1VWBi1FZHt4SihxncH2zbRx81jK5RGiAeJ6fENyf34=	2018-01-23 19:50:01.73051+00	f	klient4				f	t	2018-01-06 14:32:37.585028+00
34	pbkdf2_sha256$36000$Z5ChowB21xCV$tN08K/ITjDz8r2a5QmjPWSfPv0MLsWaiJ07k3vNlebs=	2018-01-23 20:12:55.638124+00	f	pracownik				f	t	2018-01-12 21:51:22+00
12	pbkdf2_sha256$36000$qC9RHlo5NkFm$gVSW4j5eXLvh8+qkp7M47NakvUHH0sn8s3GexRl+nAI=	2017-11-24 12:06:11.632797+00	f	Jarko				f	t	2017-11-24 12:06:10.973759+00
1	pbkdf2_sha256$36000$X68dTIRLJOEw$i0UsCmQKaRROSHnQ/hKk4zq+yQz014u4Slv680mfDEw=	2018-01-23 21:16:12.962694+00	t	ks			krzysztof@sazon.pl	t	t	2017-11-05 11:24:22+00
31	pbkdf2_sha256$36000$EoFYWJehdgDh$guWKJCPkeTAL6FIc18G9IQ1taM0CpGU+ZNlAxF+CIak=	2018-01-10 11:51:58.591102+00	f	matiz				f	t	2018-01-10 07:55:40.476477+00
5	pbkdf2_sha256$36000$ARrNdQ27Kcwm$EfHsuePkK3+qwpgWdTtqjMjTlHQTAVawf91K5/df34A=	2018-01-26 20:13:18.261813+00	f	mariusz				f	t	2017-11-22 09:03:33+00
14	pbkdf2_sha256$36000$XBrQPgz0VzOb$bHzIjcG05DOeUEpzp4YSrqbElwvh6DW8L/gApC/pe5I=	2018-01-06 00:10:07.222938+00	f	klient2				f	t	2017-12-29 17:28:30.882072+00
33	pbkdf2_sha256$36000$08aCrr7Xmt1v$02112DpFjTEngb5JDlpHDmYGaNFAVs1jzGUdZXkqWSM=	\N	f	prezentacja				f	t	2018-01-12 21:35:17.652342+00
6	pbkdf2_sha256$36000$911Up1bx38mN$ZzXGcxkwsaAduomrml0VFmMTdKhl3iHX5I2YrNspeRk=	2017-11-22 18:33:38.063666+00	f	pipa				f	t	2017-11-22 18:33:36.30456+00
32	pbkdf2_sha256$36000$DnQNNIg93SFY$l43zEXgsXEU3dXRLYb+x9dWmpfhRY4JmYdHdgFh8a6w=	2018-01-12 21:36:13.04305+00	f	klient5				f	t	2018-01-11 23:05:06.403061+00
3	pbkdf2_sha256$36000$eq1SHs4O1pwq$U1ltjIsXNEshxs8PspJuzOPeHGUJeMA7ub6AWVQ+Phs=	2018-01-12 21:43:35.099813+00	f	wiesiek				f	t	2017-11-11 15:49:53+00
17	pbkdf2_sha256$36000$M3Q1GcpXxOTp$N/V+Jd3rpKMZi0Ak2ghDUyzbviODRYu92hlZ1p9tgIU=	2018-01-06 09:42:32.202943+00	f	asdasdasdasdasd				f	t	2018-01-06 09:42:31.641911+00
19	pbkdf2_sha256$36000$R5k2nNEt9EJc$YsJgtoC7+JZzIqtCDkkDUiVI2tQ3QyuVkm+7NgjjoYg=	\N	f	asdsadasdasdasd				f	t	2018-01-06 09:48:39.394303+00
20	pbkdf2_sha256$36000$9VB1H0gKiNRs$+jqSfWr20kgQrUy73xhRPipuLm2W5+pkEZHyDn2CTwU=	2018-01-06 09:53:00.95557+00	f	asdsadasdasdasdasd				f	t	2018-01-06 09:53:00.225528+00
21	pbkdf2_sha256$36000$0QcnjHBUebZL$I2hYAlUDgr3gulMHuXfjMB9pjFx30AtyURyu+7pKHsU=	2018-01-06 09:56:15.808905+00	f	malwina				f	t	2018-01-06 09:56:15.032861+00
4	pbkdf2_sha256$36000$j849M0mZV3fH$2yTolC0KcokRkaH/x90MyK5FGEr6I+iA74aKp217UFo=	2018-01-10 13:00:46.345312+00	t	mateusz				t	t	2017-11-19 15:27:26+00
7	pbkdf2_sha256$36000$vrZl9s0ZKN49$f2HxwHTlEfsPQp4NnJtvXwTMMT/Miy5IFFQpiuzxjm4=	2018-03-21 18:59:38.235867+00	t	adm				t	t	2017-11-23 16:16:50+00
30	pbkdf2_sha256$36000$q8j62ZjlQwCf$WvrJ902OnQNZmEGXVKiOZ4zxZs39VNirK4h29gd20Bo=	\N	f	Bobo				f	t	2018-01-09 21:06:04.333049+00
2	pbkdf2_sha256$36000$30RFF16EfU8o$w0cHJJatLeIWXbsTpb+X/WAnhqufGObz6utosZJUh20=	2018-05-25 07:31:03.020472+00	t	marek				t	t	2017-11-06 10:24:30+00
\.


--
-- Data for Name: auth_user_groups; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_user_groups (id, user_id, group_id) FROM stdin;
1	3	1
2	5	2
3	2	2
4	1	2
5	7	1
6	7	2
7	34	2
\.


--
-- Data for Name: auth_user_user_permissions; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY auth_user_user_permissions (id, user_id, permission_id) FROM stdin;
1	7	1
2	7	2
3	7	3
4	7	4
5	7	5
6	7	6
7	7	7
8	7	8
9	7	9
10	7	10
11	7	11
12	7	12
13	7	13
14	7	14
15	7	15
16	7	16
17	7	17
18	7	18
19	7	19
20	7	20
21	7	21
22	7	22
23	7	23
24	7	24
25	7	25
26	7	26
27	7	27
28	7	28
29	7	29
30	7	30
31	7	31
32	7	32
33	7	33
34	7	34
35	7	35
36	7	36
37	7	37
38	7	38
39	7	39
40	7	40
41	7	41
42	7	42
43	7	43
44	7	44
45	7	45
46	7	46
47	7	47
48	7	48
49	7	49
50	7	50
51	7	51
52	7	52
53	7	53
54	7	54
55	7	55
56	7	56
57	7	57
58	7	58
59	7	59
60	7	60
61	7	61
62	7	62
63	7	63
64	7	64
65	7	65
66	7	66
67	7	67
68	7	68
69	7	69
70	7	70
71	7	71
72	7	72
73	7	73
74	7	74
75	7	75
76	7	76
77	7	77
78	7	78
79	7	79
80	7	80
81	7	81
82	7	82
83	7	83
84	7	84
85	7	85
86	7	86
87	7	87
88	7	88
89	7	89
90	7	90
91	7	91
92	7	92
93	7	93
94	7	94
95	7	95
96	7	96
97	7	97
98	7	98
99	7	99
100	7	100
101	7	101
102	7	102
103	7	103
104	7	104
105	7	105
106	7	106
107	7	107
108	7	108
109	7	109
110	7	110
111	7	111
112	7	112
113	7	113
114	7	114
115	7	115
116	7	116
117	7	117
118	7	118
119	7	119
120	7	120
121	7	121
122	7	122
123	7	123
124	7	124
125	7	125
126	7	126
127	7	127
128	7	128
129	7	129
130	7	130
131	7	131
132	7	132
133	7	133
134	7	134
135	7	135
136	7	136
137	7	137
138	7	138
139	7	139
140	7	140
141	7	141
142	7	142
143	7	143
144	7	144
145	7	145
146	7	146
147	7	147
148	7	148
149	7	149
150	7	150
151	7	151
152	7	152
153	7	153
154	7	154
155	7	155
156	7	156
157	7	157
158	7	158
159	7	159
160	7	160
161	7	161
162	7	162
163	7	163
164	7	164
165	7	165
\.


--
-- Data for Name: cl_blocked_reason_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_blocked_reason_dict (id_cl_blocked_reason_dict, blocked_reason_name) FROM stdin;
BRZY	Bo brzydki jak diabeł
\.


--
-- Data for Name: cl_communication_log; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_communication_log (id_cl_communication_log, client, communication_reason, service, contact_type, contact_address, message_body, minutes_before_action, notes, created_datetime, created_by, company_branch) FROM stdin;
\.


--
-- Data for Name: cl_communication_reason; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_communication_reason (id_client_communication_reason, reason_name) FROM stdin;
ZAKO	Informacja o zakończeniu
PRZYPLA	Przypomnienie o płatności
\.


--
-- Data for Name: cl_discount; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_discount (id_cl_discount, client, discount, company_branch) FROM stdin;
1	1	STALY10P	main
\.


--
-- Data for Name: cl_params; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_params (max_debt, allow_new_no_contact, default_reminder_sms_minutes, default_reminder_email_minutes, default_finished_info_sms, default_finished_info_email, max_worktime_wo_conf_minutes, default_currency, company_branch, cl_params_id) FROM stdin;
$4,520.00	f	0	0	f	f	1500	PLN	main	1
\.


--
-- Data for Name: cl_payment; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_payment (id_cl_payment, is_closed, payment_name, client, is_invoice, contact, invoice_voucher, payment_sum, paid_amount, currency, paid_datetime, posted_datetime, due_date, notes, created_datetime, created_by, company_branch) FROM stdin;
1	f	\N	2	t	1	\N	$120.00	$44.00	PLN	\N	\N	\N	\N	\N	\N	main
\.


--
-- Data for Name: cl_payment_line; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_payment_line (id_cl_payment_line, payment, service, text_on_invoice, qty, final_price, currency, company_branch) FROM stdin;
1	1	2	tekst	1	$120.00	PLN	main
\.


--
-- Data for Name: cl_unconfirmed; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY cl_unconfirmed (cl_unconfirmed, first_name, last_name, client_name, created_datetime, ip_address, email, phone, default_company_branch) FROM stdin;
\.


--
-- Data for Name: client; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY client (id_client, sex, first_name, last_name, is_company, client_name, nip, address, is_blocked, blocked_reason_id, blocked_notes, default_invoice, default_reminder_sms_minutes, is_confirmed, is_rejected, ip_address, default_reminder_email_minutes, default_finished_info_sms, default_finished_info_email, client_discount_percent_sum, notes, default_company_branch, email, phone, prefered_contact_type, client_user_login) FROM stdin;
66	\N	Bobo	Bobowski	f	\N	\N	72	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	30
6	\N	Jarko	Boczławski	f	\N	\N	17	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	12
49	M	Jarek	Krec	f	\N	\N	55	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
58	M	Klient	Klein	f	\N	\N	64	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	28
72	M	Krzysztof	Wańsk	f	\N	\N	79	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	32
33	M	Dało	Kukizz	f	\N	\N	38	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
73	F	Prez	Ent	f	\N	\N	80	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	33
28	F	Marian	Kowalski	f	\N	\N	28	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
8	F	Marenty	XXXXX	f	\N	\N	34	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	2
51	\N	Malwina	Łabadzińska	f	\N	\N	57	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	21
57	\N	ma	masd	f	\N	\N	63	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	27
34	M	ks	ks	f	\N	\N	39	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	14
3	M	Janusz	Paletko	f	\N	\N	6	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	8
2	M	Mateusz	Zaz	f	\N	\N	36	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
1	M	Krzysztof	Saz	\N	\N	\N	1	f	\N	\N	f	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
64	\N	Bono	Bono	f	\N	\N	70	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	\N
56	\N	bob	bob	f	\N	\N	62	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	26
65	M	Mateusz	Z	f	\N	\N	71	f	\N	\N	t	\N	t	f	\N	\N	\N	\N	\N	\N	main	\N	\N	\N	29
\.


--
-- Data for Name: company_branch; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY company_branch (id_company_branch, is_main, company_name, nip, address, email, phone, longitude, latitude) FROM stdin;
main	t	Warsztat Mechaniczny "Pomorończe" sp. j.	\N	1	\N	\N	19.9514120000000013	50.0062319999999971
1	f	Warsztat Mechaniczny "Pomorańcze" sp. j.	\N	2	\N	\N	19.9014300000000013	50.0117460000000023
2	f	Żono moja	123123	3	\N	\N	19.9852220000000003	50.0216240000000028
\.


--
-- Data for Name: company_description; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY company_description (id_company_description, description_long, description_short) FROM stdin;
Witam	Zapewniamy mechanikę samochodową na najwyższym poziomie.Łączymy ogromną pasję do aut, gruntowną wiedzę, praktyczne umiejętności z atrakcyjnymi cenami usług. \r\n\r\nNasi mechanicy samochodowi to prawdziwi specjaliści od wszystkich modeli aut bez względu na producenta. Nie tylko wiedzą, jak naprawić Twój samochód, ale przede wszystkim dobiorą najlepsze rozwiązanie, najbardziej optymalne pod względem technicznym, ale również finansowym. \r\nU nas Twój pojazd będzie w dobrych rękach, a Twój portfel nie ucierpi na tym znacząco. \r\n\r\nAuto Serwis Wadowicka to warsztat samochodowy z ogromnymi tradycjami. Dysponujemy nowoczesnymi narzędziami oraz urządzeniami, które pozwalają błyskawicznie i trafnie dokonać diagnozy Twojego samochodu, a także umożliwiają jego właściwe i profesjonalne naprawienie. Zapewniamy przy tym kompleksową obsługę serwisową nie tylko mieszkańcom południowej części Krakowa. \r\n\r\nNasz auto moto serwis to między innymi przeglądy rejestracyjne i testy komputerowe, sprawdzanie i ustawianie geometrii kół, wymiana opon i wulkanizacja, wymiana amortyzatorów, tłumików, kompleksowy serwis klimatyzacji i inne usługi związane z diagnostyką, konserwacją i naprawą Twojego pojazdu. Zapewniamy przy tym sprawdzone części samochodowe. Auto Serwis Wadowicka to Twój mechanik pojazdów samochodowych. \r\n\r\nCzekamy na Ciebie codziennie. \r\nZostaw Twój samochód w najlepszych rękach!	Auto Serwis Wadowicka to warsztat samochodowy z ogromnymi tradycjami. \r\nDysponujemy nowoczesnymi narzędziami oraz urządzeniami, które pozwalają błyskawicznie i trafnie dokonać diagnozy Twojego samochodu, a także umożliwiają jego właściwe i profesjonalne naprawienie. \r\nZapewniamy przy tym kompleksową obsługę serwisową nie tylko mieszkańcom południowej części Krakowa.
\.


--
-- Data for Name: contact_type; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY contact_type (id_contact_type, contact_type_name) FROM stdin;
email	E-mail
tel	Telefon
sms	Wiadomość SMS
\.


--
-- Data for Name: country_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY country_dict (country) FROM stdin;
Polska
\.


--
-- Data for Name: currrency; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY currrency (id_currency, currency_name, ratio_to_main_currency) FROM stdin;
PLN	Polski złoty	1
USD	Dolar amerykański	3.40840006
EUR	Euro	4.1711998
\.


--
-- Data for Name: discount_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY discount_dict (id_discount_dict, discount_name, scope, discount_amount, discount_percent, valid_from, valid_to) FROM stdin;
STALY10P	Bycie stałym klientem	KLIENT	\N	10	\N	\N
TANIEOPO	Promocja Tanie Oponowanie	SERWIS	10	\N	2017-11-01	2017-12-31
\.


--
-- Data for Name: discount_scope; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY discount_scope (id_discount_scope) FROM stdin;
KLIENT
SERWIS
\.


--
-- Data for Name: django_admin_log; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY django_admin_log (id, action_time, object_id, object_repr, action_flag, change_message, content_type_id, user_id) FROM stdin;
1	2017-11-05 21:24:02.40918+00	M	M	1	[{"added": {}}]	7	1
2	2017-11-05 21:24:10.449564+00	M	M	2	[]	7	1
3	2017-11-05 21:24:20.109001+00	F	F	1	[{"added": {}}]	7	1
4	2017-11-05 21:24:47.11924+00	U	U	1	[{"added": {}}]	7	1
5	2017-11-05 21:28:11.430575+00	email	email	1	[{"added": {}}]	10	1
6	2017-11-05 21:28:48.449546+00	Polska	Polska	1	[{"added": {}}]	11	1
7	2017-11-05 21:28:54.473522+00	1	1	1	[{"added": {}}]	9	1
8	2017-11-05 21:30:22.044086+00	2	Stateczny	1	[{"added": {}}]	8	1
9	2017-11-05 21:38:38.512092+00	main	main	1	[{"added": {}}]	13	1
10	2017-11-05 21:38:44.623175+00	1	Krzysztof Saz None	1	[{"added": {}}]	12	1
11	2017-11-05 21:44:14.939189+00	2017-11-06	main	1	[{"added": {}}]	14	1
12	2017-11-07 20:46:50.876544+00	1	Poczdamski	2	[{"changed": {"fields": ["first_name", "last_name", "worker_title"]}}]	8	1
13	2017-11-07 20:47:29.57734+00	PODN	Podnośnik duży	1	[{"added": {}}]	16	1
14	2017-11-07 20:48:26.528708+00	1	PODN1	1	[{"added": {}}]	17	1
15	2017-11-07 20:49:01.918634+00	WIER	WIER	1	[{"added": {}}]	19	1
16	2017-11-07 20:49:16.116713+00	1	Wiertarka Bosh 1	1	[{"added": {}}]	18	1
17	2017-11-07 20:49:37.748967+00	1	Bosh 1	2	[{"changed": {"fields": ["machine_name"]}}]	18	1
18	2017-11-07 20:50:02.190643+00	2	Hilti	1	[{"added": {}}]	18	1
19	2017-11-07 20:57:02.343668+00	SPRE	SPRE	1	[{"added": {}}]	19	1
20	2017-11-07 21:15:19.87458+00	3	Stara	1	[{"added": {}}]	18	1
21	2017-11-07 21:19:20.601192+00	SZLI	SZLI	1	[{"added": {}}]	19	1
22	2017-11-07 21:19:31.392957+00	4	Makita	1	[{"added": {}}]	18	1
23	2017-11-07 21:20:07.895572+00	2	Mateusz Zaz None	1	[{"added": {}}]	12	1
24	2017-11-07 21:36:00.643402+00	1	Parametry modulu wykorzystania zasobow	1	[{"added": {}}]	20	1
25	2017-11-07 22:11:02.973054+00	1	nie wiem, costam	1	[{"added": {}}]	21	1
26	2017-11-07 22:11:54.474051+00	PLN	Currrency object	1	[{"added": {}}]	23	1
27	2017-11-07 22:12:18.409391+00	1	main	1	[{"added": {}}]	22	1
28	2017-11-07 22:12:37.141636+00	1	Parametry slotow czasowych	1	[{"added": {}}]	24	1
29	2017-11-07 22:13:36.955549+00	PODNMA	Podnośnik mały	1	[{"added": {}}]	16	1
30	2017-11-07 22:13:48.236518+00	WOPO	WOPO	1	[{"added": {}}]	25	1
31	2017-11-07 22:14:19.00413+00	tel	tel	1	[{"added": {}}]	10	1
32	2017-11-07 22:14:30.640948+00	sms	sms	1	[{"added": {}}]	10	1
33	2017-11-07 22:14:51.660116+00	WYOPO	WYOPO	1	[{"added": {}}]	26	1
34	2017-11-07 22:15:06.770148+00	FREFE	FREFE	1	[{"added": {}}]	26	1
35	2017-11-07 22:15:55.050328+00	1	1	1	[{"added": {}}]	27	1
36	2017-11-07 22:16:01.489447+00	2	2	1	[{"added": {}}]	27	1
37	2017-11-07 22:16:07.899463+00	3	3	1	[{"added": {}}]	27	1
38	2017-11-09 21:18:58.978404+00	1	z __str__: 1	1	[{"added": {}}]	15	1
39	2017-11-09 21:32:17.399382+00	2	z __str__: 2	1	[{"added": {}}]	15	1
40	2017-11-09 21:36:14.026526+00	2	z __str__: 2	2	[{"changed": {"fields": ["min_start_datetime", "planned_start", "planned_end"]}}]	15	1
41	2017-11-09 21:50:50.758465+00	2	z __str__: 2	2	[{"changed": {"fields": ["planned_start", "planned_end"]}}]	15	1
42	2017-11-09 21:55:50.941534+00	2017-12-01	2017-12-01, main	1	[{"added": {}}]	14	1
43	2017-11-09 21:56:07.472297+00	2017-12-02	2017-12-02, main	1	[{"added": {}}]	14	1
44	2017-11-09 21:56:28.863504+00	2017-12-03	2017-12-03, main	1	[{"added": {}}]	14	1
45	2017-11-09 21:56:45.015546+00	2017-12-04	2017-12-04, main	1	[{"added": {}}]	14	1
46	2017-11-09 22:02:50.516212+00	1	1	1	[{"added": {}}]	30	1
47	2017-11-09 22:03:26.121653+00	1	1	1	[{"added": {}}]	29	1
48	2017-11-09 22:04:15.911149+00	2	Mateusz Zaz None	2	[]	12	1
49	2017-11-09 22:13:07.317386+00	2	2	1	[{"added": {}}]	9	1
50	2017-11-09 23:18:23.738306+00	3	3, Lawinowa, Lakie, 2/3	1	[{"added": {}}]	31	1
51	2017-11-09 23:18:31.668783+00	3	Szybki	1	[{"added": {}}]	8	1
52	2017-11-11 15:49:19.953447+00	1	test_worker	1	[{"added": {}}]	3	1
53	2017-11-11 15:49:53.20388+00	3	wiesiek	1	[{"added": {}}]	4	1
54	2017-11-11 15:50:21.55663+00	3	wiesiek	2	[{"changed": {"fields": ["groups"]}}]	4	1
55	2017-11-15 12:50:34.238374+00	2	2	1	[{"added": {}}]	13	2
56	2017-11-15 12:50:58.191174+00	5	Maszynka do mięsa	1	[{"added": {}}]	55	2
57	2017-11-17 11:47:10.082231+00	main	main	2	[{"changed": {"fields": ["company_name"]}}]	13	2
58	2017-11-17 12:19:27.283149+00	1	1	2	[{"changed": {"fields": ["id_company_branch"]}}]	13	2
59	2017-11-17 12:39:32.651845+00	1	1	2	[{"changed": {"fields": ["is_main"]}}]	13	2
60	2017-11-17 14:08:09.119622+00	Powitanie	Powitanie	1	[{"added": {}}]	57	2
61	2017-11-17 14:08:09.26663+00	Powitanie	Powitanie	1	[{"added": {}}]	57	2
62	2017-11-17 14:11:01.48841+00	Witam	Witam	1	[{"added": {}}]	57	2
63	2017-11-17 14:15:58.042105+00	Witam	Witam	2	[{"changed": {"fields": ["description_long", "description_short"]}}]	57	2
64	2017-11-17 18:41:12.90079+00	3	z __str__: 3	1	[{"added": {}}]	58	1
65	2017-11-17 18:41:52.830943+00	4	z __str__: 4	1	[{"added": {}}]	58	1
66	2017-11-17 18:42:31.655298+00	5	z __str__: 5	1	[{"added": {}}]	58	1
67	2017-11-17 18:44:42.498946+00	2	2, strit, sity, 3/hal/2	2	[{"changed": {"fields": ["zip"]}}]	59	2
68	2017-11-17 19:04:24.387546+00	1	1, Kurnicza, Wzgórze, 1/1	2	[{"changed": {"fields": ["city", "zip"]}}]	59	2
69	2017-11-17 19:05:01.958695+00	2	2	2	[{"changed": {"fields": ["address"]}}]	13	2
70	2017-11-17 19:05:45.987213+00	1	1	2	[{"changed": {"fields": ["address"]}}]	13	2
71	2017-11-17 19:06:19.489188+00	2	2	1	[{"added": {}}]	60	1
72	2017-11-17 19:07:08.988801+00	3	3	1	[{"added": {}}]	60	1
73	2017-11-17 19:07:34.932798+00	4	4	1	[{"added": {}}]	60	1
74	2017-11-17 19:08:52.281869+00	2	2, Wiesława IV, Nowy Sącz, 3/2	2	[{"changed": {"fields": ["street", "house_no", "city"]}}]	59	2
75	2017-11-17 19:09:34.163264+00	3	3, Lawinowa, Lakie, 2/3	2	[{"changed": {"fields": ["email", "phone", "zip"]}}]	59	2
76	2017-11-17 19:55:22.124797+00	2	z __str__: 2	2	[{"changed": {"fields": ["is_confirmed", "min_start_datetime", "planned_start", "planned_end"]}}]	58	1
77	2017-11-17 19:56:36.820934+00	2	PODN2	1	[{"added": {}}]	17	1
78	2017-11-17 19:57:10.749385+00	3	z __str__: 3	2	[{"changed": {"fields": ["location", "planned_start", "planned_end"]}}]	58	1
79	2017-11-17 19:57:52.904796+00	4	z __str__: 4	2	[{"changed": {"fields": ["is_confirmed", "planned_start", "planned_end"]}}]	58	1
80	2017-11-17 19:58:28.322396+00	5	z __str__: 5	2	[{"changed": {"fields": ["location", "planned_start", "planned_end"]}}]	58	1
81	2017-11-17 19:58:48.227396+00	6	z __str__: 6	1	[{"added": {}}]	58	1
82	2017-11-17 19:59:02.982589+00	7	z __str__: 7	1	[{"added": {}}]	58	1
83	2017-11-17 19:59:16.761375+00	8	z __str__: 8	1	[{"added": {}}]	58	1
84	2017-11-17 19:59:29.390814+00	9	z __str__: 9	1	[{"added": {}}]	58	1
85	2017-11-17 19:59:55.19559+00	10	z __str__: 10	1	[{"added": {}}]	58	1
86	2017-11-17 20:04:03.91366+00	6	spre1	1	[{"added": {}}]	55	1
87	2017-11-17 20:04:15.782478+00	7	spre2	1	[{"added": {}}]	55	1
88	2017-11-17 20:04:54.11719+00	4	Adamski	1	[{"added": {}}]	61	1
89	2017-11-17 20:05:43.551861+00	2	2	2	[{"changed": {"fields": ["machine", "worker", "finish_timestamp"]}}]	60	1
90	2017-11-17 20:09:09.520145+00	5	Basienko	1	[{"added": {}}]	61	1
91	2017-11-17 20:09:32.945853+00	3	3	2	[{"changed": {"fields": ["machine", "worker", "start_timestamp", "finish_timestamp"]}}]	60	1
92	2017-11-17 20:10:07.950171+00	6	Cezary	1	[{"added": {}}]	61	1
93	2017-11-17 20:10:19.243245+00	4	4	2	[{"changed": {"fields": ["service", "worker", "finish_timestamp"]}}]	60	1
94	2017-11-17 20:11:24.181262+00	5	5	1	[{"added": {}}]	60	1
95	2017-11-17 20:12:07.83296+00	6	6	1	[{"added": {}}]	60	1
96	2017-11-17 20:12:54.699842+00	7	7	1	[{"added": {}}]	60	1
97	2017-11-17 20:14:36.506996+00	8	8	1	[{"added": {}}]	60	1
98	2017-11-17 20:15:50.112112+00	9	9	1	[{"added": {}}]	60	1
99	2017-11-17 20:16:25.288846+00	10	10	1	[{"added": {}}]	60	1
100	2017-11-17 20:17:28.379665+00	4	4	2	[{"changed": {"fields": ["machine"]}}]	60	1
101	2017-11-17 20:44:21.374646+00	2017-12-01	2017-12-01, main	2	[{"changed": {"fields": ["work_start", "work_end"]}}]	62	1
102	2017-11-17 20:50:18.611795+00	2017-12-01	2017-12-01, main	2	[]	62	1
103	2017-11-18 11:10:24.078866+00	2	Mateusz Zaz None	2	[{"changed": {"fields": ["address"]}}]	63	1
104	2017-11-19 15:27:26.341404+00	4	mateusz	1	[{"added": {}}]	4	1
105	2017-11-19 15:27:37.219683+00	4	mateusz	2	[{"changed": {"fields": ["is_staff", "is_superuser"]}}]	4	1
106	2017-11-21 20:55:07.646622+00	BRZY	[BRZY] Bo brzydki jak diabeł	1	[{"added": {}}]	66	1
107	2017-11-21 20:57:10.526966+00	ZAKO	ZAKO	1	[{"added": {}}]	67	1
108	2017-11-21 20:57:26.629245+00	PRZYPLA	PRZYPLA	1	[{"added": {}}]	67	1
109	2017-11-21 21:03:18.555408+00	KLIENT	DiscountScope object	1	[{"added": {}}]	70	1
110	2017-11-21 21:07:27.290565+00	STALY10P	Bycie stałym klientem	1	[{"added": {}}]	69	1
111	2017-11-21 21:07:33.633981+00	1	1	1	[{"added": {}}]	68	1
112	2017-11-21 21:12:02.489452+00	1	1	1	[{"added": {}}]	71	1
113	2017-11-21 21:22:26.857554+00	1	1	2	[{"changed": {"fields": ["worker_ability"]}}]	71	1
114	2017-11-21 21:24:03.762218+00	1	1	2	[{"changed": {"fields": ["qty", "activity_sort_order"]}}]	71	1
115	2017-11-21 21:28:24.281202+00	SERWIS	SERWIS	1	[{"added": {}}]	70	1
116	2017-11-21 21:28:39.861648+00	TANIEOPO	Promocja Tanie Oponowanie	1	[{"added": {}}]	69	1
117	2017-11-21 21:28:51.672668+00	1	1	1	[{"added": {}}]	72	1
118	2017-11-22 09:03:10.632964+00	2	workers	1	[{"added": {}}]	3	2
119	2017-11-22 09:03:33.853292+00	5	mariusz	1	[{"added": {}}]	4	2
120	2017-11-22 09:03:57.472643+00	5	mariusz	2	[{"changed": {"fields": ["groups"]}}]	4	2
121	2017-11-22 18:25:13.557457+00	11	z __str__: 11	1	[{"added": {}}]	58	1
122	2017-11-22 18:43:17.15604+00	11	z __str__: 11	2	[]	58	1
123	2017-11-22 18:43:49.414238+00	11	11	1	[{"added": {}}]	60	1
124	2017-11-22 19:05:10.116362+00	12	12	1	[{"added": {}}]	78	4
125	2017-11-23 11:38:49.860711+00	6	Cezary	2	[{"changed": {"fields": ["user_login"]}}]	61	2
126	2017-11-23 11:49:09.788847+00	8	Poczdamski	1	[{"added": {}}]	61	2
127	2017-11-23 12:20:23.263061+00	4	4, Sezamkowa, Grzybno, 1/23	1	[{"added": {}}]	59	2
128	2017-11-23 12:33:50.093337+00	8	Mariusz Poczdamski	2	[{"changed": {"fields": ["address"]}}]	61	2
129	2017-11-23 13:22:33.56355+00	2	marek	2	[{"changed": {"fields": ["groups"]}}]	4	2
130	2017-11-23 13:23:39.791338+00	8	Mariusz Poczdamski	2	[{"changed": {"fields": ["user_login"]}}]	61	2
131	2017-11-23 14:59:01.786617+00	8	Mariusz Dziabałko	2	[{"changed": {"fields": ["user_login"]}}]	61	2
132	2017-11-23 15:03:02.214369+00	8	Piotr Dziabałko	2	[{"changed": {"fields": ["address"]}}]	61	2
133	2017-11-23 15:04:38.234861+00	8	Piotr Dziabałko	2	[{"changed": {"fields": ["address"]}}]	61	2
134	2017-11-24 09:52:26.343495+00	8	Mariusz Dziabałko	2	[{"changed": {"fields": ["first_name"]}}]	61	2
135	2017-11-24 10:10:54.370563+00	8	Mariusz Dziabałko	2	[{"changed": {"fields": ["address", "user_login"]}}]	61	2
136	2017-11-24 10:41:38.159479+00	2	Mateusz Zaz None	2	[{"changed": {"fields": ["address"]}}]	63	2
137	2017-11-24 11:05:32.835997+00	8	Dario	1	[{"added": {}}]	4	2
138	2017-11-24 11:06:36.681433+00	6	6, Miesiączkowo	1	[{"added": {}}]	59	2
139	2017-11-24 11:06:45.635005+00	3	Dario Kapsztadzki None	1	[{"added": {}}]	63	2
140	2017-11-24 11:57:58.977005+00	5	Janusz None None	3		63	2
141	2017-11-24 11:58:09.652344+00	4	Janusz None None	3		63	2
142	2017-11-24 11:58:45.616758+00	15	15	3		59	2
143	2017-11-24 11:58:45.791768+00	14	14	3		59	2
144	2017-11-24 11:58:45.961777+00	13	13	3		59	2
145	2017-11-24 11:58:46.131787+00	12	12	3		59	2
146	2017-11-24 11:58:46.301797+00	11	11	3		59	2
147	2017-11-24 11:58:46.473807+00	10	10	3		59	2
148	2017-11-24 11:58:46.651817+00	9	9	3		59	2
149	2017-11-24 11:58:46.825827+00	8	8	3		59	2
150	2017-11-24 11:58:46.999837+00	7	7, Polacka	3		59	2
151	2017-11-24 11:59:54.484793+00	9	Janusz	3		4	2
152	2017-11-24 11:59:54.658803+00	10	Karandasz	3		4	2
153	2017-11-24 13:38:52.011764+00	main	main	2	[{"changed": {"fields": ["company_name"]}}]	13	2
154	2017-11-27 19:45:05.443669+00	3	Adam Adamski - WYOPO	2	[{"changed": {"fields": ["worker"]}}]	64	1
155	2017-11-27 19:45:14.575408+00	1	Barbara Basienko - WYOPO	2	[{"changed": {"fields": ["worker"]}}]	64	1
156	2017-11-27 19:45:29.160506+00	2	Cezary Cezary - WYOPO	2	[{"changed": {"fields": ["worker", "worker_ability"]}}]	64	1
157	2017-11-27 19:48:18.143059+00	2	Cezary Cezary - FREFE	2	[{"changed": {"fields": ["worker_ability"]}}]	64	1
158	2017-11-27 20:50:47.26315+00	1	1	2	[{"changed": {"fields": ["location_type", "time_minutes"]}}]	71	1
159	2017-11-27 20:57:19.716528+00	2	2	2	[{"changed": {"fields": ["location"]}}]	60	1
160	2017-11-27 20:57:56.610524+00	3	3	2	[{"changed": {"fields": ["location"]}}]	60	1
161	2017-11-27 20:58:12.318803+00	4	4	2	[{"changed": {"fields": ["location"]}}]	60	1
162	2017-11-27 20:58:27.770231+00	5	5	2	[{"changed": {"fields": ["location"]}}]	60	1
163	2017-11-27 20:58:42.52379+00	6	6	2	[{"changed": {"fields": ["location"]}}]	60	1
164	2017-11-27 20:58:56.553825+00	7	7	2	[{"changed": {"fields": ["location"]}}]	60	1
165	2017-11-27 20:59:23.402949+00	8	8	2	[{"changed": {"fields": ["location"]}}]	60	1
166	2017-11-27 20:59:39.906813+00	9	9	2	[{"changed": {"fields": ["location"]}}]	60	1
167	2017-11-27 21:00:03.10956+00	10	10	2	[{"changed": {"fields": ["location"]}}]	60	1
168	2017-11-27 21:07:30.305767+00	2	Cezary Cezary - WYOPO	2	[{"changed": {"fields": ["worker_ability"]}}]	64	1
246	2017-12-25 12:13:30.778988+00	7	adm	2	[{"changed": {"fields": ["groups"]}}]	4	7
169	2017-11-27 21:08:12.917035+00	8	Tez makita	2	[{"changed": {"fields": ["machine_type"]}}]	55	1
170	2017-11-27 21:08:23.39186+00	10	Knut	2	[{"changed": {"fields": ["machine_type"]}}]	55	1
171	2017-11-27 21:08:27.637923+00	7	spre2	2	[]	55	1
172	2017-11-27 21:08:43.901268+00	5	Maszynka do mięsa	2	[{"changed": {"fields": ["machine_type"]}}]	55	1
173	2017-11-27 21:08:56.478166+00	3	Stara	2	[{"changed": {"fields": ["machine_type"]}}]	55	1
174	2017-11-27 21:09:04.922805+00	1	Bosh 1	2	[]	55	1
175	2017-12-06 14:29:36.985257+00	26	26, Gniewna	1	[{"added": {}}]	59	2
176	2017-12-06 14:30:37.044471+00	26	ertwetrwer None None	2	[{"changed": {"fields": ["address"]}}]	63	2
177	2017-12-08 13:45:29.543958+00	Witam	Witam	2	[{"changed": {"fields": ["description_long"]}}]	57	2
178	2017-12-08 13:46:29.631395+00	Powitanie	Powitanie	3		57	2
179	2017-12-08 13:46:29.804405+00	Powitanie	Powitanie	3		57	2
180	2017-12-08 13:51:52.11384+00	Witam	Witam	2	[]	57	2
181	2017-12-08 13:55:18.798662+00	Witam	Witam	2	[{"changed": {"fields": ["description_short"]}}]	57	2
182	2017-12-08 15:20:09.399827+00	2	2, Wiesława IV, Nowy Sącz, 3/2	2	[{"changed": {"fields": ["phone"]}}]	59	2
183	2017-12-08 15:21:43.496209+00	1	1, Kurnicza, Wzgórze, 1/1	2	[{"changed": {"fields": ["phone"]}}]	59	2
184	2017-12-08 18:07:16.58696+00	9	Adam Mózg	1	[{"added": {}}]	61	7
185	2017-12-08 18:35:22.820267+00	11	Krzyś S	1	[{"added": {}}]	61	1
186	2017-12-08 18:44:47.323976+00	1	ks	2	[{"changed": {"fields": ["groups"]}}]	4	1
187	2017-12-10 20:54:12.827524+00	2017-12-10	2017-12-10, main	1	[{"added": {}}]	62	1
188	2017-12-10 20:54:22.438131+00	2017-12-11	2017-12-11, main	1	[{"added": {}}]	62	1
189	2017-12-11 19:36:47.448113+00	2017-12-12	2017-12-12, main	1	[{"added": {}}]	62	1
190	2017-12-11 19:37:22.445798+00	2017-12-13	2017-12-13, main	1	[{"added": {}}]	62	1
191	2017-12-11 19:37:31.685681+00	2017-12-14	2017-12-14, main	1	[{"added": {}}]	62	1
192	2017-12-12 21:00:00.025228+00	a	SeGroupDict object	1	[{"added": {}}]	80	1
193	2017-12-12 21:00:14.806616+00	ZWIERZ	SeGroupDict object	1	[{"added": {}}]	80	1
194	2017-12-12 21:00:46.919429+00	SAMOCHOD	SeGroupDict object	1	[{"added": {}}]	80	1
195	2017-12-12 21:02:04.138625+00	KLAMKI	KLAMKI	1	[{"added": {}}]	80	1
196	2017-12-12 21:02:57.94098+00	WOPO	WOPO	2	[{"changed": {"fields": ["se_group", "base_price"]}}]	78	1
197	2017-12-12 21:06:13.943333+00	WOPO	WOPO	2	[{"changed": {"fields": ["se_group", "base_price"]}}]	78	1
198	2017-12-12 21:29:30.236361+00	WOPO	WOPO	2	[]	78	1
199	2017-12-13 13:04:18.683132+00	3	Kanał	1	[{"added": {}}]	17	4
200	2017-12-13 13:08:54.768969+00	3	Kanał	2	[]	17	4
201	2017-12-13 13:08:59.4787+00	3	Kanał	2	[]	17	4
202	2017-12-13 14:20:04.047841+00	2017-12-15	2017-12-15, main	1	[{"added": {}}]	62	2
203	2017-12-13 14:20:20.412777+00	2017-12-16	2017-12-16, main	1	[{"added": {}}]	62	2
204	2017-12-13 14:20:32.126447+00	2017-12-17	2017-12-17, main	1	[{"added": {}}]	62	2
205	2017-12-13 14:20:46.16025+00	2017-12-18	2017-12-18, main	1	[{"added": {}}]	62	2
206	2017-12-13 14:20:56.898864+00	2017-12-19	2017-12-19, main	1	[{"added": {}}]	62	2
207	2017-12-13 14:21:09.190567+00	2017-12-20	2017-12-20, main	1	[{"added": {}}]	62	2
208	2017-12-13 14:21:20.169195+00	2017-12-21	2017-12-21, main	1	[{"added": {}}]	62	2
209	2017-12-13 14:21:29.332719+00	2017-12-22	2017-12-22, main	1	[{"added": {}}]	62	2
210	2017-12-13 17:36:42.727077+00	EUR	EUR	2	[{"changed": {"fields": ["ratio_to_main_currency"]}}]	77	1
211	2017-12-13 17:37:19.975352+00	WOPO	WOPO	2	[{"changed": {"fields": ["base_price"]}}]	78	1
212	2017-12-13 20:33:16.615979+00	WOPO	WOPO	2	[]	78	1
213	2017-12-13 20:33:46.797263+00	sss	sss	2	[{"changed": {"fields": ["base_price"]}}]	78	1
214	2017-12-13 22:56:04.011846+00	WOPO	WOPO	2	[{"changed": {"fields": ["se_group"]}}]	78	4
215	2017-12-13 23:02:39.125211+00	1	1, Stanisława Stojałowskiego, Kraków, 15	2	[{"changed": {"fields": ["street", "house_no", "apartment_no", "city", "zip"]}}]	59	2
216	2017-12-13 23:25:18.090181+00	strps	strps	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "location_type", "avg_time"]}}]	78	4
217	2017-12-13 23:25:54.493832+00	sss	sss	2	[{"changed": {"fields": ["se_dict_name", "se_group", "location_type"]}}]	78	4
218	2017-12-14 23:13:58.024498+00	strps	strps	2	[{"changed": {"fields": ["notes"]}}]	78	4
219	2017-12-14 23:22:40.591076+00	strps	strps	2	[]	78	4
220	2017-12-15 05:10:32.099728+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
221	2017-12-15 05:18:15.603189+00	WOPO	WOPO	2	[]	78	4
222	2017-12-15 05:18:37.44562+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
223	2017-12-15 06:20:49.545351+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
224	2017-12-15 10:55:13.878075+00	2	2, Koszarówka, Kraków, 7/2	2	[{"changed": {"fields": ["email", "street", "house_no", "city", "zip"]}}]	59	2
225	2017-12-15 11:07:40.266766+00	3	3, Szara, Kraków, 23/2	2	[{"changed": {"fields": ["email", "street", "house_no", "apartment_no", "city", "zip"]}}]	59	2
226	2017-12-16 00:08:25.786203+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
227	2017-12-16 00:08:25.954469+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
228	2017-12-16 00:16:55.947689+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
229	2017-12-16 00:17:12.640884+00	WOPO	WOPO	2	[{"changed": {"fields": ["notes"]}}]	78	4
230	2017-12-16 10:25:23.42857+00	13	klient	1	[{"added": {}}]	4	1
231	2017-12-16 10:25:31.496068+00	13	klient	2	[]	4	1
232	2017-12-16 10:27:16.351839+00	1	1	2	[{"changed": {"fields": ["default_workday_start_time", "default_workday_end_time"]}}]	73	1
233	2017-12-16 12:46:14.313838+00	sss	sss	2	[{"changed": {"fields": ["se_group", "notes"]}}]	78	4
234	2017-12-16 12:50:07.883996+00	WOPO	WOPO	2	[{"changed": {"fields": ["se_group"]}}]	78	4
235	2017-12-16 21:01:57.061507+00	main	main	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	1
236	2017-12-16 21:02:27.493454+00	2	2	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	1
237	2017-12-16 21:02:54.718295+00	1	1	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	1
238	2017-12-21 17:46:09.235861+00	strps	strps	2	[{"changed": {"fields": ["avg_time"]}}]	78	2
239	2017-12-21 20:17:29.804941+00	2	2	1	[{"added": {}}]	71	2
240	2017-12-21 20:18:37.532186+00	strps	strps	2	[{"changed": {"fields": ["avg_time", "continous"]}}]	78	2
241	2017-12-21 20:31:03.265459+00	2	2	2	[{"changed": {"fields": ["activity_sort_order", "activity_name"]}}]	71	2
242	2017-12-22 21:34:39.60695+00	4	Bizon Szybki - FREFE	1	[{"added": {}}]	64	1
243	2017-12-23 12:29:39.134762+00	2017-12-23	2017-12-23, main	1	[{"added": {}}]	62	2
244	2017-12-23 12:29:52.985342+00	2017-12-25	2017-12-25, main	1	[{"added": {}}]	62	2
245	2017-12-23 15:33:53.157376+00	28	Marian Kowalski None	2	[{"changed": {"fields": ["last_name", "client_user_login"]}}]	63	2
247	2017-12-25 18:31:04.554226+00	7	adm	2	[{"changed": {"fields": ["user_permissions"]}}]	4	7
248	2017-12-26 09:41:09.622184+00	2	workers	2	[{"changed": {"fields": ["permissions"]}}]	3	7
249	2017-12-26 17:55:32.835593+00	1	1	1	[{"added": {}}]	84	7
250	2017-12-26 17:55:56.889149+00	1	1 Adam Mózg	1	[{"added": {}}]	83	7
251	2017-12-26 17:56:36.957277+00	1	1	1	[{"added": {}}]	81	7
252	2017-12-26 17:57:04.923526+00	2	2	1	[{"added": {}}]	81	7
253	2017-12-27 12:11:11.143942+00	61	z __str__: 61	3		58	2
254	2017-12-27 12:11:11.417958+00	60	z __str__: 60	3		58	2
255	2017-12-27 12:11:11.621969+00	59	z __str__: 59	3		58	2
256	2017-12-27 12:11:11.836982+00	58	z __str__: 58	3		58	2
257	2017-12-27 12:11:12.028993+00	57	z __str__: 57	3		58	2
258	2017-12-27 12:11:12.225004+00	56	z __str__: 56	3		58	2
259	2017-12-27 12:11:12.429016+00	55	z __str__: 55	3		58	2
260	2017-12-27 12:11:12.624027+00	54	z __str__: 54	3		58	2
261	2017-12-27 12:11:12.819038+00	53	z __str__: 53	3		58	2
262	2017-12-27 12:11:13.019049+00	52	z __str__: 52	3		58	2
263	2017-12-27 12:11:13.221061+00	51	z __str__: 51	3		58	2
264	2017-12-27 12:11:13.425073+00	50	z __str__: 50	3		58	2
265	2017-12-27 12:11:13.619084+00	49	z __str__: 49	3		58	2
266	2017-12-27 12:11:13.822095+00	48	z __str__: 48	3		58	2
267	2017-12-27 12:11:14.022107+00	47	z __str__: 47	3		58	2
268	2017-12-27 12:11:14.218118+00	46	z __str__: 46	3		58	2
269	2017-12-27 12:11:14.42113+00	45	z __str__: 45	3		58	2
270	2017-12-27 12:11:14.622141+00	44	z __str__: 44	3		58	2
271	2017-12-27 12:11:14.837153+00	43	z __str__: 43	3		58	2
272	2017-12-27 12:11:15.036165+00	42	z __str__: 42	3		58	2
273	2017-12-27 12:11:15.223175+00	41	z __str__: 41	3		58	2
274	2017-12-27 12:11:15.424187+00	40	z __str__: 40	3		58	2
275	2017-12-27 12:11:15.620198+00	39	z __str__: 39	3		58	2
276	2017-12-27 12:11:15.81921+00	38	z __str__: 38	3		58	2
277	2017-12-27 12:11:16.032222+00	37	z __str__: 37	3		58	2
278	2017-12-27 12:11:16.223233+00	36	z __str__: 36	3		58	2
279	2017-12-27 12:11:16.423244+00	35	z __str__: 35	3		58	2
280	2017-12-27 12:11:16.622255+00	34	z __str__: 34	3		58	2
281	2017-12-27 12:11:16.822267+00	33	z __str__: 33	3		58	2
282	2017-12-27 12:11:17.018278+00	32	z __str__: 32	3		58	2
283	2017-12-27 12:11:17.22129+00	31	z __str__: 31	3		58	2
284	2017-12-27 12:11:17.421301+00	30	z __str__: 30	3		58	2
285	2017-12-27 12:11:17.596311+00	29	z __str__: 29	3		58	2
286	2017-12-27 12:11:17.777322+00	28	z __str__: 28	3		58	2
287	2017-12-27 12:11:17.965332+00	27	z __str__: 27	3		58	2
288	2017-12-27 12:11:18.148343+00	26	z __str__: 26	3		58	2
289	2017-12-27 12:11:18.366355+00	25	z __str__: 25	3		58	2
290	2017-12-27 12:11:18.560366+00	24	z __str__: 24	3		58	2
291	2017-12-27 12:11:18.752377+00	23	z __str__: 23	3		58	2
292	2017-12-27 12:11:18.947388+00	22	z __str__: 22	3		58	2
293	2017-12-27 12:11:19.164401+00	21	z __str__: 21	3		58	2
294	2017-12-27 12:11:19.356412+00	20	z __str__: 20	3		58	2
295	2017-12-27 12:11:19.561424+00	19	z __str__: 19	3		58	2
296	2017-12-27 12:11:19.754435+00	18	z __str__: 18	3		58	2
297	2017-12-27 12:11:19.948446+00	17	z __str__: 17	3		58	2
298	2017-12-27 12:11:20.147457+00	16	z __str__: 16	3		58	2
299	2017-12-27 12:11:20.359469+00	15	z __str__: 15	3		58	2
300	2017-12-27 12:11:20.532479+00	14	z __str__: 14	3		58	2
301	2017-12-27 12:11:20.712489+00	13	z __str__: 13	3		58	2
302	2017-12-27 12:11:20.8915+00	12	z __str__: 12	3		58	2
303	2017-12-27 15:19:08.298592+00	8	Mariusz Dawidowski	2	[{"changed": {"fields": ["user_login"]}}]	61	2
304	2017-12-27 15:19:20.382283+00	3	Bizon Szybki	2	[{"changed": {"fields": ["user_login"]}}]	61	2
305	2017-12-28 19:21:24.587278+00	MYJK	MYJK	2	[{"changed": {"fields": ["id_machine_type"]}}]	56	7
307	2018-01-03 13:38:53.746703+00	12	Tom Pac	1	[{"added": {}}]	61	7
308	2018-01-03 13:39:34.714914+00	13	Hardy Burn	1	[{"added": {}}]	61	7
309	2018-01-03 13:40:20.186513+00	ELEKT	ELEKT	1	[{"added": {}}]	82	7
310	2018-01-03 13:40:37.389915+00	MECH	MECH	1	[{"added": {}}]	82	7
311	2018-01-03 13:41:53.486857+00	5	Tom Pac - MECH	1	[{"added": {}}]	64	7
312	2018-01-03 13:42:09.472725+00	6	Hardy Burn - ELEKT	1	[{"added": {}}]	64	7
313	2018-01-03 13:43:04.970883+00	14	Milo Corty	1	[{"added": {}}]	61	7
314	2018-01-03 13:43:53.586086+00	DIAG	DIAG	1	[{"added": {}}]	82	7
315	2018-01-03 13:44:27.034991+00	7	Milo Corty - DIAG	1	[{"added": {}}]	64	7
316	2018-01-04 12:03:49.531693+00	StBadHam	Stanowisko badania hamulców	1	[{"added": {}}]	16	7
317	2018-01-04 12:04:04.359947+00	StBadHam	Stanowisko badania hamulców	2	[{"changed": {"fields": ["location_capacity"]}}]	16	7
318	2018-01-04 12:04:42.307101+00	4	Stanowisko badania hamulców	1	[{"added": {}}]	17	7
319	2018-01-04 12:06:51.108239+00	Badham	Badham	1	[{"added": {}}]	78	7
320	2018-01-04 12:08:48.977345+00	3	3	1	[{"added": {}}]	71	7
321	2018-01-04 12:10:27.639139+00	KoUkEl	Kontrola układów elektronicznych	1	[{"added": {}}]	16	7
322	2018-01-04 12:11:05.744873+00	5	Tester układów elektronicznych	1	[{"added": {}}]	17	7
323	2018-01-04 12:12:01.714357+00	KoUkEl	KoUkEl	1	[{"added": {}}]	78	7
324	2018-01-04 12:12:55.340113+00	4	4	1	[{"added": {}}]	71	7
325	2018-01-04 12:14:00.372294+00	PodKol	Podnośnik Kolumnowy	1	[{"added": {}}]	16	7
326	2018-01-04 12:14:44.812794+00	StUsSw	Stanowisko do ustawiania świateł	1	[{"added": {}}]	16	7
327	2018-01-04 12:15:24.894047+00	StKoUsKo	Stanowisko kontroli ustawienia kół	1	[{"added": {}}]	16	7
328	2018-01-04 12:15:57.874012+00	6	Podnośnik kolumnowy	1	[{"added": {}}]	17	7
329	2018-01-04 12:16:30.509234+00	7	Stanowisko kontroli ustawienia swiateł	1	[{"added": {}}]	17	7
330	2018-01-04 12:17:12.034301+00	8	Diagnostyka układu kierowniczego	1	[{"added": {}}]	17	7
331	2018-01-04 12:17:33.242535+00	7	Bazowanie swiateł	2	[{"changed": {"fields": ["location_name"]}}]	17	7
332	2018-01-04 12:18:37.765199+00	WySkBi	WySkBi	1	[{"added": {}}]	78	7
333	2018-01-04 12:19:47.729993+00	DiUkKi	DiUkKi	1	[{"added": {}}]	78	7
334	2018-01-04 12:20:28.684366+00	BazSwi	BazSwi	1	[{"added": {}}]	78	7
335	2018-01-04 12:21:27.146662+00	5	5	1	[{"added": {}}]	71	7
336	2018-01-04 12:22:55.035209+00	6	6	1	[{"added": {}}]	71	7
337	2018-01-04 12:23:51.17534+00	28	Marian Kowalski None	2	[{"changed": {"fields": ["sex"]}}]	63	2
338	2018-01-04 12:25:48.576485+00	7	7	1	[{"added": {}}]	71	7
339	2018-01-04 16:39:52.836172+00	AnaSpa	Analiza spalin	1	[{"added": {}}]	16	7
340	2018-01-04 16:40:49.930615+00	9	Analiza spalin	1	[{"added": {}}]	17	7
341	2018-01-04 16:42:22.44744+00	AnaSpa	AnaSpa	1	[{"added": {}}]	78	7
342	2018-01-04 16:42:50.432117+00	AnaSpa	Analizator spalin	2	[{"changed": {"fields": ["location_type_name"]}}]	16	7
343	2018-01-04 16:43:55.104823+00	AnSp	AnSp	1	[{"added": {}}]	56	7
344	2018-01-04 16:44:48.621168+00	24	ETT 855	1	[{"added": {}}]	55	7
345	2018-01-04 16:45:48.24687+00	8	8	1	[{"added": {}}]	71	7
346	2018-01-04 17:01:34.263459+00	KLASK	KLASK	1	[{"added": {}}]	78	1
347	2018-01-05 20:57:46.999625+00	czzeb	czzeb	2	[{"changed": {"fields": ["se_group"]}}]	78	7
348	2018-01-06 16:02:42.269761+00	ZWIERZ	ZWIERZ	2	[{"changed": {"fields": ["se_group_dict_name"]}}]	80	4
349	2018-01-06 20:10:32.293772+00	ZWIERZ	ZWIERZ	2	[{"changed": {"fields": ["se_group_dict_name"]}}]	80	7
350	2018-01-06 20:14:00.455189+00	SAMOCHOD	SAMOCHOD	2	[{"changed": {"fields": ["se_group_dict_name"]}}]	80	7
351	2018-01-06 20:14:15.246423+00	SAMOCHOD	SAMOCHOD	2	[]	80	7
352	2018-01-06 20:15:45.249838+00	KLAMKI	KLAMKI	3		80	7
353	2018-01-06 20:15:56.804684+00	a	a	3		80	7
354	2018-01-06 20:31:34.500709+00	WySkBi	WySkBi	2	[{"changed": {"fields": ["notes"]}}]	78	7
355	2018-01-06 20:32:28.65707+00	KoUkEl	KoUkEl	2	[{"changed": {"fields": ["notes"]}}]	78	7
356	2018-01-06 20:33:23.442266+00	KLASK	KLASK	2	[{"changed": {"fields": ["notes"]}}]	78	7
357	2018-01-06 20:33:54.826419+00	DiUkKi	DiUkKi	2	[{"changed": {"fields": ["notes"]}}]	78	7
358	2018-01-06 20:34:26.422381+00	czzeb	czzeb	2	[{"changed": {"fields": ["notes"]}}]	78	7
359	2018-01-06 20:34:56.203429+00	BazSwi	BazSwi	2	[{"changed": {"fields": ["notes"]}}]	78	7
360	2018-01-06 20:35:24.56822+00	Badham	Badham	2	[{"changed": {"fields": ["notes"]}}]	78	7
361	2018-01-06 20:35:40.455773+00	AnaSpa	AnaSpa	2	[{"changed": {"fields": ["notes"]}}]	78	7
362	2018-01-06 20:56:02.830258+00	KLASK	KLASK	2	[{"changed": {"fields": ["notes"]}}]	78	7
363	2018-01-06 21:03:12.463422+00	myok	myok	2	[{"changed": {"fields": ["notes"]}}]	78	7
364	2018-01-06 21:03:31.121666+00	mycok	mycok	2	[{"changed": {"fields": ["notes"]}}]	78	7
365	2018-01-06 21:03:53.453662+00	mycok	mycok	2	[{"changed": {"fields": ["notes"]}}]	78	7
366	2018-01-06 21:04:21.650529+00	12	12	2	[{"changed": {"fields": ["notes"]}}]	78	7
367	2018-01-06 21:04:44.461148+00	9	9	1	[{"added": {}}]	71	1
368	2018-01-06 21:05:21.928451+00	10	10	1	[{"added": {}}]	71	1
369	2018-01-06 21:06:08.784525+00	11	11	1	[{"added": {}}]	71	1
370	2018-01-06 21:07:06.151628+00	12	12	1	[{"added": {}}]	71	1
371	2018-01-06 21:07:55.79026+00	13	13	1	[{"added": {}}]	71	1
372	2018-01-06 21:08:20.767418+00	14	14	1	[{"added": {}}]	71	1
373	2018-01-06 23:23:50.926103+00	GLAPIE	GLAPIE	1	[{"added": {}}]	78	1
374	2018-01-06 23:24:55.33104+00	BASE	BASE	1	[{"added": {}}]	82	1
375	2018-01-06 23:24:58.522486+00	15	15	1	[{"added": {}}]	71	1
376	2018-01-06 23:25:18.324205+00	15	15	2	[{"changed": {"fields": ["machine_type", "location_type"]}}]	71	1
377	2018-01-06 23:25:58.653834+00	15	15	2	[{"changed": {"fields": ["time_minutes"]}}]	71	1
378	2018-01-06 23:27:24.967397+00	8	Adam Zbadam - BASE	1	[{"added": {}}]	64	1
379	2018-01-06 23:27:33.554587+00	9	Krzysztof Poczdamski - BASE	1	[{"added": {}}]	64	1
380	2018-01-06 23:27:40.085113+00	10	Adam Adamski - BASE	1	[{"added": {}}]	64	1
381	2018-01-06 23:27:46.844268+00	11	Barbara Basienko - BASE	1	[{"added": {}}]	64	1
382	2018-01-06 23:27:51.844065+00	12	Cezary Cezary - BASE	1	[{"added": {}}]	64	1
383	2018-01-06 23:27:57.669184+00	13	Adam Mózg - BASE	1	[{"added": {}}]	64	1
384	2018-01-06 23:28:03.639142+00	14	Krzyś S - BASE	1	[{"added": {}}]	64	1
385	2018-01-06 23:28:09.263026+00	15	Bizon Szybki - BASE	1	[{"added": {}}]	64	1
386	2018-01-06 23:28:15.870832+00	16	Bizon Szybki - BASE	1	[{"added": {}}]	64	1
387	2018-01-06 23:28:24.083531+00	17	Mariusz Dawidowski - BASE	1	[{"added": {}}]	64	1
388	2018-01-06 23:28:29.553029+00	18	Tom Pac - BASE	1	[{"added": {}}]	64	1
389	2018-01-06 23:28:39.216144+00	19	Hardy Burn - BASE	1	[{"added": {}}]	64	1
390	2018-01-06 23:28:48.774841+00	20	Milo Corty - BASE	1	[{"added": {}}]	64	1
391	2018-01-06 23:29:24.967309+00	15	15	2	[{"changed": {"fields": ["machine_type", "location_type"]}}]	71	1
392	2018-01-07 12:59:30.762364+00	28	Marian Kowalski None	2	[{"changed": {"fields": ["client_user_login"]}}]	63	2
393	2018-01-09 17:02:56.014445+00	MECH	MECH	1	[{"added": {}}]	80	4
394	2018-01-09 17:03:25.396524+00	DIAG	DIAG	2	[{"changed": {"fields": ["id_se_group_dict", "se_group_dict_name"]}}]	80	4
395	2018-01-09 17:03:41.029306+00	OKR	OKR	2	[{"changed": {"fields": ["id_se_group_dict", "se_group_dict_name"]}}]	80	4
396	2018-01-09 17:04:03.720493+00	WUL	WUL	2	[{"changed": {"fields": ["id_se_group_dict", "se_group_dict_name"]}}]	80	4
397	2018-01-09 17:04:21.848286+00	BLA	BLA	2	[{"changed": {"fields": ["id_se_group_dict", "se_group_dict_name"]}}]	80	4
398	2018-01-09 17:11:45.723205+00	SAMOCHOD	SAMOCHOD	2	[]	80	4
399	2018-01-09 17:14:34.998108+00	15	Mateusz Z	1	[{"added": {}}]	61	4
400	2018-01-09 17:15:52.156968+00	15	Mateusz Z	2	[]	61	4
401	2018-01-09 17:16:30.016989+00	15	Mateusz Z	2	[]	61	4
402	2018-01-09 17:19:37.89085+00	WySkBi	WySkBi	2	[{"changed": {"fields": ["se_group", "base_price"]}}]	78	4
403	2018-01-09 17:20:34.225999+00	WOPO	WOPO	2	[{"changed": {"fields": ["se_group", "base_price", "notes"]}}]	78	4
404	2018-01-09 17:21:51.178131+00	WYMOL	WYMOL	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "base_price", "notes"]}}]	78	4
405	2018-01-09 17:22:33.815241+00	POP	POP	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "base_price", "notes"]}}]	78	4
406	2018-01-09 17:23:19.079992+00	WYMOK	WYMOK	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "base_price", "notes"]}}]	78	4
407	2018-01-09 17:24:12.546274+00	MALEL	MALEL	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "base_price", "notes"]}}]	78	4
408	2018-01-09 17:24:59.932794+00	PRA	PRA	2	[{"changed": {"fields": ["id_se_dict", "se_dict_name", "se_group", "base_price", "notes"]}}]	78	4
409	2018-01-09 19:14:17.795996+00	2	1 Mateusz Z	1	[{"added": {}}]	83	4
410	2018-01-09 21:56:25.609856+00	28	Marian Kowalski None	2	[{"changed": {"fields": ["client_user_login"]}}]	63	2
411	2018-01-09 23:21:29.71155+00	main	main	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	2
412	2018-01-09 23:21:47.709376+00	1	1	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	2
413	2018-01-09 23:22:36.836692+00	2	2	2	[{"changed": {"fields": ["longitude", "latitude"]}}]	13	2
414	2018-01-10 06:52:52.834483+00	16	Mateusz Zet	1	[{"added": {}}]	61	4
415	2018-01-10 07:05:41.010485+00	16	Mateusz Zet	2	[]	61	4
416	2018-01-10 07:55:40.52442+00	31	matiz	1	[{"added": {}}]	4	4
417	2018-01-10 07:55:47.646252+00	17	Mati Z	1	[{"added": {}}]	61	4
418	2018-01-10 10:07:03.745736+00	21	Mati Z - WYOPO	1	[{"added": {}}]	64	4
419	2018-01-10 10:07:20.759287+00	21	Mati Z - WYOPO	3		64	4
420	2018-01-10 11:47:27.258083+00	17	Mati Z	2	[]	61	4
421	2018-01-10 13:38:49.543459+00	1	main	2	[{"changed": {"fields": ["max_debt", "max_worktime_wo_conf_minutes"]}}]	65	2
422	2018-01-10 14:35:36.804004+00	1	main	2	[{"changed": {"fields": ["max_debt", "max_worktime_wo_conf_minutes"]}}]	65	2
423	2018-01-10 14:37:58.522972+00	1	main	2	[{"changed": {"fields": ["max_debt"]}}]	65	2
424	2018-01-12 20:42:49.252096+00	KoUkEl	KoUkEl	2	[{"changed": {"fields": ["se_group"]}}]	78	7
425	2018-01-12 20:42:44.320419+00	Badham	Badham	2	[{"changed": {"fields": ["se_group"]}}]	78	1
426	2018-01-12 20:44:21.333047+00	GLAPIE	GLAPIE	2	[{"changed": {"fields": ["notes"]}}]	78	7
427	2018-01-12 21:36:05.634934+00	3	Bizon Szybki	2	[{"changed": {"fields": ["user_login"]}}]	61	2
428	2018-01-12 21:51:22.483962+00	34	pracownik	1	[{"added": {}}]	4	1
429	2018-01-12 21:52:11.978632+00	3	Bizon Szybki	2	[{"changed": {"fields": ["user_login"]}}]	61	1
430	2018-01-12 21:53:55.353196+00	34	pracownik	2	[{"changed": {"fields": ["groups"]}}]	4	1
431	2018-01-12 22:17:58.635591+00	28	Marian Kowalski None	2	[{"changed": {"fields": ["client_user_login"]}}]	63	2
432	2018-04-06 11:49:37.95428+00	8	Marenty XXXXX None	2	[{"changed": {"fields": ["client_user_login"]}}]	63	2
433	2018-04-06 11:50:14.522449+00	1	Krzysztof Poczdamski	2	[{"changed": {"fields": ["user_login"]}}]	61	2
\.


--
-- Data for Name: django_content_type; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY django_content_type (id, app_label, model) FROM stdin;
1	admin	logentry
2	auth	permission
3	auth	group
4	auth	user
5	contenttypes	contenttype
6	sessions	session
7	company	sexdict
8	company	worker
9	company	contact
10	company	contacttype
11	company	countrydict
12	company	client
13	company	companybranch
14	company	workdaycalendar
15	company	service
16	company	locationtype
17	company	location
18	company	machine
19	company	machinetype
20	company	resourcesusageparams
21	company	workdaycalendarparams
22	company	clparams
23	company	currrency
24	company	timeslotparams
25	company	sedict
26	company	woabilitydict
27	company	woability
28	company	serequirement
29	company	clpaymentline
30	company	clpayment
31	company	address
32	company	clblockedreasondict
33	company	clcommunicationlog
34	company	clcommunicationreason
35	company	cldiscount
36	company	clunconfirmed
37	company	discountdict
38	company	discountscope
39	company	djangomigrations
40	company	resourcesusage
41	company	sediscount
42	company	servicearchived
43	company	timeslotlist
44	company	woabilitygroupdict
45	company	woabsence
46	company	woabsencetype
47	company	wogroup
48	company	wogroupdict
49	company	wogroupprivilege
50	company	wonotification
51	company	woprivilegedict
52	company	woprivilegeleveldict
53	company	wouser
54	company	zipcodesdict
55	machines	machine
56	machines	machinetype
57	company	companydescription
58	services	service
59	utilities	address
60	utilities	resourcesusage
61	workers	worker
62	utilities	workdaycalendar
63	clients	client
64	workers	woability
65	clients	clparams
66	clients	clblockedreasondict
67	clients	clcommunicationreason
68	clients	cldiscount
69	clients	discountdict
70	clients	discountscope
71	services	serequirement
72	services	sediscount
73	utilities	workdaycalendarparams
74	utilities	resourcesusageparams
75	utilities	countrydict
76	utilities	contacttype
77	utilities	currrency
78	services	sedict
79	utilities	sexdict
80	services	segroupdict
81	workers	wonotification
82	workers	woabilitydict
83	workers	wogroup
84	workers	wogroupdict
85	clients	clpayment
86	clients	clpaymentline
\.


--
-- Data for Name: django_migrations; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY django_migrations (id, app, name, applied) FROM stdin;
1	contenttypes	0001_initial	2017-11-05 11:22:16.100348+00
2	auth	0001_initial	2017-11-05 11:22:18.062864+00
3	admin	0001_initial	2017-11-05 11:22:18.662893+00
4	admin	0002_logentry_remove_auto_add	2017-11-05 11:22:19.086283+00
5	contenttypes	0002_remove_content_type_name	2017-11-05 11:22:19.610602+00
6	auth	0002_alter_permission_name_max_length	2017-11-05 11:22:19.978972+00
7	auth	0003_alter_user_email_max_length	2017-11-05 11:22:20.402152+00
8	auth	0004_alter_user_username_opts	2017-11-05 11:22:20.681674+00
9	auth	0005_alter_user_last_login_null	2017-11-05 11:22:21.069067+00
10	auth	0006_require_contenttypes_0002	2017-11-05 11:22:21.278467+00
11	auth	0007_alter_validators_add_error_messages	2017-11-05 11:22:21.545211+00
12	auth	0008_alter_user_username_max_length	2017-11-05 11:22:21.958688+00
13	sessions	0001_initial	2017-11-05 11:22:22.449166+00
14	company	0001_initial	2017-11-15 09:35:14.117826+00
15	machines	0001_initial	2017-11-15 12:47:06.045155+00
16	utilities	0001_initial	2017-11-16 12:11:31.692519+00
\.


--
-- Data for Name: django_session; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY django_session (session_key, session_data, expire_date) FROM stdin;
f4o7dz2bwe8flboab853bjfdy57v9om5	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2017-12-09 10:57:36.334012+00
y1vssl2wb3y3o3jzddcqi0ea2b50vc1f	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-11-19 21:06:00.487948+00
ng4lb27czn9igolk9rshybgzla17e1gg	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-06 16:30:09.021994+00
zrauc6slelqadj95e3i45o9hugepgfku	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-09 13:23:56.591963+00
zovze2b8or8nc8xcz20oe4121q4cdx3l	MTAwZTRiNjRjNjU1MjQ4ZDg0MjczYTcxNGJmY2YxMjJhZjdkNjM2YTp7Il9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIiwiX2F1dGhfdXNlcl9pZCI6IjEiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-06 18:33:54.775807+00
nheg46t2huep3jzh95ut95ja9ooz0xlg	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-06 18:42:15.953143+00
4d6isd8l9r79fqvzk1kfk3ywzvro1dtw	Yjk5NzZiNWI1MmY3MTQ1MWM4MDExMzI4NzlkN2ZjZjg3MTgxM2EyNjp7Il9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIiwiX2F1dGhfdXNlcl9oYXNoIjoiZWI4YWZkMzVjNjIyZjNjNDcyMjViNmYxODA4ZjM5NjE4NmEzMGRmZSIsIl9hdXRoX3VzZXJfaWQiOiIxIn0=	2017-12-22 20:08:26.705998+00
hmhkrsjf8j9c8jdl9xjta67u6gcl392y	YjdhMmM2YTgxNGFkMzViMDZkOGY1MDUzYmQ0MjUwMjBlYzI3OWE4Mjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9oYXNoIjoiZWI4YWZkMzVjNjIyZjNjNDcyMjViNmYxODA4ZjM5NjE4NmEzMGRmZSIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIn0=	2017-11-30 20:26:19.917289+00
gj2s271w15djcx744t6hab8ouez5nmmx	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-06-08 07:31:03.034038+00
6txy6vt1mum7b2hm8z6yxs7c10sp981n	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2017-12-23 10:34:23.87714+00
8aeepxj1ekc0o0b35uaeays0g8uwbaze	Yjk5NzZiNWI1MmY3MTQ1MWM4MDExMzI4NzlkN2ZjZjg3MTgxM2EyNjp7Il9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIiwiX2F1dGhfdXNlcl9oYXNoIjoiZWI4YWZkMzVjNjIyZjNjNDcyMjViNmYxODA4ZjM5NjE4NmEzMGRmZSIsIl9hdXRoX3VzZXJfaWQiOiIxIn0=	2017-12-24 19:07:32.585179+00
om2mqe0hvg7kudw5ui1rek7owujtbud3	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-27 11:21:17.917027+00
omglaf17bygvwcs27mmta1uxiq27on5j	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-27 11:50:30.483647+00
f9n2fcqizuksuoy02m4bpwg9l3aqytzk	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-27 14:23:17.247592+00
1hqnr3awvcvevktgxs98upthvit65xbk	ZTViZDkzMTc1NWY2YWMyMDQ0MjE4MDI0OWVmYzg3YmMyMjRkN2E4YTp7fQ==	2017-12-22 16:04:40.484605+00
4vn8nbbqeu5sozisq2ozekk7q3dksie6	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-02 10:28:48.923415+00
mv8olb8zf4b532hj5aj4p0icxp578yuc	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2017-12-27 20:04:11.021601+00
q4s0lta5bkw3oyugon02wprdjzai0w2y	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-02 10:43:19.812533+00
u62k3oy2s0qgk2s1w7hfxhp6x4lbd408	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-03 15:29:59.038409+00
cxc4rvpm69zn20j2e1y6meerezhzflsx	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2017-12-27 20:35:01.491789+00
9u2ozjl46ykfuix295zw7cdvhle4u58l	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2017-12-28 23:12:37.426216+00
h6qc0oq0la7nwhsrpzsfzgqoln2yvb5k	ZTViZDkzMTc1NWY2YWMyMDQ0MjE4MDI0OWVmYzg3YmMyMjRkN2E4YTp7fQ==	2017-12-06 11:10:47.401903+00
dryn3qq5mw0oagvvrik2u7uegjqwrhvs	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2017-12-30 10:42:14.323054+00
deez357r824l3on2ym7ti7gha3h14u94	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2017-12-08 13:38:30.841291+00
ibja5ylsw7sg91ulv735xagotgiwr9jj	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-01-16 18:57:54.021791+00
vrjvmqj3v8a6h8j58y800amz36rsjqbq	MTZlOWQyMWFjODgxOTJmOTUxZWVhZWFlYTYwZjNlZmE2NjA5MDU1YTp7Il9hdXRoX3VzZXJfaWQiOiIyOSIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIiwiX2F1dGhfdXNlcl9oYXNoIjoiNGYzZjg1Y2ExMDI2MTcyMDRhNzM0ZjEzNjJkZWRmN2FhZmQxNTllNSJ9	2018-01-24 06:39:31.921652+00
1q8mhnppxaft3r824358b7osrc7t4g9v	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2018-02-06 21:16:12.969818+00
fdcajm01w1qu1z2h2g0opp2xwx4dj981	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-01-22 14:23:23.36322+00
7dxm8azqsbltwb9fvrbim9ceu3q61b1f	MjM1YzI3MzhhMGYzOGJmMDQwY2ZkZWFlNTc3YzY4MjQ1NmFiNmZjODp7Il9hdXRoX3VzZXJfaWQiOiIyOCIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImRqYW5nby5jb250cmliLmF1dGguYmFja2VuZHMuTW9kZWxCYWNrZW5kIiwiX2F1dGhfdXNlcl9oYXNoIjoiZGFkOTVkZDBiNDRmMzFlZDJhYTRjMDc4NmRjNmMzMDIzNDExODFmOSJ9	2018-01-20 14:42:53.185153+00
vslvwp6r77wk2w3q9u84t31md3avwqja	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-01-20 10:19:38.128803+00
0jty0wq11xv7qgalpqexg1cuf7lhzrr6	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2018-01-24 13:00:46.350156+00
ptpl8sk1t2ne7nbz4f2e2e02mjvn8fip	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-01-16 20:40:55.125527+00
oqwmbjzi45cu8ur8cunk6lpwe62n3lv7	NzEwM2I1Y2M0NjU3Zjk2OTQ5ZmUzMjFhMDRhZjcwMGRkOTY3OTk1Njp7Il9hdXRoX3VzZXJfaWQiOiI1IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxOWEyZjA1ZGM2MjlkNTFmY2MxZmI3OGFjYWVlOTczZjAzM2RkMjFkIn0=	2018-02-09 20:13:18.425822+00
q8ibz77d9adxn7292t6csvlr52fic99f	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-01-24 13:30:41.882566+00
ci3mcapla23431ite3jp3w8loj29sljp	NzMzNTg2Njg0MWU5Y2FmOTg5OTcxY2ZkZjM0M2Q0Y2IwNGY5NDBkYjp7Il9hdXRoX3VzZXJfaGFzaCI6ImViOGFmZDM1YzYyMmYzYzQ3MjI1YjZmMTgwOGYzOTYxODZhMzBkZmUiLCJfYXV0aF91c2VyX2JhY2tlbmQiOiJkamFuZ28uY29udHJpYi5hdXRoLmJhY2tlbmRzLk1vZGVsQmFja2VuZCIsIl9hdXRoX3VzZXJfaWQiOiIxIn0=	2018-01-19 21:18:33.93242+00
z157qkojjwo9jrwvpj0agxplcy2nkqqr	MWJhMTNiZDgxYjFjY2JhOThhOTFlY2RmY2YwZTFlNzM0NDcxZjc3Nzp7Il9hdXRoX3VzZXJfaWQiOiI0IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJhY2E5MzczMDVlNmU2ZDllMjE0MTAyZGU2NjFjNTczZmEzZjg4ODgwIn0=	2018-01-20 16:01:27.908505+00
roroerxdcw0tf5i7iow7otas48dk2979	NDFiY2VjYzlkZjQzMmMzZTUyZjU2NTQwYjRkYmJmOTRkZTRhMTdlZjp7Il9hdXRoX3VzZXJfaWQiOiIxIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiJlYjhhZmQzNWM2MjJmM2M0NzIyNWI2ZjE4MDhmMzk2MTg2YTMwZGZlIn0=	2018-01-18 13:23:25.964717+00
t7e2tya63yqk989iqkspeegeq40ms8hx	Y2M5ZDU0YTdjYzJkZmVhZTMwODlhOTA2MjJjOGE3OWFjM2NkZjUyMjp7Il9hdXRoX3VzZXJfaWQiOiI3IiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiI1YTgxYmJkZDQyZDRlZDVmMGJjNDZhMjMzYjllMWFkMGVlNmY5MDRhIn0=	2018-01-10 14:58:50.330493+00
0yarfqt1l0e3cy3joucwh9zaldcc8iqh	YWM3MWE2Y2NkYjJjNzFhN2JmMDk0MGFlODM1Yzg0M2MxZTAzZjgxMTp7Il9hdXRoX3VzZXJfaWQiOiIyIiwiX2F1dGhfdXNlcl9iYWNrZW5kIjoiZGphbmdvLmNvbnRyaWIuYXV0aC5iYWNrZW5kcy5Nb2RlbEJhY2tlbmQiLCJfYXV0aF91c2VyX2hhc2giOiIxYjFkOWZmMGM3Mzk3YmMwNDk3ZWRhZmIzZmI2ZDBjZDhmMTZhMzc4In0=	2018-04-20 11:48:45.536985+00
\.


--
-- Data for Name: location; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY location (id_location, location_name, location_type, is_operational, notes, company_branch) FROM stdin;
1	PODN1	PODN	t	\N	main
2	PODN2	PODN	t	\N	main
3	Kanał	PODN	t	\N	main
4	Stanowisko badania hamulców	StBadHam	t	\N	main
5	Tester układów elektronicznych	KoUkEl	t	\N	main
6	Podnośnik kolumnowy	PodKol	t	\N	main
8	Diagnostyka układu kierowniczego	StKoUsKo	t	\N	main
7	Bazowanie swiateł	StUsSw	t	\N	main
9	Analiza spalin	AnaSpa	t	\N	main
\.


--
-- Data for Name: location_type; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY location_type (id_location_type, location_type_name, location_capacity) FROM stdin;
PODN	Podnośnik duży	1
PODNMA	Podnośnik mały	1
StBadHam	Stanowisko badania hamulców	1
KoUkEl	Kontrola układów elektronicznych	1
PodKol	Podnośnik Kolumnowy	1
StUsSw	Stanowisko do ustawiania świateł	1
StKoUsKo	Stanowisko kontroli ustawienia kół	1
AnaSpa	Analizator spalin	1
\.


--
-- Data for Name: machine; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY machine (id_machine, machine_name, machine_type, service_interval, last_service, is_operational, notes, company_branch) FROM stdin;
3	Stara	KON	100	2017-11-12	t	\N	main
1	Bosh 1	WIER	302	2017-11-06	t	\N	main
2	Hiltii	WIER	401	2017-10-10	f	\N	main
24	ETT 855	AnSp	2000	2018-01-03	t	\N	main
18	EF14	WYKOL	365	2017-01-03	t	\N	main
9	DeWalt	WIER	9	2018-01-02	t	\N	main
25	bori	WIER	372	2017-01-03	t	\N	main
14	KTS 550	TEUKEL	2000	2018-01-03	t	\N	main
4	Makita	SZLI	10	2018-01-03	f	\N	main
8	Tez makita	SZLI	222	2018-01-03	t	\N	main
13	Myjka	MYJK	2000	2016-01-03	t	\N	main
20	Podniośnik kolumnowy nr1	PODN	2000	2016-01-06	t	\N	main
22	Podnośnik nr 3	PODN	2000	2017-07-08	t	\N	main
21	Podnośnik kolumnowy nr2	PODN	2000	2018-06-09	t	\N	main
6	spre1	SPRE	730	2017-11-12	f	\N	main
15	BSA 251	STBAHA	2000	2017-11-12	t	\N	main
19	HG 660	STKOUK	2000	2017-11-12	t	\N	main
7	spre2	SPRE	100	2017-11-12	t	\N	main
17	EBT	STBAHA	2000	2017-11-12	t	\N	main
16	FWT	STKOZA	2000	2017-11-12	t	\N	main
23	USP-20	STBASW	2000	2017-11-12	t	\N	main
5	Maszynka do mięsa	WIER	121	2017-11-10	t	\N	2
11	Weri	WIER	365	2017-01-03	t	\N	main
10	Knut	KON	222	2017-03-03	t	\N	main
\.


--
-- Data for Name: machine_type; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY machine_type (id_machine_type, machine_type_name) FROM stdin;
WIER	Wiertarka
SPRE	Sprężarka
SZLI	Szlifierka kątowa
KON	Koń
MYJK	Maszyny myjące
TEUKEL	Tester ukł. elektronicznych
STBAHA	Stanowisko do badania hamulcow
STKOZA	Stanowisko do kontroli zawieszenia
WYKOL	Wyważarka do kół
STKOUK	Stanowisko do kontroli ustawienia kół
PODN	Podnośniki
STBASW	Stanowisko do ustawiania swiateł
AnSp	Analizator spalin
MLOTUD	Młot udarowy
\.


--
-- Data for Name: resources_usage; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY resources_usage (id_resources_usage, service, machine, worker, time_slot, calendar_date, company_branch, start_timestamp, finish_timestamp, location) FROM stdin;
75	99	7	4	\N	2018-01-12	main	2018-01-12 10:30:00	2018-01-12 11:05:00	1
2	2	6	4	\N	\N	main	2017-12-01 05:00:00	2017-12-01 07:00:00	1
3	3	7	5	\N	\N	main	2017-12-01 06:00:00	2017-12-01 08:00:00	2
4	4	7	6	\N	\N	main	2017-12-01 10:00:00	2017-12-01 12:00:00	1
5	5	6	4	\N	\N	main	2017-12-01 11:00:00	2017-12-01 13:00:00	2
6	6	7	5	\N	\N	main	2017-12-01 12:00:00	2017-12-01 14:00:00	1
7	7	6	4	\N	\N	main	2017-12-01 17:00:00	2017-12-01 19:00:00	2
8	8	7	5	\N	\N	main	2017-12-01 19:00:00	2017-12-01 21:00:00	1
9	9	6	6	\N	\N	main	2017-12-01 20:00:00	2017-12-01 22:00:00	2
10	10	7	4	\N	\N	main	2017-12-01 21:00:00	2017-12-01 23:00:00	1
13	\N	\N	\N	\N	\N	main	2017-12-25 07:00:00	2017-12-25 07:30:00	\N
39	63	6	3	\N	2018-01-03	main	2018-01-03 08:00:00	2018-01-03 08:30:00	1
40	64	7	4	\N	2018-01-01	main	2018-01-01 13:30:00	2018-01-01 14:05:00	1
41	65	7	4	\N	2018-01-03	main	2018-01-03 09:00:00	2018-01-03 09:35:00	1
42	66	7	3	\N	2018-01-03	main	2018-01-03 08:30:00	2018-01-03 09:00:00	1
44	68	7	4	\N	2018-01-05	main	2018-01-05 11:30:00	2018-01-05 12:05:00	1
45	69	7	3	\N	2018-01-04	main	2018-01-04 11:30:00	2018-01-04 12:00:00	1
47	71	7	3	\N	2018-01-04	main	2018-01-04 09:00:00	2018-01-04 09:30:00	1
48	72	7	4	\N	2018-01-03	main	2018-01-03 12:30:00	2018-01-03 13:05:00	1
53	77	2	1	\N	2018-01-05	main	2018-01-05 11:30:00	2018-01-05 12:00:00	7
54	78	7	3	\N	2018-01-05	main	2018-01-05 13:30:00	2018-01-05 14:00:00	1
55	79	7	4	\N	2018-01-08	main	2018-01-08 07:00:00	2018-01-08 07:35:00	1
58	82	7	4	\N	2018-01-09	main	2018-01-09 08:30:00	2018-01-09 09:05:00	1
59	83	7	4	\N	2018-01-09	main	2018-01-09 09:30:00	2018-01-09 10:05:00	1
60	84	7	3	\N	2018-01-10	main	2018-01-10 10:30:00	2018-01-10 11:00:00	1
61	85	1	1	\N	2018-01-15	main	2018-01-15 09:30:00	2018-01-15 09:50:00	4
62	86	7	3	\N	2018-01-16	main	2018-01-16 07:30:00	2018-01-16 08:00:00	1
63	87	1	1	\N	2018-01-10	main	2018-01-10 07:00:00	2018-01-10 07:30:00	5
65	89	7	4	\N	2018-01-10	main	2018-01-10 08:30:00	2018-01-10 09:05:00	1
66	90	1	1	\N	2018-01-09	main	2018-01-09 11:30:00	2018-01-09 11:32:00	9
67	91	2	1	\N	2018-01-11	main	2018-01-11 07:00:00	2018-01-11 07:30:00	7
68	92	1	1	\N	2018-01-12	main	2018-01-12 10:00:00	2018-01-12 10:02:00	9
70	94	1	1	\N	2018-01-10	main	2018-01-10 07:30:00	2018-01-10 08:00:00	8
71	95	2	1	\N	2018-01-12	main	2018-01-12 08:30:00	2018-01-12 09:00:00	7
73	97	2	1	\N	2018-01-10	main	2018-01-10 07:00:00	2018-01-10 07:30:00	9
74	98	1	1	\N	2018-01-11	main	2018-01-11 08:00:00	2018-01-11 08:30:00	8
77	101	1	1	\N	2018-01-12	main	2018-01-12 07:00:00	2018-01-12 07:22:00	1
78	102	8	3	\N	2018-01-11	main	2018-01-11 09:30:00	2018-01-11 09:40:00	1
79	103	1	1	\N	2018-01-09	main	2018-01-09 09:00:00	2018-01-09 09:30:00	9
80	104	1	1	\N	2018-01-11	main	2018-01-11 09:00:00	2018-01-11 09:22:00	1
81	105	8	3	\N	2018-01-09	main	2018-01-09 08:00:00	2018-01-09 08:10:00	1
82	106	1	1	\N	2018-01-10	main	2018-01-10 09:00:00	2018-01-10 09:22:00	2
83	107	2	1	\N	2018-01-09	main	2018-01-09 08:00:00	2018-01-09 08:30:00	7
84	108	1	1	\N	2018-01-08	main	2018-01-08 08:30:00	2018-01-08 09:00:00	9
85	109	1	1	\N	2018-01-11	main	2018-01-11 08:30:00	2018-01-11 08:52:00	1
86	110	1	1	\N	2018-01-11	main	2018-01-11 08:30:00	2018-01-11 08:52:00	2
87	111	1	1	\N	2018-01-11	main	2018-01-11 08:30:00	2018-01-11 08:52:00	3
88	112	1	1	\N	2018-01-09	main	2018-01-09 08:30:00	2018-01-09 08:52:00	2
89	113	1	1	\N	2018-01-08	main	2018-01-08 07:00:00	2018-01-08 07:22:00	2
90	114	1	1	\N	2018-01-08	main	2018-01-08 07:30:00	2018-01-08 08:00:00	5
91	115	7	3	\N	2018-01-12	main	2018-01-12 07:30:00	2018-01-12 08:00:00	1
92	121	2	1	\N	2018-01-10	main	2018-01-10 07:00:00	2018-01-10 07:30:00	7
93	122	1	1	\N	2018-01-10	main	2018-01-10 07:00:00	2018-01-10 07:22:00	1
94	123	2	1	\N	2018-01-10	main	2018-01-10 07:30:00	2018-01-10 08:00:00	7
95	124	1	1	\N	2018-01-09	main	2018-01-09 07:00:00	2018-01-09 07:22:00	1
96	136	\N	1	\N	2018-01-11	main	2018-01-11 07:00:00	2018-01-11 07:33:00	\N
97	137	\N	1	\N	2018-01-09	main	2018-01-09 08:30:00	2018-01-09 09:03:00	\N
98	138	\N	1	\N	2018-01-08	main	2018-01-08 08:30:00	2018-01-08 09:03:00	\N
99	139	\N	1	\N	2018-01-10	main	2018-01-10 08:00:00	2018-01-10 08:33:00	\N
100	141	\N	1	\N	2018-01-10	main	2018-01-10 07:30:00	2018-01-10 08:03:00	\N
102	143	1	1	\N	2018-01-11	main	2018-01-11 07:00:00	2018-01-11 07:30:00	9
103	144	7	3	\N	2018-01-11	main	2018-01-11 07:30:00	2018-01-11 08:00:00	1
104	145	1	1	\N	2018-01-11	main	2018-01-11 08:30:00	2018-01-11 09:00:00	9
105	146	2	1	\N	2018-01-11	main	2018-01-11 07:30:00	2018-01-11 08:30:00	6
106	147	2	1	\N	2018-01-12	main	2018-01-12 09:00:00	2018-01-12 10:00:00	6
107	148	1	1	\N	2018-01-10	main	2018-01-10 11:00:00	2018-01-10 11:30:00	9
108	149	1	1	\N	2018-01-10	main	2018-01-10 07:30:00	2018-01-10 08:00:00	9
109	150	1	1	\N	2018-01-12	main	2018-01-12 08:30:00	2018-01-12 09:00:00	9
110	151	1	1	\N	2018-01-10	main	2018-01-10 09:30:00	2018-01-10 10:00:00	9
111	152	1	1	\N	2018-01-12	main	2018-01-12 07:30:00	2018-01-12 08:00:00	9
113	154	2	1	\N	2018-01-11	main	2018-01-11 12:00:00	2018-01-11 13:00:00	6
114	155	2	1	\N	2018-01-12	main	2018-01-12 11:00:00	2018-01-12 12:00:00	6
115	156	1	1	\N	2018-01-11	main	2018-01-11 10:00:00	2018-01-11 10:30:00	9
116	157	2	1	\N	2018-01-11	main	2018-01-11 09:00:00	2018-01-11 10:00:00	6
117	158	2	1	\N	2018-01-18	main	2018-01-18 09:00:00	2018-01-18 10:00:00	6
118	159	2	1	\N	2018-01-11	main	2018-01-11 10:30:00	2018-01-11 11:30:00	6
119	160	2	1	\N	2018-01-12	main	2018-01-12 07:30:00	2018-01-12 08:30:00	6
120	161	1	1	\N	2018-01-11	main	2018-01-11 13:00:00	2018-01-11 13:30:00	9
121	162	7	4	\N	2018-01-12	main	2018-01-12 09:00:00	2018-01-12 09:34:00	1
122	163	2	1	\N	2018-01-16	main	2018-01-16 10:30:00	2018-01-16 11:30:00	6
123	164	1	1	\N	2018-01-18	main	2018-01-18 09:00:00	2018-01-18 09:30:00	9
124	165	1	1	\N	2018-01-12	main	2018-01-12 11:00:00	2018-01-12 11:30:00	9
125	166	7	4	\N	2018-01-17	main	2018-01-17 09:30:00	2018-01-17 10:04:00	1
126	167	1	1	\N	2018-01-11	main	2018-01-11 10:30:00	2018-01-11 11:00:00	9
127	168	2	1	\N	2018-01-12	main	2018-01-12 13:30:00	2018-01-12 14:30:00	6
128	169	1	1	\N	2018-01-11	main	2018-01-11 09:30:00	2018-01-11 10:00:00	9
129	170	1	1	\N	2018-01-12	main	2018-01-12 12:30:00	2018-01-12 13:00:00	9
130	171	2	1	\N	2018-01-16	main	2018-01-16 08:30:00	2018-01-16 09:30:00	6
131	172	1	1	\N	2018-01-11	main	2018-01-11 12:00:00	2018-01-11 12:30:00	9
132	173	7	3	\N	2018-01-18	main	2018-01-18 11:30:00	2018-01-18 12:00:00	1
133	174	7	4	\N	2018-01-15	main	2018-01-15 12:30:00	2018-01-15 13:04:00	1
134	175	7	4	\N	2018-01-25	main	2018-01-25 11:30:00	2018-01-25 12:04:00	1
135	176	7	3	\N	2018-01-15	main	2018-01-15 07:30:00	2018-01-15 08:00:00	1
136	177	2	1	\N	2018-01-15	main	2018-01-15 07:30:00	2018-01-15 08:30:00	6
137	178	7	3	\N	2018-01-19	main	2018-01-19 08:30:00	2018-01-19 09:00:00	1
138	179	1	1	\N	2018-01-19	main	2018-01-19 08:30:00	2018-01-19 09:00:00	5
139	180	1	1	\N	2018-01-16	main	2018-01-16 07:30:00	2018-01-16 08:00:00	9
140	181	2	1	\N	2018-01-19	main	2018-01-19 11:30:00	2018-01-19 14:50:00	4
141	182	\N	1	\N	2018-01-18	main	2018-01-18 09:00:00	2018-01-18 09:33:00	\N
142	183	7	3	\N	2018-01-19	main	2018-01-19 11:30:00	2018-01-19 12:00:00	1
143	184	2	1	\N	2018-01-18	main	2018-01-18 10:30:00	2018-01-18 11:30:00	6
144	185	2	1	\N	2018-01-15	main	2018-01-15 09:30:00	2018-01-15 10:30:00	6
145	186	1	1	\N	2018-01-15	main	2018-01-15 11:30:00	2018-01-15 12:00:00	9
146	187	1	1	\N	2018-01-17	main	2018-01-17 08:00:00	2018-01-17 08:30:00	5
147	188	2	1	\N	2018-01-19	main	2018-01-19 12:00:00	2018-01-19 13:00:00	6
148	189	2	1	\N	2018-01-17	main	2018-01-17 09:00:00	2018-01-17 10:00:00	6
149	190	1	1	\N	2018-04-10	main	2018-04-10 10:00:00	2018-04-10 10:30:00	5
150	191	1	1	\N	2018-04-11	main	2018-04-11 07:00:00	2018-04-11 07:22:00	1
151	192	1	1	\N	2018-04-11	main	2018-04-11 07:00:00	2018-04-11 07:22:00	2
152	193	7	4	\N	2018-04-11	main	2018-04-11 10:00:00	2018-04-11 10:34:00	1
153	194	1	1	\N	2018-04-12	main	2018-04-12 10:00:00	2018-04-12 10:30:00	5
154	195	1	1	\N	2018-04-12	main	2018-04-12 08:30:00	2018-04-12 08:50:00	4
155	196	2	1	\N	2018-04-11	main	2018-04-11 09:00:00	2018-04-11 10:00:00	6
156	197	2	1	\N	2018-04-13	main	2018-04-13 11:00:00	2018-04-13 12:00:00	6
157	198	2	1	\N	2018-04-09	main	2018-04-09 12:00:00	2018-04-09 13:00:00	6
158	199	2	1	\N	2018-04-09	main	2018-04-09 07:30:00	2018-04-09 08:30:00	6
\.


--
-- Data for Name: resources_usage_params; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY resources_usage_params (allow_using_machine_without_service_days_before_date, resources_usage_params_id) FROM stdin;
100	1
\.


--
-- Data for Name: se_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY se_dict (id_se_dict, se_dict_name, base_price, location_type, avg_time, continous, notes, se_group) FROM stdin;
mycok	Mycie okien	22.00	PODNMA	\N	t	Mogą zdarzyć się cuda.	INNE
PUCBUT	Pucowanie butów	1.00	PODN	2	t	Własna pasta. Receptura tajna.	INNE
WySkBi	Wymiana skrzyni biegów	1000.00	PodKol	60	f	Szybka i fachowa	MECH
WOPO	Wymiana opon	50.00	PODNMA	35	f	Wymiana przy użyciu najnowszego sprzętu.	WUL
WYMOL	Wymiana oleju i filtrów	200.00	PODN	\N	t	Ulga dla silnika	OKR
POP	Poprawki lakiernicze	250.00	PODN	\N	t	Cena za element	BLA
WYMOK	Wymiana szyby przedniej	50.00	PODNMA	30	f	Zestaw uszczelek gratis	BLA
MALEL	Malowanie elementu	400.00	PODN	2	t	Świeże farby	BLA
PRA	Pranie tapicerki	120.00	PODNMA	30	f	Cudowna chemia prosto z Niemiec	INNE
KoUkEl	Kontrola układów elektronicznych	100.00	KoUkEl	30	t	Diagnostyka na najlepszym sprzęcie	DIAG
Badham	Diagnostyka układu hamulcowego	100.00	StBadHam	20	f	Skracamy długość hamowania o 75%.	DIAG
GLAPIE	Gładzenie wymion	0.00	\N	\N	t	Czule i delikatnie.	ZWIERZ
strps	Strzyżenie psiura	2.00	PODNMA	30	f	Strzyżemy psy - tanio, szybko, gładko. Okazja!	ZWIERZ
czzeb	Czyszczenie zębów konia	2.00	PODN	\N	t	Pucu, pucu i bambucu	ZWIERZ
BazSwi	Bazowanie świateł	150.00	StUsSw	30	t	Aby swieciły prosto. Bardzo prosto.	SAMOCHOD
sss	Wysysanie oleju z silnika metodą u-u	32.00	PODN	\N	t	Super sprawa!	SAMOCHOD
DiUkKi	Diagnostyka układu kierowniczego	200.00	StKoUsKo	30	t	Sprawdź czy nie skręcasz	SAMOCHOD
AnaSpa	Analiza spalin	100.00	AnaSpa	30	t	Wdechem	SAMOCHOD
KLASK	Klaskanie	12.00	\N	\N	t	Klaszczemy na okazjach. Szybko, mocno i gdziekolwiek.	ZWIERZ
myok	Mycie okien na drugim piętrze	22.00	PODNMA	\N	t	Mamy swoje drabiny.	INNE
\.


--
-- Data for Name: se_discount; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY se_discount (id_se_discount, discount, service, company_branch) FROM stdin;
1	TANIEOPO	2	main
\.


--
-- Data for Name: se_group_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY se_group_dict (id_se_group_dict, se_group_dict_name) FROM stdin;
ZWIERZ	Usługi dla zwierząt
INNE	Usługi inne
MECH	Mechaniczne
DIAG	Diagnostyczne
OKR	Okresowe
WUL	Wulkanizacyjne
BLA	Blacharskie
SAMOCHOD	Usługi samochodowe
\.


--
-- Data for Name: se_requirement; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY se_requirement (id_se_requirement, service_code, machine_type, worker_ability, qty, activity_sort_order, activity_name, time_minutes, location_type) FROM stdin;
1	WOPO	SPRE	WYOPO	1	1	\N	34	PODN
2	strps	SPRE	FREFE	1	2	asdasd	30	PODN
3	Badham	STBAHA	DIAG	1	1	Diagnostyka układu hamulcowego	20	StBadHam
4	KoUkEl	TEUKEL	ELEKT	1	1	Kontrola układów elektronicznych	30	KoUkEl
5	WySkBi	PODN	MECH	1	1	Wymiana skrzyni biegów	60	PodKol
6	DiUkKi	STKOUK	DIAG	1	1	diagnostyka układu kierowniczego	30	StKoUsKo
7	BazSwi	STBASW	ELEKT	1	1	Bazowanie świateł	30	StUsSw
8	AnaSpa	AnSp	DIAG	1	1	\N	30	AnaSpa
9	czzeb	SZLI	FREFE	1	1	\N	10	PODN
10	sss	TEUKEL	DIAG	1	1	\N	22	PODN
11	mycok	MYJK	DIAG	1	1	\N	\N	AnaSpa
12	PUCBUT	STKOUK	MECH	1	1	\N	30	AnaSpa
13	KLASK	KON	MECH	1	1	\N	20	PODN
14	myok	PODN	ELEKT	1	1	\N	200	StBadHam
15	GLAPIE	\N	BASE	1	1	\N	33	\N
\.


--
-- Data for Name: service; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY service (id_service, is_confirmed, service_code, client, location, create_invoice, service_discount_amount, service_discount_percent, min_start_datetime, planned_start, planned_end, real_start, real_end, reminder_sms_minutes, reminder_email_minutes, finished_info_sms, finished_info_email, notes, created_datetime, created_by, confirmed_datetime, confirmed_by, company_branch) FROM stdin;
2	t	WOPO	1	1	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
3	t	WOPO	2	2	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
4	t	WOPO	2	1	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
5	t	WOPO	1	2	t	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
6	t	WOPO	1	1	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
7	t	WOPO	2	2	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
8	t	WOPO	1	1	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
9	t	WOPO	2	2	f	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
10	t	WOPO	1	1	t	\N	\N	\N	\N	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
63	f	strps	28	\N	f	\N	\N	\N	2018-01-03 08:00:00	2018-01-03 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
64	f	WOPO	28	\N	f	\N	\N	\N	2018-01-01 13:30:00	2018-01-01 14:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
65	f	WOPO	28	\N	f	\N	\N	\N	2018-01-03 09:00:00	2018-01-03 09:35:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
66	f	strps	28	\N	f	\N	\N	\N	2018-01-03 08:30:00	2018-01-03 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
68	f	WOPO	34	\N	f	\N	\N	\N	2018-01-05 11:30:00	2018-01-05 12:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
69	f	strps	33	\N	f	\N	\N	\N	2018-01-04 11:30:00	2018-01-04 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
71	f	strps	28	\N	f	\N	\N	\N	2018-01-04 09:00:00	2018-01-04 09:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
72	f	WOPO	28	\N	f	\N	\N	\N	2018-01-03 12:30:00	2018-01-03 13:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
77	f	BazSwi	28	\N	f	\N	\N	\N	2018-01-05 11:30:00	2018-01-05 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
78	f	strps	28	\N	f	\N	\N	\N	2018-01-05 13:30:00	2018-01-05 14:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
79	f	WOPO	28	\N	f	\N	\N	\N	2018-01-08 07:00:00	2018-01-08 07:35:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
82	f	WOPO	34	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
83	f	WOPO	34	\N	f	\N	\N	\N	2018-01-09 09:30:00	2018-01-09 10:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
84	f	strps	34	\N	f	\N	\N	\N	2018-01-10 10:30:00	2018-01-10 11:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
85	f	Badham	58	\N	f	\N	\N	\N	2018-01-15 09:30:00	2018-01-15 09:50:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
86	f	strps	58	\N	f	\N	\N	\N	2018-01-16 07:30:00	2018-01-16 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
87	f	KoUkEl	28	\N	f	\N	\N	\N	2018-01-10 07:00:00	2018-01-10 07:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
89	f	WOPO	58	\N	f	\N	\N	\N	2018-01-10 08:30:00	2018-01-10 09:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
90	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-09 11:30:00	2018-01-09 11:32:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
91	f	BazSwi	28	\N	f	\N	\N	\N	2018-01-11 07:00:00	2018-01-11 07:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
92	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-12 10:00:00	2018-01-12 10:02:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
93	f	sss	28	\N	f	\N	\N	\N	2018-01-11 08:30:00	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
94	f	DiUkKi	28	\N	f	\N	\N	\N	2018-01-10 07:30:00	2018-01-10 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
95	f	BazSwi	28	\N	f	\N	\N	\N	2018-01-12 08:30:00	2018-01-12 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
96	f	czzeb	28	\N	f	\N	\N	\N	2018-01-12 08:00:00	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
97	f	AnaSpa	28	\N	f	\N	\N	\N	2018-01-10 07:00:00	2018-01-10 07:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
98	f	DiUkKi	28	\N	f	\N	\N	\N	2018-01-11 08:00:00	2018-01-11 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
99	f	WOPO	28	\N	f	\N	\N	\N	2018-01-12 10:30:00	2018-01-12 11:05:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
101	f	sss	28	\N	f	\N	\N	\N	2018-01-12 07:00:00	2018-01-12 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
102	f	czzeb	28	\N	f	\N	\N	\N	2018-01-11 09:30:00	2018-01-11 09:40:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
103	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-09 09:00:00	2018-01-09 09:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
104	f	sss	28	\N	f	\N	\N	\N	2018-01-11 09:00:00	2018-01-11 09:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
105	f	czzeb	28	\N	t	\N	\N	\N	2018-01-09 08:00:00	2018-01-09 08:10:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
106	f	sss	28	\N	f	\N	\N	\N	2018-01-10 09:00:00	2018-01-10 09:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
107	f	BazSwi	28	\N	f	\N	\N	\N	2018-01-09 08:00:00	2018-01-09 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
108	f	PUCBUT	56	\N	f	\N	\N	\N	2018-01-08 08:30:00	2018-01-08 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
109	f	sss	56	\N	f	\N	\N	\N	2018-01-11 08:30:00	2018-01-11 08:52:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
110	f	sss	56	\N	f	\N	\N	\N	2018-01-11 08:30:00	2018-01-11 08:52:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
111	f	sss	56	\N	f	\N	\N	\N	2018-01-11 08:30:00	2018-01-11 08:52:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
112	f	sss	56	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 08:52:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
113	f	sss	6	\N	f	\N	\N	\N	2018-01-08 07:00:00	2018-01-08 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
114	f	KoUkEl	56	\N	f	\N	\N	\N	2018-01-08 07:30:00	2018-01-08 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
115	f	strps	58	\N	f	\N	\N	\N	2018-01-12 07:30:00	2018-01-12 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
116	f	GLAPIE	58	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
117	f	GLAPIE	58	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
118	f	GLAPIE	58	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
119	f	GLAPIE	58	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
120	f	GLAPIE	58	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
121	f	BazSwi	56	\N	f	\N	\N	\N	2018-01-10 07:00:00	2018-01-10 07:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
122	f	sss	56	\N	f	\N	\N	\N	2018-01-10 07:00:00	2018-01-10 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
123	f	BazSwi	34	\N	f	\N	\N	\N	2018-01-10 07:30:00	2018-01-10 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
124	f	sss	1	\N	f	\N	\N	\N	2018-01-09 07:00:00	2018-01-09 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
125	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-11 08:30:00	2018-01-11 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
126	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 09:30:00	2018-01-10 10:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
127	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-08 09:00:00	2018-01-08 09:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
128	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 08:30:00	2018-01-10 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
129	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 07:00:00	2018-01-10 07:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
130	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-11 07:30:00	2018-01-11 08:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
131	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-09 07:00:00	2018-01-09 07:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
132	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-12 09:00:00	2018-01-12 09:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
133	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-12 09:00:00	2018-01-12 09:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
134	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-11 07:00:00	2018-01-11 07:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
135	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-11 09:30:00	2018-01-11 10:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
136	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-11 07:00:00	2018-01-11 07:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
137	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-09 08:30:00	2018-01-09 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
138	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-08 08:30:00	2018-01-08 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
139	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 08:00:00	2018-01-10 08:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
140	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 08:30:00	2018-01-10 09:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
141	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-10 07:30:00	2018-01-10 08:03:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
143	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 07:00:00	2018-01-11 07:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
144	f	strps	28	\N	f	\N	\N	\N	2018-01-11 07:30:00	2018-01-11 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
145	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 08:30:00	2018-01-11 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
146	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-11 07:30:00	2018-01-11 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
147	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-12 09:00:00	2018-01-12 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
148	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-10 11:00:00	2018-01-10 11:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
149	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-10 07:30:00	2018-01-10 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
150	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-12 08:30:00	2018-01-12 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
151	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-10 09:30:00	2018-01-10 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
152	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-12 07:30:00	2018-01-12 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
153	f	mycok	28	\N	t	\N	\N	\N	2018-01-12 11:30:00	\N	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
154	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-11 12:00:00	2018-01-11 13:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
155	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-12 11:00:00	2018-01-12 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
156	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 10:00:00	2018-01-11 10:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
157	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-11 09:00:00	2018-01-11 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
158	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-18 09:00:00	2018-01-18 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
159	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-11 10:30:00	2018-01-11 11:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
160	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-12 07:30:00	2018-01-12 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
161	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 13:00:00	2018-01-11 13:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
162	f	WOPO	28	\N	f	\N	\N	\N	2018-01-12 09:00:00	2018-01-12 09:34:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
163	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-16 10:30:00	2018-01-16 11:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
164	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-18 09:00:00	2018-01-18 09:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
165	f	PUCBUT	58	\N	f	\N	\N	\N	2018-01-12 11:00:00	2018-01-12 11:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
166	f	WOPO	58	\N	t	\N	\N	\N	2018-01-17 09:30:00	2018-01-17 10:04:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
167	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 10:30:00	2018-01-11 11:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
168	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-12 13:30:00	2018-01-12 14:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
169	f	PUCBUT	56	\N	f	\N	\N	\N	2018-01-11 09:30:00	2018-01-11 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
170	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-12 12:30:00	2018-01-12 13:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
171	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-16 08:30:00	2018-01-16 09:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
172	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-11 12:00:00	2018-01-11 12:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
173	f	strps	72	\N	f	\N	\N	\N	2018-01-18 11:30:00	2018-01-18 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
174	f	WOPO	58	\N	f	\N	\N	\N	2018-01-15 12:30:00	2018-01-15 13:04:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
175	f	WOPO	28	\N	f	\N	\N	\N	2018-01-25 11:30:00	2018-01-25 12:04:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
176	f	strps	58	\N	f	\N	\N	\N	2018-01-15 07:30:00	2018-01-15 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
177	f	WySkBi	58	\N	f	\N	\N	\N	2018-01-15 07:30:00	2018-01-15 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
178	f	strps	56	\N	f	\N	\N	\N	2018-01-19 08:30:00	2018-01-19 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
179	f	KoUkEl	56	\N	f	\N	\N	\N	2018-01-19 08:30:00	2018-01-19 09:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
180	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-16 07:30:00	2018-01-16 08:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
181	f	myok	28	\N	f	\N	\N	\N	2018-01-19 11:30:00	2018-01-19 14:50:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
182	f	GLAPIE	28	\N	f	\N	\N	\N	2018-01-18 09:00:00	2018-01-18 09:33:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
183	f	strps	72	\N	f	\N	\N	\N	2018-01-19 11:30:00	2018-01-19 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
184	f	WySkBi	72	\N	f	\N	\N	\N	2018-01-18 10:30:00	2018-01-18 11:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
185	f	WySkBi	28	\N	f	\N	\N	\N	2018-01-15 09:30:00	2018-01-15 10:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
186	f	PUCBUT	28	\N	f	\N	\N	\N	2018-01-15 11:30:00	2018-01-15 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
187	f	KoUkEl	56	\N	f	\N	\N	\N	2018-01-17 08:00:00	2018-01-17 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
188	f	WySkBi	56	\N	f	\N	\N	\N	2018-01-19 12:00:00	2018-01-19 13:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
189	f	WySkBi	58	\N	f	\N	\N	\N	2018-01-17 09:00:00	2018-01-17 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
190	f	KoUkEl	8	\N	f	\N	\N	\N	2018-04-10 10:00:00	2018-04-10 10:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
191	f	sss	8	\N	f	\N	\N	\N	2018-04-11 07:00:00	2018-04-11 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
192	f	sss	8	\N	f	\N	\N	\N	2018-04-11 07:00:00	2018-04-11 07:22:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
193	f	WOPO	8	\N	f	\N	\N	\N	2018-04-11 10:00:00	2018-04-11 10:34:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
194	f	KoUkEl	8	\N	f	\N	\N	\N	2018-04-12 10:00:00	2018-04-12 10:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
195	f	Badham	8	\N	f	\N	\N	\N	2018-04-12 08:30:00	2018-04-12 08:50:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
196	f	WySkBi	8	\N	f	\N	\N	\N	2018-04-11 09:00:00	2018-04-11 10:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
197	f	WySkBi	8	\N	f	\N	\N	\N	2018-04-13 11:00:00	2018-04-13 12:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
198	f	WySkBi	8	\N	f	\N	\N	\N	2018-04-09 12:00:00	2018-04-09 13:00:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
199	f	WySkBi	8	\N	f	\N	\N	\N	2018-04-09 07:30:00	2018-04-09 08:30:00	\N	\N	-1	-1	t	t	\N	\N	\N	\N	\N	main
\.


--
-- Data for Name: service_archived; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY service_archived (id_service_archived, id_service, service_code, service_name, service_discount_amount, service_discount_percent, client, client_first_name, client_last_name, client_name, client_discount, location, location_name, planned_start, planned_end, real_start, real_end, reminder_sms_minutes, reminder_email_minutes, finished_info_sms, finished_info_email, notes, created_datetime, created_by, is_confirmed, confirmed_datetime, confirmed_by, is_deleted, deleted_timestamp, deleted_by, deleted_reason, company_branch, min_start_datetime) FROM stdin;
3	11	WOPO	\N	\N	\N	2	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
4	12	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
5	13	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
6	14	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
7	15	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
8	16	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
9	17	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
10	18	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
11	19	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
12	20	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
13	21	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
14	22	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
15	23	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
16	24	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
17	25	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
18	26	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
19	27	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
20	28	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
21	29	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
22	30	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
23	31	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
24	32	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
25	33	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
26	34	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
27	35	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
28	36	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
29	37	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
30	38	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
31	39	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
32	40	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
33	41	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
34	42	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
35	43	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
36	44	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
37	45	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
38	46	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
39	47	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
40	48	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
41	49	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
42	50	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
43	51	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
44	52	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
45	53	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
46	54	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
47	55	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
48	56	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
49	57	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
50	58	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
51	59	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
52	60	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
53	61	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
54	62	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
55	73	strps	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
56	74	AnaSpa	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
57	70	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
58	67	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
59	75	Badham	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
60	76	WySkBi	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
61	81	Badham	\N	\N	\N	34	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
62	88	KoUkEl	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
63	80	WOPO	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
64	142	strps	\N	\N	\N	65	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
65	100	KoUkEl	\N	\N	\N	28	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	f	\N	\N	t	\N	\N	\N	main	\N
\.


--
-- Data for Name: sex_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY sex_dict (id_sex_dict, sex) FROM stdin;
M	Mezczyzna
F	Kobieta
U	Nieokresl
\.


--
-- Data for Name: time_slot_list; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY time_slot_list (id_time_slot) FROM stdin;
\.


--
-- Data for Name: time_slot_params; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY time_slot_params (time_slot_minutes, time_slot_params_id) FROM stdin;
30	1
\.


--
-- Data for Name: wo_ability; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_ability (id_wo_ability, worker, worker_ability, notes, company_branch) FROM stdin;
3	4	WYOPO	\N	main
1	5	WYOPO	\N	main
2	6	WYOPO	\N	main
4	3	FREFE	\N	main
5	12	MECH	\N	main
6	13	ELEKT	\N	main
7	14	DIAG	\N	main
8	2	BASE	\N	main
9	1	BASE	\N	main
10	4	BASE	\N	main
11	5	BASE	\N	main
12	6	BASE	\N	main
13	9	BASE	\N	main
14	11	BASE	\N	main
15	3	BASE	\N	main
16	3	BASE	\N	main
17	8	BASE	\N	main
18	12	BASE	\N	main
19	13	BASE	\N	main
20	14	BASE	\N	main
\.


--
-- Data for Name: wo_ability_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_ability_dict (id_wo_ability_dict, ability_name, ability_group) FROM stdin;
WYOPO	Wymiana opon	\N
FREFE	Frezowanie felg	\N
ELEKT	Elektryk	\N
MECH	Mechanik	\N
DIAG	Diagnostyka	\N
BASE	Obeność	\N
\.


--
-- Data for Name: wo_ability_group_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_ability_group_dict (id_wo_ablility_group_dict, ability_group_name) FROM stdin;
\.


--
-- Data for Name: wo_absence; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_absence (id_wo_absence, worker, absence_type, start_datetime, end_datetime, workdays, hours, notes, company_branch) FROM stdin;
\.


--
-- Data for Name: wo_absence_type; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_absence_type (id_wo_absence_type, absence_name) FROM stdin;
\.


--
-- Data for Name: wo_group; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_group (id_wo_group, worker, worker_group, company_branch) FROM stdin;
1	9	1	main
2	15	1	main
\.


--
-- Data for Name: wo_group_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_group_dict (id_wo_group_dict, worker_group_name) FROM stdin;
1	wer
\.


--
-- Data for Name: wo_group_privilege; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_group_privilege (id_wo_group_privilege, worker_group, privilege_id, view_id, privilege_level) FROM stdin;
\.


--
-- Data for Name: wo_notification; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_notification (id_wo_notification, worker, worker_group, notification_subject, notification_text, severity, marked_as_read, company_branch, from_user) FROM stdin;
2	9	1	drugi	drugi tekst edycja	\N	\N	main	\N
1	9	1	pierwszy okta	pierwszy tekst edycja proba	\N	\N	main	\N
4	9	1	czwarty	czwarty oki	\N	\N	main	\N
6	2	\N	Temat	Tekst	2	\N	main	\N
5	1	1	Sprawdzenie	Działanie	\N	\N	main	\N
3	9	1	Trzeci	Trzeci temat ręczny.	\N	\N	main	\N
\.


--
-- Data for Name: wo_privilege_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_privilege_dict (id_wo_privilige_dict, privilege_name) FROM stdin;
\.


--
-- Data for Name: wo_privilege_level_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_privilege_level_dict (id_wo_privilege_level_dict) FROM stdin;
\.


--
-- Data for Name: wo_user; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY wo_user (app_user, worker) FROM stdin;
\.


--
-- Data for Name: workday_calendar; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY workday_calendar (id_workday_calendar, is_workday, company_branch, work_start, work_end) FROM stdin;
2017-11-06	t	main	08:00:00	16:00:00
2017-12-02	t	main	06:00:00	20:00:00
2017-12-03	f	main	\N	\N
2017-12-04	f	main	06:00:00	20:00:00
2017-11-13	t	main	06:00:00	20:00:00
2017-12-01	t	main	05:00:00	23:00:00
2017-12-10	t	main	08:00:00	20:00:00
2017-12-11	t	main	07:00:00	19:00:00
2017-12-12	t	main	07:00:00	13:00:00
2017-12-13	t	main	08:00:00	17:00:00
2017-12-14	t	main	09:00:00	20:00:00
2017-12-15	t	main	04:00:00	17:00:00
2017-12-16	t	main	04:00:00	17:00:00
2017-12-17	t	main	04:00:00	17:00:00
2017-12-18	t	main	04:00:00	17:00:00
2017-12-19	t	main	04:00:00	17:00:00
2017-12-20	t	main	04:00:00	17:00:00
2017-12-21	t	main	04:00:00	17:00:00
2017-12-22	t	main	04:00:00	17:00:00
2018-01-01	t	main	07:00:00	16:00:00
2018-01-02	t	main	07:00:00	16:00:00
2018-01-03	t	main	07:00:00	16:00:00
2018-01-04	t	main	07:00:00	16:00:00
2018-01-05	t	main	07:00:00	16:00:00
2018-01-06	f	main	07:00:00	16:00:00
2018-01-07	f	main	07:00:00	16:00:00
2018-01-08	t	main	07:00:00	16:00:00
2018-01-09	t	main	07:00:00	16:00:00
2018-01-10	t	main	07:00:00	16:00:00
2018-01-11	t	main	07:00:00	16:00:00
2018-01-12	t	main	07:00:00	16:00:00
2018-01-13	f	main	07:00:00	16:00:00
2018-01-14	f	main	07:00:00	16:00:00
2018-01-15	t	main	07:00:00	16:00:00
2018-01-16	t	main	07:00:00	16:00:00
2018-01-17	t	main	07:00:00	16:00:00
2018-01-18	t	main	07:00:00	16:00:00
2018-01-19	t	main	07:00:00	16:00:00
2018-01-20	f	main	07:00:00	16:00:00
2018-01-21	f	main	07:00:00	16:00:00
2018-01-22	t	main	07:00:00	16:00:00
2018-01-23	t	main	07:00:00	16:00:00
2018-01-24	t	main	07:00:00	16:00:00
2018-01-25	t	main	07:00:00	16:00:00
2018-01-26	t	main	07:00:00	16:00:00
2018-01-27	f	main	07:00:00	16:00:00
2018-01-28	f	main	07:00:00	16:00:00
2018-01-29	t	main	07:00:00	16:00:00
2018-01-30	t	main	07:00:00	16:00:00
2018-01-31	t	main	07:00:00	16:00:00
2018-02-01	t	main	07:00:00	16:00:00
2018-02-02	t	main	07:00:00	16:00:00
2018-02-03	f	main	07:00:00	16:00:00
2018-02-04	f	main	07:00:00	16:00:00
2018-02-05	t	main	07:00:00	16:00:00
2018-02-06	t	main	07:00:00	16:00:00
2018-02-07	t	main	07:00:00	16:00:00
2018-02-08	t	main	07:00:00	16:00:00
2018-02-09	t	main	07:00:00	16:00:00
2018-02-10	f	main	07:00:00	16:00:00
2018-02-11	f	main	07:00:00	16:00:00
2018-02-12	t	main	07:00:00	16:00:00
2018-02-13	t	main	07:00:00	16:00:00
2018-02-14	t	main	07:00:00	16:00:00
2018-02-15	t	main	07:00:00	16:00:00
2018-02-16	t	main	07:00:00	16:00:00
2018-02-17	f	main	07:00:00	16:00:00
2018-02-18	f	main	07:00:00	16:00:00
2018-02-19	t	main	07:00:00	16:00:00
2018-02-20	t	main	07:00:00	16:00:00
2018-02-21	t	main	07:00:00	16:00:00
2018-02-22	t	main	07:00:00	16:00:00
2018-02-23	t	main	07:00:00	16:00:00
2018-02-24	f	main	07:00:00	16:00:00
2018-02-25	f	main	07:00:00	16:00:00
2018-02-26	t	main	07:00:00	16:00:00
2018-02-27	t	main	07:00:00	16:00:00
2018-02-28	t	main	07:00:00	16:00:00
2018-03-01	t	main	07:00:00	16:00:00
2018-03-02	t	main	07:00:00	16:00:00
2018-03-03	f	main	07:00:00	16:00:00
2018-03-04	f	main	07:00:00	16:00:00
2018-03-05	t	main	07:00:00	16:00:00
2018-03-06	t	main	07:00:00	16:00:00
2018-03-07	t	main	07:00:00	16:00:00
2018-03-08	t	main	07:00:00	16:00:00
2018-03-09	t	main	07:00:00	16:00:00
2018-03-10	f	main	07:00:00	16:00:00
2018-03-11	f	main	07:00:00	16:00:00
2018-03-12	t	main	07:00:00	16:00:00
2018-03-13	t	main	07:00:00	16:00:00
2018-03-14	t	main	07:00:00	16:00:00
2018-03-15	t	main	07:00:00	16:00:00
2018-03-16	t	main	07:00:00	16:00:00
2018-03-17	f	main	07:00:00	16:00:00
2018-03-18	f	main	07:00:00	16:00:00
2018-03-19	t	main	07:00:00	16:00:00
2018-03-20	t	main	07:00:00	16:00:00
2018-03-21	t	main	07:00:00	16:00:00
2018-03-22	t	main	07:00:00	16:00:00
2018-03-23	t	main	07:00:00	16:00:00
2018-03-24	f	main	07:00:00	16:00:00
2018-03-25	f	main	07:00:00	16:00:00
2018-03-26	t	main	07:00:00	16:00:00
2018-03-27	t	main	07:00:00	16:00:00
2018-03-28	t	main	07:00:00	16:00:00
2018-03-29	t	main	07:00:00	16:00:00
2018-03-30	t	main	07:00:00	16:00:00
2018-03-31	f	main	07:00:00	16:00:00
2018-04-01	f	main	07:00:00	16:00:00
2018-04-02	t	main	07:00:00	16:00:00
2018-04-03	t	main	07:00:00	16:00:00
2018-04-04	t	main	07:00:00	16:00:00
2018-04-05	t	main	07:00:00	16:00:00
2018-04-06	t	main	07:00:00	16:00:00
2018-04-07	f	main	07:00:00	16:00:00
2018-04-08	f	main	07:00:00	16:00:00
2018-04-09	t	main	07:00:00	16:00:00
2018-04-10	t	main	07:00:00	16:00:00
2018-04-11	t	main	07:00:00	16:00:00
2018-04-12	t	main	07:00:00	16:00:00
2018-04-13	t	main	07:00:00	16:00:00
2018-04-14	f	main	07:00:00	16:00:00
2018-04-15	f	main	07:00:00	16:00:00
2018-04-16	t	main	07:00:00	16:00:00
2018-04-17	t	main	07:00:00	16:00:00
2018-04-18	t	main	07:00:00	16:00:00
2018-04-19	t	main	07:00:00	16:00:00
2018-04-20	t	main	07:00:00	16:00:00
2018-04-21	f	main	07:00:00	16:00:00
2018-04-22	f	main	07:00:00	16:00:00
2018-04-23	t	main	07:00:00	16:00:00
2018-04-24	t	main	07:00:00	16:00:00
2018-04-25	t	main	07:00:00	16:00:00
2018-04-26	t	main	07:00:00	16:00:00
2018-04-27	t	main	07:00:00	16:00:00
2018-04-28	f	main	07:00:00	16:00:00
2018-04-29	f	main	07:00:00	16:00:00
2018-04-30	t	main	07:00:00	16:00:00
2018-05-01	t	main	07:00:00	16:00:00
2018-05-02	t	main	07:00:00	16:00:00
2018-05-03	t	main	07:00:00	16:00:00
2018-05-04	t	main	07:00:00	16:00:00
2018-05-05	f	main	07:00:00	16:00:00
2018-05-06	f	main	07:00:00	16:00:00
2018-05-07	t	main	07:00:00	16:00:00
2018-05-08	t	main	07:00:00	16:00:00
2018-05-09	t	main	07:00:00	16:00:00
2018-05-10	t	main	07:00:00	16:00:00
2018-05-11	t	main	07:00:00	16:00:00
2018-05-12	f	main	07:00:00	16:00:00
2018-05-13	f	main	07:00:00	16:00:00
2018-05-14	t	main	07:00:00	16:00:00
2018-05-15	t	main	07:00:00	16:00:00
2018-05-16	t	main	07:00:00	16:00:00
2018-05-17	t	main	07:00:00	16:00:00
2018-05-18	t	main	07:00:00	16:00:00
2018-05-19	f	main	07:00:00	16:00:00
2018-05-20	f	main	07:00:00	16:00:00
2018-05-21	t	main	07:00:00	16:00:00
2018-05-22	t	main	07:00:00	16:00:00
2018-05-23	t	main	07:00:00	16:00:00
2018-05-24	t	main	07:00:00	16:00:00
2018-05-25	t	main	07:00:00	16:00:00
2018-05-26	f	main	07:00:00	16:00:00
2018-05-27	f	main	07:00:00	16:00:00
2018-05-28	t	main	07:00:00	16:00:00
2018-05-29	t	main	07:00:00	16:00:00
2018-05-30	t	main	07:00:00	16:00:00
2018-05-31	t	main	07:00:00	16:00:00
2018-06-01	t	main	07:00:00	16:00:00
2018-06-02	f	main	07:00:00	16:00:00
2018-06-03	f	main	07:00:00	16:00:00
2018-06-04	t	main	07:00:00	16:00:00
2018-06-05	t	main	07:00:00	16:00:00
2018-06-06	t	main	07:00:00	16:00:00
2018-06-07	t	main	07:00:00	16:00:00
2018-06-08	t	main	07:00:00	16:00:00
2018-06-09	f	main	07:00:00	16:00:00
2018-06-10	f	main	07:00:00	16:00:00
2018-06-11	t	main	07:00:00	16:00:00
2018-06-12	t	main	07:00:00	16:00:00
2018-06-13	t	main	07:00:00	16:00:00
2018-06-14	t	main	07:00:00	16:00:00
2018-06-15	t	main	07:00:00	16:00:00
2018-06-16	f	main	07:00:00	16:00:00
2018-06-17	f	main	07:00:00	16:00:00
2018-06-18	t	main	07:00:00	16:00:00
2018-06-19	t	main	07:00:00	16:00:00
2018-06-20	t	main	07:00:00	16:00:00
2018-06-21	t	main	07:00:00	16:00:00
2018-06-22	t	main	07:00:00	16:00:00
2018-06-23	f	main	07:00:00	16:00:00
2018-06-24	f	main	07:00:00	16:00:00
2018-06-25	t	main	07:00:00	16:00:00
2018-06-26	t	main	07:00:00	16:00:00
2018-06-27	t	main	07:00:00	16:00:00
2018-06-28	t	main	07:00:00	16:00:00
2018-06-29	t	main	07:00:00	16:00:00
2018-06-30	f	main	07:00:00	16:00:00
2018-07-01	f	main	07:00:00	16:00:00
2018-07-02	t	main	07:00:00	16:00:00
2018-07-03	t	main	07:00:00	16:00:00
2018-07-04	t	main	07:00:00	16:00:00
2018-07-05	t	main	07:00:00	16:00:00
2018-07-06	t	main	07:00:00	16:00:00
2018-07-07	f	main	07:00:00	16:00:00
2018-07-08	f	main	07:00:00	16:00:00
2018-07-09	t	main	07:00:00	16:00:00
2018-07-10	t	main	07:00:00	16:00:00
2018-07-11	t	main	07:00:00	16:00:00
2018-07-12	t	main	07:00:00	16:00:00
2018-07-13	t	main	07:00:00	16:00:00
2018-07-14	f	main	07:00:00	16:00:00
2018-07-15	f	main	07:00:00	16:00:00
2018-07-16	t	main	07:00:00	16:00:00
2018-07-17	t	main	07:00:00	16:00:00
2018-07-18	t	main	07:00:00	16:00:00
2018-07-19	t	main	07:00:00	16:00:00
2018-07-20	t	main	07:00:00	16:00:00
2018-07-21	f	main	07:00:00	16:00:00
2018-07-22	f	main	07:00:00	16:00:00
2018-07-23	t	main	07:00:00	16:00:00
2018-07-24	t	main	07:00:00	16:00:00
2018-07-25	t	main	07:00:00	16:00:00
2018-07-26	t	main	07:00:00	16:00:00
2018-07-27	t	main	07:00:00	16:00:00
2018-07-28	f	main	07:00:00	16:00:00
2018-07-29	f	main	07:00:00	16:00:00
2018-07-30	t	main	07:00:00	16:00:00
2018-07-31	t	main	07:00:00	16:00:00
2018-08-01	t	main	07:00:00	16:00:00
2018-08-02	t	main	07:00:00	16:00:00
2018-08-03	t	main	07:00:00	16:00:00
2018-08-04	f	main	07:00:00	16:00:00
2018-08-05	f	main	07:00:00	16:00:00
2018-08-06	t	main	07:00:00	16:00:00
2018-08-07	t	main	07:00:00	16:00:00
2018-08-08	t	main	07:00:00	16:00:00
2018-08-09	t	main	07:00:00	16:00:00
2018-08-10	t	main	07:00:00	16:00:00
2018-08-11	f	main	07:00:00	16:00:00
2018-08-12	f	main	07:00:00	16:00:00
2018-08-13	t	main	07:00:00	16:00:00
2018-08-14	t	main	07:00:00	16:00:00
2018-08-15	t	main	07:00:00	16:00:00
2018-08-16	t	main	07:00:00	16:00:00
2018-08-17	t	main	07:00:00	16:00:00
2018-08-18	f	main	07:00:00	16:00:00
2018-08-19	f	main	07:00:00	16:00:00
2018-08-20	t	main	07:00:00	16:00:00
2018-08-21	t	main	07:00:00	16:00:00
2018-08-22	t	main	07:00:00	16:00:00
2018-08-23	t	main	07:00:00	16:00:00
2018-08-24	t	main	07:00:00	16:00:00
2018-08-25	f	main	07:00:00	16:00:00
2018-08-26	f	main	07:00:00	16:00:00
2018-08-27	t	main	07:00:00	16:00:00
2018-08-28	t	main	07:00:00	16:00:00
2018-08-29	t	main	07:00:00	16:00:00
2018-08-30	t	main	07:00:00	16:00:00
2018-08-31	t	main	07:00:00	16:00:00
2018-09-01	f	main	07:00:00	16:00:00
2018-09-02	f	main	07:00:00	16:00:00
2018-09-03	t	main	07:00:00	16:00:00
2018-09-04	t	main	07:00:00	16:00:00
2018-09-05	t	main	07:00:00	16:00:00
2018-09-06	t	main	07:00:00	16:00:00
2018-09-07	t	main	07:00:00	16:00:00
2018-09-08	f	main	07:00:00	16:00:00
2018-09-09	f	main	07:00:00	16:00:00
2018-09-10	t	main	07:00:00	16:00:00
2018-09-11	t	main	07:00:00	16:00:00
2018-09-12	t	main	07:00:00	16:00:00
2018-09-13	t	main	07:00:00	16:00:00
2018-09-14	t	main	07:00:00	16:00:00
2018-09-15	f	main	07:00:00	16:00:00
2018-09-16	f	main	07:00:00	16:00:00
2018-09-17	t	main	07:00:00	16:00:00
2018-09-18	t	main	07:00:00	16:00:00
2018-09-19	t	main	07:00:00	16:00:00
2018-09-20	t	main	07:00:00	16:00:00
2018-09-21	t	main	07:00:00	16:00:00
2018-09-22	f	main	07:00:00	16:00:00
2018-09-23	f	main	07:00:00	16:00:00
2018-09-24	t	main	07:00:00	16:00:00
2018-09-25	t	main	07:00:00	16:00:00
2018-09-26	t	main	07:00:00	16:00:00
2018-09-27	t	main	07:00:00	16:00:00
2018-09-28	t	main	07:00:00	16:00:00
2018-09-29	f	main	07:00:00	16:00:00
2018-09-30	f	main	07:00:00	16:00:00
2018-10-01	t	main	07:00:00	16:00:00
2018-10-02	t	main	07:00:00	16:00:00
2018-10-03	t	main	07:00:00	16:00:00
2018-10-04	t	main	07:00:00	16:00:00
2018-10-05	t	main	07:00:00	16:00:00
2018-10-06	f	main	07:00:00	16:00:00
2018-10-07	f	main	07:00:00	16:00:00
2018-10-08	t	main	07:00:00	16:00:00
2018-10-09	t	main	07:00:00	16:00:00
2018-10-10	t	main	07:00:00	16:00:00
2018-10-11	t	main	07:00:00	16:00:00
2018-10-12	t	main	07:00:00	16:00:00
2018-10-13	f	main	07:00:00	16:00:00
2018-10-14	f	main	07:00:00	16:00:00
2018-10-15	t	main	07:00:00	16:00:00
2018-10-16	t	main	07:00:00	16:00:00
2018-10-17	t	main	07:00:00	16:00:00
2018-10-18	t	main	07:00:00	16:00:00
2018-10-19	t	main	07:00:00	16:00:00
2018-10-20	f	main	07:00:00	16:00:00
2018-10-21	f	main	07:00:00	16:00:00
2018-10-22	t	main	07:00:00	16:00:00
2018-10-23	t	main	07:00:00	16:00:00
2018-10-24	t	main	07:00:00	16:00:00
2018-10-25	t	main	07:00:00	16:00:00
2018-10-26	t	main	07:00:00	16:00:00
2018-10-27	f	main	07:00:00	16:00:00
2018-10-28	f	main	07:00:00	16:00:00
2018-10-29	t	main	07:00:00	16:00:00
2018-10-30	t	main	07:00:00	16:00:00
2018-10-31	t	main	07:00:00	16:00:00
2018-11-01	t	main	07:00:00	16:00:00
2018-11-02	t	main	07:00:00	16:00:00
2018-11-03	f	main	07:00:00	16:00:00
2018-11-04	f	main	07:00:00	16:00:00
2018-11-05	t	main	07:00:00	16:00:00
2018-11-06	t	main	07:00:00	16:00:00
2018-11-07	t	main	07:00:00	16:00:00
2018-11-08	t	main	07:00:00	16:00:00
2018-11-09	t	main	07:00:00	16:00:00
2018-11-10	f	main	07:00:00	16:00:00
2018-11-11	f	main	07:00:00	16:00:00
2018-11-12	t	main	07:00:00	16:00:00
2018-11-13	t	main	07:00:00	16:00:00
2018-11-14	t	main	07:00:00	16:00:00
2018-11-15	t	main	07:00:00	16:00:00
2018-11-16	t	main	07:00:00	16:00:00
2018-11-17	f	main	07:00:00	16:00:00
2018-11-18	f	main	07:00:00	16:00:00
2018-11-19	t	main	07:00:00	16:00:00
2018-11-20	t	main	07:00:00	16:00:00
2018-11-21	t	main	07:00:00	16:00:00
2018-11-22	t	main	07:00:00	16:00:00
2018-11-23	t	main	07:00:00	16:00:00
2018-11-24	f	main	07:00:00	16:00:00
2018-11-25	f	main	07:00:00	16:00:00
2018-11-26	t	main	07:00:00	16:00:00
2018-11-27	t	main	07:00:00	16:00:00
2018-11-28	t	main	07:00:00	16:00:00
2018-11-29	t	main	07:00:00	16:00:00
2018-11-30	t	main	07:00:00	16:00:00
2018-12-01	f	main	07:00:00	16:00:00
2018-12-02	f	main	07:00:00	16:00:00
2018-12-03	t	main	07:00:00	16:00:00
2018-12-04	t	main	07:00:00	16:00:00
2018-12-05	t	main	07:00:00	16:00:00
2018-12-06	t	main	07:00:00	16:00:00
2018-12-07	t	main	07:00:00	16:00:00
2018-12-08	f	main	07:00:00	16:00:00
2018-12-09	f	main	07:00:00	16:00:00
2018-12-10	t	main	07:00:00	16:00:00
2018-12-11	t	main	07:00:00	16:00:00
2018-12-12	t	main	07:00:00	16:00:00
2018-12-13	t	main	07:00:00	16:00:00
2018-12-14	t	main	07:00:00	16:00:00
2018-12-15	f	main	07:00:00	16:00:00
2018-12-16	f	main	07:00:00	16:00:00
2018-12-17	t	main	07:00:00	16:00:00
2018-12-18	t	main	07:00:00	16:00:00
2018-12-19	t	main	07:00:00	16:00:00
2018-12-20	t	main	07:00:00	16:00:00
2018-12-21	t	main	07:00:00	16:00:00
2018-12-22	f	main	07:00:00	16:00:00
2018-12-23	f	main	07:00:00	16:00:00
2018-12-24	t	main	07:00:00	16:00:00
2018-12-25	t	main	07:00:00	16:00:00
2018-12-26	t	main	07:00:00	16:00:00
2018-12-27	t	main	07:00:00	16:00:00
2018-12-28	t	main	07:00:00	16:00:00
2018-12-29	f	main	07:00:00	16:00:00
2018-12-30	f	main	07:00:00	16:00:00
2018-12-31	t	main	07:00:00	16:00:00
2019-01-01	t	main	07:00:00	16:00:00
2017-12-23	t	main	07:00:00	14:00:00
2017-12-25	t	main	07:00:00	14:00:00
\.


--
-- Data for Name: workday_calendar_params; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY workday_calendar_params (default_is_saturday_workday, workday_calendar_params_id, default_workday_start_time, default_workday_end_time, default_saturday_start_time, default_saturday_end_time, days_to_display) FROM stdin;
f	1	06:00:00	14:00:00	\N	\N	61
\.


--
-- Data for Name: worker; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY worker (id_worker, first_name, last_name, worker_title, active, address, notes, company_branch, email, phone, prefered_contact_type, worker_user_login) FROM stdin;
2	Adam	Zbadam	pan licencjat	t	1	\N	main	\N	\N	\N	\N
4	Adam	Adamski	\N	t	\N	\N	main	\N	\N	\N	\N
5	Barbara	Basienko	\N	t	\N	\N	main	\N	\N	\N	\N
6	Cezary	Cezary	\N	t	\N	\N	main	\N	\N	\N	6
9	Adam	Mózg	idealne	t	3	dotyczace	main	\N	\N	\N	7
11	Krzyś	S	\N	t	37	\N	main	\N	\N	\N	1
15	Mateusz	Z	Kierownik	t	71	\N	main	\N	\N	\N	29
16	Mateusz	Zet	Kierownik	t	71	\N	main	\N	\N	\N	4
17	Mati	Z	Mechanik	t	26	\N	main	\N	\N	\N	31
8	Mariusz	Dawidowski	\N	t	5	\N	main	\N	\N	\N	\N
3	Bizon	Szybki	smyracz	t	3	\N	main	\N	\N	\N	34
1	Krzysztof	Poczdamski	majster	t	1	\N	main	\N	\N	\N	2
12	Tom	Pac	Mechanik	t	26	\N	main	\N	\N	\N	\N
13	Hardy	Burn	Elektryk	t	6	\N	main	\N	\N	\N	\N
14	Milo	Corty	Diagnosta	t	28	\N	main	\N	\N	\N	\N
\.


--
-- Data for Name: zip_codes_dict; Type: TABLE DATA; Schema: public; Owner: ks
--

COPY zip_codes_dict (id_zip_codes_dict, country, zip, city) FROM stdin;
\.


--
-- Name: auth_group_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_group_id_seq', 2, true);


--
-- Name: auth_group_permissions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_group_permissions_id_seq', 3, true);


--
-- Name: auth_permission_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_permission_id_seq', 165, true);


--
-- Name: auth_user_groups_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_user_groups_id_seq', 7, true);


--
-- Name: auth_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_user_id_seq', 34, true);


--
-- Name: auth_user_user_permissions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('auth_user_user_permissions_id_seq', 165, true);


--
-- Name: cl_communication_log_id_cl_communication_log_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_communication_log_id_cl_communication_log_seq', 1, false);


--
-- Name: cl_discount_id_cl_discount_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_discount_id_cl_discount_seq', 1, true);


--
-- Name: cl_params_cl_params_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_params_cl_params_id_seq', 1, true);


--
-- Name: cl_payment_id_cl_payment_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_payment_id_cl_payment_seq', 1, true);


--
-- Name: cl_payment_line_id_cl_payment_line_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_payment_line_id_cl_payment_line_seq', 1, true);


--
-- Name: cl_unconfirmed_cl_unconfirmed_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('cl_unconfirmed_cl_unconfirmed_seq', 1, false);


--
-- Name: client_id_client_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('client_id_client_seq', 73, true);


--
-- Name: contact_id_contact_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('contact_id_contact_seq', 80, true);


--
-- Name: django_admin_log_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('django_admin_log_id_seq', 433, true);


--
-- Name: django_content_type_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('django_content_type_id_seq', 86, true);


--
-- Name: django_migrations_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('django_migrations_id_seq', 16, true);


--
-- Name: location_id_location_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('location_id_location_seq', 9, true);


--
-- Name: machine_id_machine_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('machine_id_machine_seq', 28, true);


--
-- Name: resources_usage_id_resources_usage_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('resources_usage_id_resources_usage_seq', 158, true);


--
-- Name: resources_usage_params_resources_usage_params_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('resources_usage_params_resources_usage_params_id_seq', 1, true);


--
-- Name: se_discount_id_se_discount_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('se_discount_id_se_discount_seq', 1, true);


--
-- Name: se_requirement_id_se_requirement_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('se_requirement_id_se_requirement_seq', 15, true);


--
-- Name: service_archived_id_service_archived_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('service_archived_id_service_archived_seq', 65, true);


--
-- Name: service_id_service_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('service_id_service_seq', 199, true);


--
-- Name: time_slot_params_time_slot_params_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('time_slot_params_time_slot_params_id_seq', 1, true);


--
-- Name: wo_ability_id_wo_ability_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('wo_ability_id_wo_ability_seq', 21, true);


--
-- Name: wo_group_id_wo_group_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('wo_group_id_wo_group_seq', 2, true);


--
-- Name: wo_group_privilege_id_wo_group_privilege_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('wo_group_privilege_id_wo_group_privilege_seq', 1, false);


--
-- Name: wo_notification_id_wo_notification_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('wo_notification_id_wo_notification_seq', 6, true);


--
-- Name: workday_calendar_params_workday_calendar_params_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('workday_calendar_params_workday_calendar_params_id_seq', 1, true);


--
-- Name: worker_id_worker_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('worker_id_worker_seq', 17, true);


--
-- Name: zip_codes_dict_id_zip_codes_dict_seq; Type: SEQUENCE SET; Schema: public; Owner: ks
--

SELECT pg_catalog.setval('zip_codes_dict_id_zip_codes_dict_seq', 1, false);


--
-- Name: auth_group auth_group_name_key; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group
    ADD CONSTRAINT auth_group_name_key UNIQUE (name);


--
-- Name: auth_group_permissions auth_group_permissions_group_id_permission_id_0cd325b0_uniq; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_group_id_permission_id_0cd325b0_uniq UNIQUE (group_id, permission_id);


--
-- Name: auth_group_permissions auth_group_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_pkey PRIMARY KEY (id);


--
-- Name: auth_group auth_group_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group
    ADD CONSTRAINT auth_group_pkey PRIMARY KEY (id);


--
-- Name: auth_permission auth_permission_content_type_id_codename_01ab375a_uniq; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_permission
    ADD CONSTRAINT auth_permission_content_type_id_codename_01ab375a_uniq UNIQUE (content_type_id, codename);


--
-- Name: auth_permission auth_permission_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_permission
    ADD CONSTRAINT auth_permission_pkey PRIMARY KEY (id);


--
-- Name: auth_user_groups auth_user_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_groups
    ADD CONSTRAINT auth_user_groups_pkey PRIMARY KEY (id);


--
-- Name: auth_user_groups auth_user_groups_user_id_group_id_94350c0c_uniq; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_groups
    ADD CONSTRAINT auth_user_groups_user_id_group_id_94350c0c_uniq UNIQUE (user_id, group_id);


--
-- Name: auth_user auth_user_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user
    ADD CONSTRAINT auth_user_pkey PRIMARY KEY (id);


--
-- Name: auth_user_user_permissions auth_user_user_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_pkey PRIMARY KEY (id);


--
-- Name: auth_user_user_permissions auth_user_user_permissions_user_id_permission_id_14a6b632_uniq; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_user_id_permission_id_14a6b632_uniq UNIQUE (user_id, permission_id);


--
-- Name: auth_user auth_user_username_key; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user
    ADD CONSTRAINT auth_user_username_key UNIQUE (username);


--
-- Name: cl_blocked_reason_dict cl_blocked_reason_dict__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_blocked_reason_dict
    ADD CONSTRAINT cl_blocked_reason_dict__un UNIQUE (blocked_reason_name);


--
-- Name: cl_blocked_reason_dict cl_blocked_reason_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_blocked_reason_dict
    ADD CONSTRAINT cl_blocked_reason_dict_pk PRIMARY KEY (id_cl_blocked_reason_dict);


--
-- Name: cl_communication_log cl_communication_log_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_pk PRIMARY KEY (id_cl_communication_log);


--
-- Name: cl_communication_reason cl_communication_reason__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_reason
    ADD CONSTRAINT cl_communication_reason__un UNIQUE (reason_name);


--
-- Name: cl_communication_reason cl_communication_reason_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_reason
    ADD CONSTRAINT cl_communication_reason_pk PRIMARY KEY (id_client_communication_reason);


--
-- Name: cl_discount cl_discount__unv1; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_discount
    ADD CONSTRAINT cl_discount__unv1 UNIQUE (client, discount);


--
-- Name: cl_discount cl_discount_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_discount
    ADD CONSTRAINT cl_discount_pk PRIMARY KEY (id_cl_discount);


--
-- Name: cl_params cl_params_cl_params_id_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_params
    ADD CONSTRAINT cl_params_cl_params_id_pk PRIMARY KEY (cl_params_id);


--
-- Name: cl_payment cl_payment__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment
    ADD CONSTRAINT cl_payment__un UNIQUE (invoice_voucher);


--
-- Name: cl_payment_line cl_payment_line_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line
    ADD CONSTRAINT cl_payment_line_pk PRIMARY KEY (id_cl_payment_line);


--
-- Name: cl_payment cl_payment_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment
    ADD CONSTRAINT cl_payment_pk PRIMARY KEY (id_cl_payment);


--
-- Name: cl_unconfirmed cl_unconfirmed_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_unconfirmed
    ADD CONSTRAINT cl_unconfirmed_pk PRIMARY KEY (cl_unconfirmed);


--
-- Name: client client_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_pk PRIMARY KEY (id_client);


--
-- Name: company_branch company_branch_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY company_branch
    ADD CONSTRAINT company_branch_pk PRIMARY KEY (id_company_branch);


--
-- Name: address contact_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY address
    ADD CONSTRAINT contact_pk PRIMARY KEY (id_address);


--
-- Name: contact_type contact_type__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY contact_type
    ADD CONSTRAINT contact_type__un UNIQUE (contact_type_name);


--
-- Name: contact_type contact_type_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY contact_type
    ADD CONSTRAINT contact_type_pk PRIMARY KEY (id_contact_type);


--
-- Name: address contact_un_email; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY address
    ADD CONSTRAINT contact_un_email UNIQUE (email);


--
-- Name: country_dict country_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY country_dict
    ADD CONSTRAINT country_dict_pk PRIMARY KEY (country);


--
-- Name: currrency currrency__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY currrency
    ADD CONSTRAINT currrency__un UNIQUE (currency_name);


--
-- Name: currrency currrency_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY currrency
    ADD CONSTRAINT currrency_pk PRIMARY KEY (id_currency);


--
-- Name: discount_dict discount_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY discount_dict
    ADD CONSTRAINT discount_dict_pk PRIMARY KEY (id_discount_dict);


--
-- Name: discount_scope discount_scope_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY discount_scope
    ADD CONSTRAINT discount_scope_pk PRIMARY KEY (id_discount_scope);


--
-- Name: django_admin_log django_admin_log_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_admin_log
    ADD CONSTRAINT django_admin_log_pkey PRIMARY KEY (id);


--
-- Name: django_content_type django_content_type_app_label_model_76bd3d3b_uniq; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_content_type
    ADD CONSTRAINT django_content_type_app_label_model_76bd3d3b_uniq UNIQUE (app_label, model);


--
-- Name: django_content_type django_content_type_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_content_type
    ADD CONSTRAINT django_content_type_pkey PRIMARY KEY (id);


--
-- Name: django_migrations django_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_migrations
    ADD CONSTRAINT django_migrations_pkey PRIMARY KEY (id);


--
-- Name: django_session django_session_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_session
    ADD CONSTRAINT django_session_pkey PRIMARY KEY (session_key);


--
-- Name: location location__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location
    ADD CONSTRAINT location__un UNIQUE (location_name);


--
-- Name: location location_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location
    ADD CONSTRAINT location_pk PRIMARY KEY (id_location);


--
-- Name: location_type location_type__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location_type
    ADD CONSTRAINT location_type__un UNIQUE (location_type_name);


--
-- Name: location_type location_type_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location_type
    ADD CONSTRAINT location_type_pk PRIMARY KEY (id_location_type);


--
-- Name: machine machine_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY machine
    ADD CONSTRAINT machine_pk PRIMARY KEY (id_machine);


--
-- Name: machine_type machine_type_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY machine_type
    ADD CONSTRAINT machine_type_pk PRIMARY KEY (id_machine_type);


--
-- Name: resources_usage resources_usage__un_machine_slot_date; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage__un_machine_slot_date UNIQUE (machine, time_slot, calendar_date);


--
-- Name: resources_usage resources_usage__un_worker_slot_date; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage__un_worker_slot_date UNIQUE (worker, time_slot, calendar_date);


--
-- Name: resources_usage_params resources_usage_params_resources_usage_params_id_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage_params
    ADD CONSTRAINT resources_usage_params_resources_usage_params_id_pk PRIMARY KEY (resources_usage_params_id);


--
-- Name: resources_usage resources_usage_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_pk PRIMARY KEY (id_resources_usage);


--
-- Name: se_dict se_dict__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_dict
    ADD CONSTRAINT se_dict__un UNIQUE (se_dict_name);


--
-- Name: se_dict se_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_dict
    ADD CONSTRAINT se_dict_pk PRIMARY KEY (id_se_dict);


--
-- Name: se_group_dict se_group_dict_pkey; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_group_dict
    ADD CONSTRAINT se_group_dict_pkey PRIMARY KEY (id_se_group_dict);


--
-- Name: se_requirement se_requirement_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement
    ADD CONSTRAINT se_requirement_pk PRIMARY KEY (id_se_requirement);


--
-- Name: service_archived service_archived_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service_archived
    ADD CONSTRAINT service_archived_pk PRIMARY KEY (id_service_archived);


--
-- Name: service service_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_pk PRIMARY KEY (id_service);


--
-- Name: sex_dict sex_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY sex_dict
    ADD CONSTRAINT sex_dict_pk PRIMARY KEY (id_sex_dict);


--
-- Name: time_slot_list time_slot_list_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY time_slot_list
    ADD CONSTRAINT time_slot_list_pk PRIMARY KEY (id_time_slot);


--
-- Name: time_slot_params time_slot_params_time_slot_params_id_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY time_slot_params
    ADD CONSTRAINT time_slot_params_time_slot_params_id_pk PRIMARY KEY (time_slot_params_id);


--
-- Name: wo_ability_dict wo_ability_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability_dict
    ADD CONSTRAINT wo_ability_dict_pk PRIMARY KEY (id_wo_ability_dict);


--
-- Name: wo_ability_group_dict wo_ability_group_dict__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability_group_dict
    ADD CONSTRAINT wo_ability_group_dict__un UNIQUE (ability_group_name);


--
-- Name: wo_ability_group_dict wo_ability_group_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability_group_dict
    ADD CONSTRAINT wo_ability_group_dict_pk PRIMARY KEY (id_wo_ablility_group_dict);


--
-- Name: wo_ability wo_ability_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability
    ADD CONSTRAINT wo_ability_pk PRIMARY KEY (id_wo_ability);


--
-- Name: wo_absence wo_absence_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence
    ADD CONSTRAINT wo_absence_pk PRIMARY KEY (id_wo_absence);


--
-- Name: wo_absence_type wo_absence_type_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence_type
    ADD CONSTRAINT wo_absence_type_pk PRIMARY KEY (id_wo_absence_type);


--
-- Name: wo_group_dict wo_group_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_dict
    ADD CONSTRAINT wo_group_dict_pk PRIMARY KEY (id_wo_group_dict);


--
-- Name: wo_group wo_group_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group
    ADD CONSTRAINT wo_group_pk PRIMARY KEY (id_wo_group);


--
-- Name: wo_group_privilege wo_group_privilege__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege
    ADD CONSTRAINT wo_group_privilege__un UNIQUE (worker_group, privilege_id);


--
-- Name: wo_group_privilege wo_group_privilege_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege
    ADD CONSTRAINT wo_group_privilege_pk PRIMARY KEY (id_wo_group_privilege);


--
-- Name: wo_notification wo_notification_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification
    ADD CONSTRAINT wo_notification_pk PRIMARY KEY (id_wo_notification);


--
-- Name: wo_privilege_dict wo_privilege_dict__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_privilege_dict
    ADD CONSTRAINT wo_privilege_dict__un UNIQUE (privilege_name);


--
-- Name: wo_privilege_level_dict wo_privilege_level_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_privilege_level_dict
    ADD CONSTRAINT wo_privilege_level_dict_pk PRIMARY KEY (id_wo_privilege_level_dict);


--
-- Name: wo_privilege_dict wo_privilige_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_privilege_dict
    ADD CONSTRAINT wo_privilige_dict_pk PRIMARY KEY (id_wo_privilige_dict);


--
-- Name: workday_calendar_params workday_calendar_params_workday_calendar_params_id_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY workday_calendar_params
    ADD CONSTRAINT workday_calendar_params_workday_calendar_params_id_pk PRIMARY KEY (workday_calendar_params_id);


--
-- Name: workday_calendar workday_calendar_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY workday_calendar
    ADD CONSTRAINT workday_calendar_pk PRIMARY KEY (id_workday_calendar);


--
-- Name: wo_absence_type worker_absence_type__un; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence_type
    ADD CONSTRAINT worker_absence_type__un UNIQUE (absence_name);


--
-- Name: worker worker_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY worker
    ADD CONSTRAINT worker_pk PRIMARY KEY (id_worker);


--
-- Name: wo_user worker_user_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_user
    ADD CONSTRAINT worker_user_pk PRIMARY KEY (app_user);


--
-- Name: zip_codes_dict zip_codes_dict_pk; Type: CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY zip_codes_dict
    ADD CONSTRAINT zip_codes_dict_pk PRIMARY KEY (id_zip_codes_dict);


--
-- Name: auth_group_name_a6ea08ec_like; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_group_name_a6ea08ec_like ON auth_group USING btree (name varchar_pattern_ops);


--
-- Name: auth_group_permissions_group_id_b120cbf9; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_group_permissions_group_id_b120cbf9 ON auth_group_permissions USING btree (group_id);


--
-- Name: auth_group_permissions_permission_id_84c5c92e; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_group_permissions_permission_id_84c5c92e ON auth_group_permissions USING btree (permission_id);


--
-- Name: auth_permission_content_type_id_2f476e4b; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_permission_content_type_id_2f476e4b ON auth_permission USING btree (content_type_id);


--
-- Name: auth_user_groups_group_id_97559544; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_user_groups_group_id_97559544 ON auth_user_groups USING btree (group_id);


--
-- Name: auth_user_groups_user_id_6a12ed8b; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_user_groups_user_id_6a12ed8b ON auth_user_groups USING btree (user_id);


--
-- Name: auth_user_user_permissions_permission_id_1fbb5f2c; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_user_user_permissions_permission_id_1fbb5f2c ON auth_user_user_permissions USING btree (permission_id);


--
-- Name: auth_user_user_permissions_user_id_a95ead1b; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_user_user_permissions_user_id_a95ead1b ON auth_user_user_permissions USING btree (user_id);


--
-- Name: auth_user_username_6821ab7c_like; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX auth_user_username_6821ab7c_like ON auth_user USING btree (username varchar_pattern_ops);


--
-- Name: cl_discount__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX cl_discount__idx ON cl_discount USING btree (client);


--
-- Name: cl_params_cl_params_id_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX cl_params_cl_params_id_uindex ON cl_params USING btree (cl_params_id);


--
-- Name: cl_payment__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX cl_payment__idx ON cl_payment USING btree (id_cl_payment, invoice_voucher, client);


--
-- Name: cl_payment_line__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX cl_payment_line__idx ON cl_payment_line USING btree (id_cl_payment_line, payment);


--
-- Name: client__idxv2; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX client__idxv2 ON client USING btree (id_client);


--
-- Name: client_email_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX client_email_uindex ON client USING btree (email);


--
-- Name: client_phone_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX client_phone_uindex ON client USING btree (phone);


--
-- Name: company_branch_email_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX company_branch_email_uindex ON company_branch USING btree (email);


--
-- Name: company_branch_phone_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX company_branch_phone_uindex ON company_branch USING btree (phone);


--
-- Name: contact__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX contact__idx ON address USING btree (id_address);


--
-- Name: discount_dict__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX discount_dict__idx ON discount_dict USING btree (id_discount_dict);


--
-- Name: django_admin_log_content_type_id_c4bce8eb; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX django_admin_log_content_type_id_c4bce8eb ON django_admin_log USING btree (content_type_id);


--
-- Name: django_admin_log_user_id_c564eba6; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX django_admin_log_user_id_c564eba6 ON django_admin_log USING btree (user_id);


--
-- Name: django_session_expire_date_a5c62663; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX django_session_expire_date_a5c62663 ON django_session USING btree (expire_date);


--
-- Name: django_session_session_key_c0390e0f_like; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX django_session_session_key_c0390e0f_like ON django_session USING btree (session_key varchar_pattern_ops);


--
-- Name: machine__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX machine__idx ON machine USING btree (id_machine);


--
-- Name: resources_usage_params_resources_usage_params_id_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX resources_usage_params_resources_usage_params_id_uindex ON resources_usage_params USING btree (resources_usage_params_id);


--
-- Name: se_dict__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX se_dict__idx ON se_dict USING btree (id_se_dict, se_dict_name);


--
-- Name: se_group_dict_id_se_group_dict_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX se_group_dict_id_se_group_dict_uindex ON se_group_dict USING btree (id_se_group_dict);


--
-- Name: se_group_dict_name_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX se_group_dict_name_uindex ON se_group_dict USING btree (se_group_dict_name);


--
-- Name: se_requirement__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX se_requirement__idx ON se_requirement USING btree (service_code);


--
-- Name: service__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX service__idx ON service USING btree (id_service);


--
-- Name: time_slot_params_time_slot_params_id_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX time_slot_params_time_slot_params_id_uindex ON time_slot_params USING btree (time_slot_params_id);


--
-- Name: workday_calendar_params_workday_calendar_params_id_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX workday_calendar_params_workday_calendar_params_id_uindex ON workday_calendar_params USING btree (workday_calendar_params_id);


--
-- Name: worker__idx; Type: INDEX; Schema: public; Owner: ks
--

CREATE INDEX worker__idx ON worker USING btree (id_worker);


--
-- Name: worker_email_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX worker_email_uindex ON worker USING btree (email);


--
-- Name: worker_phone_uindex; Type: INDEX; Schema: public; Owner: ks
--

CREATE UNIQUE INDEX worker_phone_uindex ON worker USING btree (phone);


--
-- Name: service archive_service; Type: TRIGGER; Schema: public; Owner: ks
--

CREATE TRIGGER archive_service BEFORE DELETE ON service FOR EACH ROW EXECUTE PROCEDURE archive_service();


--
-- Name: auth_group_permissions auth_group_permissio_permission_id_84c5c92e_fk_auth_perm; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group_permissions
    ADD CONSTRAINT auth_group_permissio_permission_id_84c5c92e_fk_auth_perm FOREIGN KEY (permission_id) REFERENCES auth_permission(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_group_permissions auth_group_permissions_group_id_b120cbf9_fk_auth_group_id; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_group_id_b120cbf9_fk_auth_group_id FOREIGN KEY (group_id) REFERENCES auth_group(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_permission auth_permission_content_type_id_2f476e4b_fk_django_co; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_permission
    ADD CONSTRAINT auth_permission_content_type_id_2f476e4b_fk_django_co FOREIGN KEY (content_type_id) REFERENCES django_content_type(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_groups auth_user_groups_group_id_97559544_fk_auth_group_id; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_groups
    ADD CONSTRAINT auth_user_groups_group_id_97559544_fk_auth_group_id FOREIGN KEY (group_id) REFERENCES auth_group(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_groups auth_user_groups_user_id_6a12ed8b_fk_auth_user_id; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_groups
    ADD CONSTRAINT auth_user_groups_user_id_6a12ed8b_fk_auth_user_id FOREIGN KEY (user_id) REFERENCES auth_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_user_permissions auth_user_user_permi_permission_id_1fbb5f2c_fk_auth_perm; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permi_permission_id_1fbb5f2c_fk_auth_perm FOREIGN KEY (permission_id) REFERENCES auth_permission(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_user_permissions auth_user_user_permissions_user_id_a95ead1b_fk_auth_user_id; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_user_id_a95ead1b_fk_auth_user_id FOREIGN KEY (user_id) REFERENCES auth_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: cl_communication_log cl_communication_log_cl_communication_reason_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_cl_communication_reason_fk FOREIGN KEY (communication_reason) REFERENCES cl_communication_reason(id_client_communication_reason);


--
-- Name: cl_communication_log cl_communication_log_client_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_client_fk FOREIGN KEY (client) REFERENCES client(id_client);


--
-- Name: cl_communication_log cl_communication_log_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: cl_communication_log cl_communication_log_contact_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_contact_type_fk FOREIGN KEY (contact_type) REFERENCES contact_type(id_contact_type);


--
-- Name: cl_communication_log cl_communication_log_service_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_communication_log
    ADD CONSTRAINT cl_communication_log_service_fk FOREIGN KEY (service) REFERENCES service(id_service);


--
-- Name: cl_discount cl_discount_client_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_discount
    ADD CONSTRAINT cl_discount_client_fk FOREIGN KEY (client) REFERENCES client(id_client);


--
-- Name: cl_discount cl_discount_discount_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_discount
    ADD CONSTRAINT cl_discount_discount_dict_fk FOREIGN KEY (discount) REFERENCES discount_dict(id_discount_dict);


--
-- Name: cl_payment cl_payment_client_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment
    ADD CONSTRAINT cl_payment_client_fk FOREIGN KEY (client) REFERENCES client(id_client);


--
-- Name: cl_payment cl_payment_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment
    ADD CONSTRAINT cl_payment_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: cl_payment cl_payment_currrency_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment
    ADD CONSTRAINT cl_payment_currrency_fk FOREIGN KEY (currency) REFERENCES currrency(id_currency);


--
-- Name: cl_payment_line cl_payment_line_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line
    ADD CONSTRAINT cl_payment_line_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: cl_payment_line cl_payment_line_currrency_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line
    ADD CONSTRAINT cl_payment_line_currrency_fk FOREIGN KEY (currency) REFERENCES currrency(id_currency);


--
-- Name: cl_payment_line cl_payment_line_payment_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line
    ADD CONSTRAINT cl_payment_line_payment_fk FOREIGN KEY (payment) REFERENCES cl_payment(id_cl_payment);


--
-- Name: cl_payment_line cl_payment_line_service_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_payment_line
    ADD CONSTRAINT cl_payment_line_service_fk FOREIGN KEY (service) REFERENCES service(id_service);


--
-- Name: cl_unconfirmed cl_unconfirmed_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_unconfirmed
    ADD CONSTRAINT cl_unconfirmed_company_branch_fk FOREIGN KEY (default_company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: client client_blocked_reason_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_blocked_reason_dict_fk FOREIGN KEY (blocked_reason_id) REFERENCES cl_blocked_reason_dict(id_cl_blocked_reason_dict);


--
-- Name: client client_client_user_login_auth_user_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_client_user_login_auth_user_fk FOREIGN KEY (client_user_login) REFERENCES auth_user(id);


--
-- Name: client client_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_company_branch_fk FOREIGN KEY (default_company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: client client_contact_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_contact_fk FOREIGN KEY (address) REFERENCES address(id_address);


--
-- Name: client client_contact_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_contact_type_fk FOREIGN KEY (prefered_contact_type) REFERENCES contact_type(id_contact_type) ON UPDATE CASCADE;


--
-- Name: cl_params client_params_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_params
    ADD CONSTRAINT client_params_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: cl_params client_params_currrency_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY cl_params
    ADD CONSTRAINT client_params_currrency_fk FOREIGN KEY (default_currency) REFERENCES currrency(id_currency);


--
-- Name: client client_sex_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY client
    ADD CONSTRAINT client_sex_dict_fk FOREIGN KEY (sex) REFERENCES sex_dict(id_sex_dict);


--
-- Name: address contact_contact_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY address
    ADD CONSTRAINT contact_contact_type_fk FOREIGN KEY (prefered_contact_type) REFERENCES contact_type(id_contact_type);


--
-- Name: address contact_country_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY address
    ADD CONSTRAINT contact_country_dict_fk FOREIGN KEY (country) REFERENCES country_dict(country);


--
-- Name: discount_dict discount_dict_discount_scope_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY discount_dict
    ADD CONSTRAINT discount_dict_discount_scope_fk FOREIGN KEY (scope) REFERENCES discount_scope(id_discount_scope);


--
-- Name: django_admin_log django_admin_log_content_type_id_c4bce8eb_fk_django_co; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_admin_log
    ADD CONSTRAINT django_admin_log_content_type_id_c4bce8eb_fk_django_co FOREIGN KEY (content_type_id) REFERENCES django_content_type(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: django_admin_log django_admin_log_user_id_c564eba6_fk_auth_user_id; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY django_admin_log
    ADD CONSTRAINT django_admin_log_user_id_c564eba6_fk_auth_user_id FOREIGN KEY (user_id) REFERENCES auth_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: location location_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location
    ADD CONSTRAINT location_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: location location_location_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY location
    ADD CONSTRAINT location_location_type_fk FOREIGN KEY (location_type) REFERENCES location_type(id_location_type);


--
-- Name: machine machine_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY machine
    ADD CONSTRAINT machine_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: machine machine_machine_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY machine
    ADD CONSTRAINT machine_machine_type_fk FOREIGN KEY (machine_type) REFERENCES machine_type(id_machine_type);


--
-- Name: resources_usage resource_usage_machine_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resource_usage_machine_fk FOREIGN KEY (machine) REFERENCES machine(id_machine);


--
-- Name: resources_usage resources_usage_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: resources_usage resources_usage_location_id_location_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_location_id_location_fk FOREIGN KEY (location) REFERENCES location(id_location) ON UPDATE CASCADE;


--
-- Name: resources_usage resources_usage_service_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_service_fk FOREIGN KEY (service) REFERENCES service(id_service) ON DELETE CASCADE;


--
-- Name: resources_usage resources_usage_time_slot_list_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_time_slot_list_fk FOREIGN KEY (time_slot) REFERENCES time_slot_list(id_time_slot);


--
-- Name: resources_usage resources_usage_workday_calendar_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_workday_calendar_fk FOREIGN KEY (calendar_date) REFERENCES workday_calendar(id_workday_calendar);


--
-- Name: resources_usage resources_usage_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY resources_usage
    ADD CONSTRAINT resources_usage_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: se_dict se_dict_location_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_dict
    ADD CONSTRAINT se_dict_location_type_fk FOREIGN KEY (location_type) REFERENCES location_type(id_location_type);


--
-- Name: se_dict se_dict_se_group_dict_id_se_group_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_dict
    ADD CONSTRAINT se_dict_se_group_dict_id_se_group_dict_fk FOREIGN KEY (se_group) REFERENCES se_group_dict(id_se_group_dict) ON UPDATE CASCADE;


--
-- Name: se_discount se_discount_discount_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_discount
    ADD CONSTRAINT se_discount_discount_dict_fk FOREIGN KEY (discount) REFERENCES discount_dict(id_discount_dict);


--
-- Name: se_discount se_discount_service_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_discount
    ADD CONSTRAINT se_discount_service_fk FOREIGN KEY (service) REFERENCES service(id_service);


--
-- Name: se_requirement se_requirement_location_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement
    ADD CONSTRAINT se_requirement_location_type_fk FOREIGN KEY (location_type) REFERENCES location_type(id_location_type) ON UPDATE CASCADE;


--
-- Name: se_requirement se_requirement_machine_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement
    ADD CONSTRAINT se_requirement_machine_type_fk FOREIGN KEY (machine_type) REFERENCES machine_type(id_machine_type);


--
-- Name: se_requirement se_requirement_se_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement
    ADD CONSTRAINT se_requirement_se_dict_fk FOREIGN KEY (service_code) REFERENCES se_dict(id_se_dict);


--
-- Name: se_requirement se_requirement_wo_ability_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY se_requirement
    ADD CONSTRAINT se_requirement_wo_ability_dict_fk FOREIGN KEY (worker_ability) REFERENCES wo_ability_dict(id_wo_ability_dict) ON UPDATE CASCADE;


--
-- Name: service service_client_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_client_fk FOREIGN KEY (client) REFERENCES client(id_client);


--
-- Name: service service_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: service service_location_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_location_fk FOREIGN KEY (location) REFERENCES location(id_location);


--
-- Name: service service_se_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_se_dict_fk FOREIGN KEY (service_code) REFERENCES se_dict(id_se_dict);


--
-- Name: wo_ability wo_ability_ability_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability
    ADD CONSTRAINT wo_ability_ability_dict_fk FOREIGN KEY (worker_ability) REFERENCES wo_ability_dict(id_wo_ability_dict);


--
-- Name: wo_ability wo_ability_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability
    ADD CONSTRAINT wo_ability_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: wo_ability_dict wo_ability_dict_wo_ability_group_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability_dict
    ADD CONSTRAINT wo_ability_dict_wo_ability_group_fk FOREIGN KEY (ability_group) REFERENCES wo_ability_group_dict(id_wo_ablility_group_dict);


--
-- Name: wo_ability wo_ability_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_ability
    ADD CONSTRAINT wo_ability_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: wo_absence wo_absence_absence_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence
    ADD CONSTRAINT wo_absence_absence_type_fk FOREIGN KEY (absence_type) REFERENCES wo_absence_type(id_wo_absence_type);


--
-- Name: wo_absence wo_absence_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence
    ADD CONSTRAINT wo_absence_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: wo_absence wo_absence_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_absence
    ADD CONSTRAINT wo_absence_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: wo_group wo_group_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group
    ADD CONSTRAINT wo_group_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: wo_group_privilege wo_group_privilege_wo_group_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege
    ADD CONSTRAINT wo_group_privilege_wo_group_dict_fk FOREIGN KEY (worker_group) REFERENCES wo_group_dict(id_wo_group_dict);


--
-- Name: wo_group_privilege wo_group_privilege_wo_privilege_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege
    ADD CONSTRAINT wo_group_privilege_wo_privilege_dict_fk FOREIGN KEY (privilege_id) REFERENCES wo_privilege_dict(id_wo_privilige_dict);


--
-- Name: wo_group_privilege wo_group_privilege_wo_privilege_level_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group_privilege
    ADD CONSTRAINT wo_group_privilege_wo_privilege_level_dict_fk FOREIGN KEY (privilege_level) REFERENCES wo_privilege_level_dict(id_wo_privilege_level_dict);


--
-- Name: wo_group wo_group_wo_group_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group
    ADD CONSTRAINT wo_group_wo_group_dict_fk FOREIGN KEY (worker_group) REFERENCES wo_group_dict(id_wo_group_dict);


--
-- Name: wo_group wo_group_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_group
    ADD CONSTRAINT wo_group_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: wo_notification wo_notification_auth_user_username_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification
    ADD CONSTRAINT wo_notification_auth_user_username_fk FOREIGN KEY (from_user) REFERENCES auth_user(username) ON UPDATE CASCADE;


--
-- Name: wo_notification wo_notification_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification
    ADD CONSTRAINT wo_notification_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: wo_notification wo_notification_wo_group_list_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification
    ADD CONSTRAINT wo_notification_wo_group_list_fk FOREIGN KEY (worker_group) REFERENCES wo_group_dict(id_wo_group_dict);


--
-- Name: wo_notification wo_notification_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_notification
    ADD CONSTRAINT wo_notification_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: workday_calendar workday_calendar_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY workday_calendar
    ADD CONSTRAINT workday_calendar_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch);


--
-- Name: worker worker_company_branch_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY worker
    ADD CONSTRAINT worker_company_branch_fk FOREIGN KEY (company_branch) REFERENCES company_branch(id_company_branch) ON UPDATE CASCADE;


--
-- Name: worker worker_contact_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY worker
    ADD CONSTRAINT worker_contact_type_fk FOREIGN KEY (prefered_contact_type) REFERENCES contact_type(id_contact_type) ON UPDATE CASCADE;


--
-- Name: wo_user worker_user_worker_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY wo_user
    ADD CONSTRAINT worker_user_worker_fk FOREIGN KEY (worker) REFERENCES worker(id_worker);


--
-- Name: worker worker_worker_user_login_auth_user_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY worker
    ADD CONSTRAINT worker_worker_user_login_auth_user_fk FOREIGN KEY (worker_user_login) REFERENCES auth_user(id);


--
-- Name: zip_codes_dict zip_codes_dict_country_dict_fk; Type: FK CONSTRAINT; Schema: public; Owner: ks
--

ALTER TABLE ONLY zip_codes_dict
    ADD CONSTRAINT zip_codes_dict_country_dict_fk FOREIGN KEY (country) REFERENCES country_dict(country);


--
-- Name: public; Type: ACL; Schema: -; Owner: ks
--

REVOKE ALL ON SCHEMA public FROM rdsadmin;
REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO ks;
GRANT ALL ON SCHEMA public TO PUBLIC;
GRANT USAGE ON SCHEMA public TO rd_only;


--
-- Name: address; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE address TO rd_only;


--
-- Name: auth_group; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_group TO rd_only;


--
-- Name: auth_group_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_group_id_seq TO rd_only;


--
-- Name: auth_group_permissions; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_group_permissions TO rd_only;


--
-- Name: auth_group_permissions_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_group_permissions_id_seq TO rd_only;


--
-- Name: auth_permission; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_permission TO rd_only;


--
-- Name: auth_permission_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_permission_id_seq TO rd_only;


--
-- Name: auth_user; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_user TO rd_only;


--
-- Name: auth_user_groups; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_user_groups TO rd_only;


--
-- Name: auth_user_groups_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_user_groups_id_seq TO rd_only;


--
-- Name: auth_user_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_user_id_seq TO rd_only;


--
-- Name: auth_user_user_permissions; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE auth_user_user_permissions TO rd_only;


--
-- Name: auth_user_user_permissions_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE auth_user_user_permissions_id_seq TO rd_only;


--
-- Name: cl_blocked_reason_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_blocked_reason_dict TO rd_only;


--
-- Name: cl_communication_log; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_communication_log TO rd_only;


--
-- Name: cl_communication_log_id_cl_communication_log_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_communication_log_id_cl_communication_log_seq TO rd_only;


--
-- Name: cl_communication_reason; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_communication_reason TO rd_only;


--
-- Name: cl_discount; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_discount TO rd_only;


--
-- Name: cl_discount_id_cl_discount_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_discount_id_cl_discount_seq TO rd_only;


--
-- Name: cl_params; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_params TO rd_only;


--
-- Name: cl_params_cl_params_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_params_cl_params_id_seq TO rd_only;


--
-- Name: cl_payment; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_payment TO rd_only;


--
-- Name: cl_payment_id_cl_payment_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_payment_id_cl_payment_seq TO rd_only;


--
-- Name: cl_payment_line; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_payment_line TO rd_only;


--
-- Name: cl_payment_line_id_cl_payment_line_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_payment_line_id_cl_payment_line_seq TO rd_only;


--
-- Name: cl_unconfirmed; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE cl_unconfirmed TO rd_only;


--
-- Name: cl_unconfirmed_cl_unconfirmed_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE cl_unconfirmed_cl_unconfirmed_seq TO rd_only;


--
-- Name: client; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client TO rd_only;


--
-- Name: client_block_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_block_v TO rd_only;


--
-- Name: client_blocked_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_blocked_list_v TO rd_only;


--
-- Name: contact_type; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE contact_type TO rd_only;


--
-- Name: se_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE se_dict TO rd_only;


--
-- Name: service; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service TO rd_only;


--
-- Name: client_communication_log_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_communication_log_v TO rd_only;


--
-- Name: client_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_details_v TO rd_only;


--
-- Name: client_id_client_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE client_id_client_seq TO rd_only;


--
-- Name: client_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_list_v TO rd_only;


--
-- Name: client_not_yet_accepted_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_not_yet_accepted_list_v TO rd_only;


--
-- Name: client_params_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_params_v TO rd_only;


--
-- Name: client_payment_line_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_payment_line_v TO rd_only;


--
-- Name: client_payment_list_all_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_payment_list_all_v TO rd_only;


--
-- Name: client_payment_list_closed_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_payment_list_closed_v TO rd_only;


--
-- Name: client_payment_list_posted_unpaid_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_payment_list_posted_unpaid_v TO rd_only;


--
-- Name: client_payment_list_unposted_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE client_payment_list_unposted_v TO rd_only;


--
-- Name: company_branch; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE company_branch TO rd_only;


--
-- Name: company_description; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE company_description TO rd_only;


--
-- Name: contact_id_contact_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE contact_id_contact_seq TO rd_only;


--
-- Name: country_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE country_dict TO rd_only;


--
-- Name: currrency; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE currrency TO rd_only;


--
-- Name: discount_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE discount_dict TO rd_only;


--
-- Name: discount_scope; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE discount_scope TO rd_only;


--
-- Name: discount_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE discount_v TO rd_only;


--
-- Name: django_admin_log; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE django_admin_log TO rd_only;


--
-- Name: django_admin_log_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE django_admin_log_id_seq TO rd_only;


--
-- Name: django_content_type; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE django_content_type TO rd_only;


--
-- Name: django_content_type_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE django_content_type_id_seq TO rd_only;


--
-- Name: django_migrations; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE django_migrations TO rd_only;


--
-- Name: django_migrations_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE django_migrations_id_seq TO rd_only;


--
-- Name: django_session; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE django_session TO rd_only;


--
-- Name: location; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE location TO rd_only;


--
-- Name: location_id_location_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE location_id_location_seq TO rd_only;


--
-- Name: location_type; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE location_type TO rd_only;


--
-- Name: location_type_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE location_type_details_v TO rd_only;


--
-- Name: location_type_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE location_type_list_v TO rd_only;


--
-- Name: location_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE location_v TO rd_only;


--
-- Name: machine; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine TO rd_only;


--
-- Name: machine_id_machine_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE machine_id_machine_seq TO rd_only;


--
-- Name: machine_type; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine_type TO rd_only;


--
-- Name: machine_incoming_services_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine_incoming_services_v TO rd_only;


--
-- Name: machine_type_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine_type_details_v TO rd_only;


--
-- Name: machine_type_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine_type_list_v TO rd_only;


--
-- Name: machine_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE machine_v TO rd_only;


--
-- Name: payment_list_overdue_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE payment_list_overdue_v TO rd_only;


--
-- Name: resources_usage; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE resources_usage TO rd_only;


--
-- Name: resources_usage_id_resources_usage_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE resources_usage_id_resources_usage_seq TO rd_only;


--
-- Name: worker; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker TO rd_only;


--
-- Name: resources_usage_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE resources_usage_list_v TO rd_only;


--
-- Name: resources_usage_params; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE resources_usage_params TO rd_only;


--
-- Name: resources_usage_params_resources_usage_params_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE resources_usage_params_resources_usage_params_id_seq TO rd_only;


--
-- Name: resources_usage_params_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE resources_usage_params_v TO rd_only;


--
-- Name: se_discount; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE se_discount TO rd_only;


--
-- Name: se_discount_id_se_discount_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE se_discount_id_se_discount_seq TO rd_only;


--
-- Name: se_group_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE se_group_dict TO rd_only;


--
-- Name: se_requirement; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE se_requirement TO rd_only;


--
-- Name: se_requirement_id_se_requirement_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE se_requirement_id_se_requirement_seq TO rd_only;


--
-- Name: service_accept_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_accept_v TO rd_only;


--
-- Name: service_archived; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_archived TO rd_only;


--
-- Name: service_archived_id_service_archived_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE service_archived_id_service_archived_seq TO rd_only;


--
-- Name: service_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_details_v TO rd_only;


--
-- Name: service_dict_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_dict_details_v TO rd_only;


--
-- Name: service_dict_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_dict_list_v TO rd_only;


--
-- Name: service_id_service_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE service_id_service_seq TO rd_only;


--
-- Name: service_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE service_list_v TO rd_only;


--
-- Name: sex_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE sex_dict TO rd_only;


--
-- Name: time_slot_list; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE time_slot_list TO rd_only;


--
-- Name: time_slot_params; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE time_slot_params TO rd_only;


--
-- Name: time_slot_params_time_slot_params_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE time_slot_params_time_slot_params_id_seq TO rd_only;


--
-- Name: time_slot_params_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE time_slot_params_v TO rd_only;


--
-- Name: wo_ability; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_ability TO rd_only;


--
-- Name: wo_ability_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_ability_dict TO rd_only;


--
-- Name: wo_ability_group_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_ability_group_dict TO rd_only;


--
-- Name: wo_ability_id_wo_ability_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE wo_ability_id_wo_ability_seq TO rd_only;


--
-- Name: wo_absence; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_absence TO rd_only;


--
-- Name: wo_absence_type; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_absence_type TO rd_only;


--
-- Name: wo_group; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_group TO rd_only;


--
-- Name: wo_group_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_group_dict TO rd_only;


--
-- Name: wo_group_id_wo_group_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE wo_group_id_wo_group_seq TO rd_only;


--
-- Name: wo_group_privilege; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_group_privilege TO rd_only;


--
-- Name: wo_group_privilege_id_wo_group_privilege_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE wo_group_privilege_id_wo_group_privilege_seq TO rd_only;


--
-- Name: wo_notification; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_notification TO rd_only;


--
-- Name: wo_notification_id_wo_notification_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE wo_notification_id_wo_notification_seq TO rd_only;


--
-- Name: wo_privilege_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_privilege_dict TO rd_only;


--
-- Name: wo_privilege_level_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_privilege_level_dict TO rd_only;


--
-- Name: wo_user; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE wo_user TO rd_only;


--
-- Name: workday_calendar; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE workday_calendar TO rd_only;


--
-- Name: workday_calendar_params; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE workday_calendar_params TO rd_only;


--
-- Name: workday_calendar_params_workday_calendar_params_id_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE workday_calendar_params_workday_calendar_params_id_seq TO rd_only;


--
-- Name: worker_ability_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_details_v TO rd_only;


--
-- Name: worker_ability_dict_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_dict_details_v TO rd_only;


--
-- Name: worker_ability_dict_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_dict_list_v TO rd_only;


--
-- Name: worker_ability_group_dict_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_group_dict_details_v TO rd_only;


--
-- Name: worker_ability_group_dict_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_group_dict_list_v TO rd_only;


--
-- Name: worker_ability_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_ability_list_v TO rd_only;


--
-- Name: worker_absence_all_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_all_v TO rd_only;


--
-- Name: worker_absence_calendar_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_calendar_v TO rd_only;


--
-- Name: worker_absence_current_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_current_v TO rd_only;


--
-- Name: worker_absence_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_details_v TO rd_only;


--
-- Name: worker_absence_ended_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_ended_v TO rd_only;


--
-- Name: worker_absence_future_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_future_v TO rd_only;


--
-- Name: worker_absence_hour_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_hour_v TO rd_only;


--
-- Name: worker_absence_other_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_other_v TO rd_only;


--
-- Name: worker_absence_recreational_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_recreational_v TO rd_only;


--
-- Name: worker_absence_sick_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_absence_sick_v TO rd_only;


--
-- Name: worker_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_details_v TO rd_only;


--
-- Name: worker_group_dict_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_group_dict_details_v TO rd_only;


--
-- Name: worker_group_dict_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_group_dict_list_v TO rd_only;


--
-- Name: worker_group_privilege_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_group_privilege_v TO rd_only;


--
-- Name: worker_id_worker_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE worker_id_worker_seq TO rd_only;


--
-- Name: worker_list_active_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_list_active_v TO rd_only;


--
-- Name: worker_list_all_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_list_all_v TO rd_only;


--
-- Name: worker_list_inactive_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_list_inactive_v TO rd_only;


--
-- Name: worker_list_incoming_leave_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_list_incoming_leave_v TO rd_only;


--
-- Name: worker_list_on_leave_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_list_on_leave_v TO rd_only;


--
-- Name: worker_user_details_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_user_details_v TO rd_only;


--
-- Name: worker_user_list_v; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE worker_user_list_v TO rd_only;


--
-- Name: zip_codes_dict; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON TABLE zip_codes_dict TO rd_only;


--
-- Name: zip_codes_dict_id_zip_codes_dict_seq; Type: ACL; Schema: public; Owner: ks
--

GRANT SELECT ON SEQUENCE zip_codes_dict_id_zip_codes_dict_seq TO rd_only;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: ks
--

ALTER DEFAULT PRIVILEGES FOR ROLE ks IN SCHEMA public REVOKE ALL ON TABLES  FROM ks;
ALTER DEFAULT PRIVILEGES FOR ROLE ks IN SCHEMA public GRANT SELECT ON TABLES  TO rd_only;


--
-- PostgreSQL database dump complete
--

