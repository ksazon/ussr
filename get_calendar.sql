create function get_calendar(
    _service character varying
    , _start timestamp without time zone
    , _end timestamp without time zone)

returns TABLE(
    from_time timestamp without time zone
    , to_time timestamp without time zone
    , free_workers text
    , free_machines text
    , free_locations text)

language plpgsql
as $$

DECLARE
ts_len            INTEGER;
sr_duration       INTEGER;
empty_sr          RECORD;
worker_required   INTEGER;
location_required INTEGER;
machine_required  INTEGER;

BEGIN
ts_len := (
    SELECT time_slot_minutes
    FROM time_slot_params
    
    LIMIT 1);

sr_duration := (
    SELECT SUM(time_minutes)
    FROM se_requirement
    
    WHERE se_requirement.service_code = _service);

worker_required := (
    SELECT CASE WHEN MAX(s_r.worker_ability) IS NULL
        THEN 0
    ELSE 1
    END
    
    FROM se_requirement s_r
    
    WHERE s_r.service_code = _service
    LIMIT 1);
--raise notice 'wr %', worker_required;

machine_required := (
    SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
        THEN 0
    ELSE 1
    END
    
    FROM se_requirement s_r
    
    WHERE s_r.service_code = _service
    
    LIMIT 1);
--raise notice 'mr %', machine_required;

location_required := (
    SELECT CASE WHEN MAX(s_r.machine_type) IS NULL
        THEN 0
    ELSE 1
    END
    
    FROM se_requirement s_r
    
    WHERE s_r.service_code = _service
    
    LIMIT 1);
--raise notice 'lr %', location_required;

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
        AND w.active = TRUE),

sr_machines AS (
    SELECT m.id_machine
    FROM se_requirement s_r
    
    JOIN machine m
        ON s_r.machine_type = m.machine_type
    
    WHERE s_r.service_code = _service
        AND m.is_operational = TRUE
    
    UNION ALL
    
    SELECT -1
    
    WHERE location_required = 0),

sr_location AS (
    SELECT l.id_location
    FROM se_requirement s_r
    
    JOIN location l
        ON s_r.location_type = l.location_type
    
    WHERE s_r.service_code = _service
        AND l.is_operational = TRUE
    
    UNION ALL
    
    SELECT -1
    WHERE location_required = 0),

timeslots AS (
    SELECT w_c.id_workday_calendar date, gen_ser.timeslot_start
    FROM workday_calendar w_c
    
    , LATERAL (
        SELECT generate_series :: TIMESTAMP timeslot_start
        
        FROM generate_series((w_c.id_workday_calendar + w_c.work_start) :: TIMESTAMP
            , (w_c.id_workday_calendar + w_c.work_end) :: TIMESTAMP
            , INTERVAL '1min' * ts_len)) gen_ser
    
    WHERE w_c.is_workday = TRUE
    
    ORDER BY date, timeslot_start)

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
    NOT EXISTS (
        SELECT 'x'    
        FROM resources_usage r_u
        
        WHERE r_u.worker = srw.id_worker
            AND r_u.finish_timestamp > ts2.timeslot_start
            AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration)

    AND (machine_required = 0
        OR
        NOT EXISTS(
            SELECT 'x'
            FROM resources_usage r_u
            
            WHERE r_u.machine = srm.id_machine
                AND r_u.finish_timestamp > ts2.timeslot_start
                AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration))

    AND (worker_required = 0
        OR
        NOT EXISTS(
            SELECT 'x'
            FROM resources_usage r_u
            
            WHERE r_u.location = srl.id_location
                AND r_u.finish_timestamp > ts2.timeslot_start
                AND r_u.start_timestamp < ts2.timeslot_start + INTERVAL '1min' * sr_duration))

    AND ts1.timeslot_start >= _start
    AND ts1.timeslot_start <= _end

GROUP BY ts2.timeslot_start

ORDER BY ts2.timeslot_start ASC

LIMIT 10000;

END;
$$;

