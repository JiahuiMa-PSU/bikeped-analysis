--checking which state has full 12 month data in which year
WITH monthly_output AS (
WITH
detector AS (
	SELECT flow_detector_id, detector_id, functional_classification
	FROM bike_ped.metadata
	WHERE bicycle = true AND detector_automated = true
)

SELECT detector.functional_classification, 
		SUM(volume) AS t_volume, 
	    SUM(sample_count) AS t_sample_count, 
	    SUM(volume_corrected) AS t_volume_corrected,
	    state, start_time
FROM detector
JOIN bike_ped.data_agg_1month monthly
ON detector.flow_detector_id = monthly.flow_detector_id
GROUP BY state, start_time, functional_classification
ORDER BY state, start_time, functional_classification
)


SELECT DISTINCT COUNT(*), state, EXTRACT(YEAR FROM start_time) AS yyyy, functional_classification
FROM monthly_output
GROUP BY state, yyyy, functional_classification


	
-- generating month_state_fc_avg.csv
WITH
detector AS (
--comp_ratio: complete ratio of detectors in 2023
WITH comp_ratio AS (
SELECT
	DATE(a.start_time) AS date,
    b.flow_detector_id,
    COUNT(*) AS record_count, -- expect 24 for traffic counters; for eco-counter in portland every 15min. VA all eco-counter, MN and DC have mixed.
	CASE 
	WHEN COUNT(*) <= 24 THEN COUNT(*)/24.00
	ELSE COUNT(*)/96.00
	END AS comp_ratio
FROM bike_ped.data AS a
JOIN bike_ped.metadata AS b
ON a.flow_detector_id = b.flow_detector_id
WHERE EXTRACT(YEAR FROM a.start_time) = 2023 
		AND bicycle = true 
		AND detector_automated = true  
		AND state IN ('MN', 'DC', 'VA')
GROUP BY DATE(a.start_time), b.flow_detector_id
)

	SELECT m.flow_detector_id, detector_id, functional_classification
	FROM bike_ped.metadata m
	LEFT JOIN comp_ratio c
	ON c.flow_detector_id = m.flow_detector_id
	WHERE bicycle = true 
		AND detector_automated = true 
		AND comp_ratio >= 0.80 
		AND state IN ('MN', 'DC', 'VA')
)

SELECT detector.functional_classification, 
		AVG(volume) AS avg_volume, 
	    AVG(sample_count) AS avg_sample_count, 
	    state, start_time
FROM detector
JOIN bike_ped.data_agg_1month monthly
ON detector.flow_detector_id = monthly.flow_detector_id
WHERE EXTRACT(YEAR FROM monthly.start_time) = 2023 
GROUP BY state, start_time, functional_classification
ORDER BY state, start_time, functional_classification

