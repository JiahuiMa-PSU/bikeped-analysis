WITH
detector AS (
	SELECT flow_detector_id, detector_id, functional_classification
	FROM bike_ped.metadata
	WHERE bicycle = true AND detector_automated = true
),

--checking which state has full 12 month data in which year
checklist AS (
SELECT COUNT(*), state, EXTRACT(YEAR FROM start_time) AS yyyy
FROM (SELECT SUM(volume) AS t_volume, 
	   SUM(sample_count) AS t_sample_count, 
	   SUM(volume_corrected) AS t_volume_corrected,
	   state, start_time
FROM bike_ped.data_agg_1month monthly
GROUP BY  state, start_time
ORDER BY  state, start_time
) mon
GROUP BY state, yyyy
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
