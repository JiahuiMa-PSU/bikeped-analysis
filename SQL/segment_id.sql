--segment_id.csv before cleaning description, facility_type, and lat, long
with segment_id_list as
	(
	select distinct segment_area_id
	from bike_ped.metadata
	where 
	  state in ('VA', 'DC','MD') and
	  --bicycle = TRUE and
	  detector_automated = TRUE and
	  eco_channel_id is not null 
	order by segment_area_id
	)

select distinct
	  t1.segment_area_id, 
	  t1.segment_name, 
	  t1.state, 
	  t1.city, 
	  t1.functional_classification, 
	  t2.description,
	  t2.facility_type, 
	  t1.lat,t1.long
	from
	  bike_ped.metadata t1
	left join  bike_ped.facilities t2 
	  on (t1.segment_area_id = t2.segment_area_id AND t1.facility_id = t2.facility_id)
	inner join segment_id_list s
		on t1.segment_area_id = s.segment_area_id;

--madt.csv
with segment_id_list as
	(
	select distinct segment_area_id
	from bike_ped.metadata
	where 
	  state in ('VA', 'DC','MD') and
	  --bicycle = TRUE and
	  detector_automated = TRUE and
	  eco_channel_id is not null 
	order by segment_area_id
	)


	select madt.segment_area_id, madt.start_time, madt.flow_type, madt.volume
	from bike_ped.madt madt
	inner join segment_id_list s
		on madt.segment_area_id = s.segment_area_id
	order by segment_area_id, start_time

