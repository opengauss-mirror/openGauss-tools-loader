  SELECT DISTINCT table_name, subpartition_method, partition_name,
                  partition_method,
                  partition_expression,
                  partition_description
    FROM information_schema.partitions
   WHERE table_schema='~a'
         ~:[~*~;and (~{table_name ~a~^ or ~})~]
         ~:[~*~;and (~{table_name ~a~^ and ~})~]
ORDER BY partition_ordinal_position;