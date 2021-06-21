  SELECT DISTINCT table_name, subpartition_method, partition_name,
                  partition_method,
                  partition_expression,
                  partition_description
    FROM information_schema.partitions
   WHERE table_schema='~a'
GROUP BY table_name
ORDER BY partition_ordinal_position;