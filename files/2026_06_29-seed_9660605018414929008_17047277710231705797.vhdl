-- Seed: 9660605018414929008,17047277710231705797

entity ffqmvtk is
  port (hf : buffer time);
end ffqmvtk;

architecture d of ffqmvtk is
  
begin
  -- Single-driven assignments
  hf <= 2#11100.0_0_1_0# ms;
end d;



-- Seed after: 12294726550450250756,17047277710231705797
