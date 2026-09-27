-- Seed: 4059236166618958797,6379010654866854599

entity kr is
  port (rbs : in time; sy : inout time);
end kr;

architecture u of kr is
  
begin
  -- Single-driven assignments
  sy <= 24 us;
end u;



-- Seed after: 14097026286355987230,6379010654866854599
