-- Seed: 3221208622532976941,15795020531041709203

entity lfee is
  port (s : inout real; ektrybfci : inout time_vector(3 to 1));
end lfee;

architecture lu of lfee is
  
begin
  -- Single-driven assignments
  ektrybfci <= ektrybfci;
  s <= s;
end lu;



-- Seed after: 16565756141758083241,15795020531041709203
