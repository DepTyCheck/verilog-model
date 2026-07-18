-- Seed: 12605655971445960754,1112937151005418631

entity l is
  port (p : buffer severity_level; c : out severity_level; yiaqb : linkage boolean);
end l;

architecture t of l is
  
begin
  -- Single-driven assignments
  c <= c;
  p <= FAILURE;
end t;



-- Seed after: 946283653422585883,1112937151005418631
