-- Seed: 15628014186692684627,6290177331721581829

use std.reflection.all;

entity p is
  port (ttlpq : inout subtype_mirror; y : inout protected_subtype_mirror; hg : buffer time);
end p;

architecture esa of p is
  
begin
  -- Single-driven assignments
  hg <= 16#B9A0# us;
end esa;



-- Seed after: 9493120535091312535,6290177331721581829
