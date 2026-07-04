-- Seed: 18328840262046728322,6290177331721581829

use std.reflection.all;

entity r is
  port (od : inout file_subtype_mirror; smk : buffer time; yhdb : in integer; x : inout integer_subtype_mirror);
end r;

architecture wfsg of r is
  
begin
  -- Single-driven assignments
  smk <= 4_0 ps;
end wfsg;



-- Seed after: 17737479474870879266,6290177331721581829
