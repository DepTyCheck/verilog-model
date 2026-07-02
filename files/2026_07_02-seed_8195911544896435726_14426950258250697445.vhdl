-- Seed: 8195911544896435726,14426950258250697445

use std.reflection.all;

entity tnscrb is
  port (p : inout integer; h : inout enumeration_value_mirror; mnxal : out integer);
end tnscrb;

architecture kr of tnscrb is
  
begin
  -- Single-driven assignments
  p <= 16#F59#;
  mnxal <= p;
end kr;



-- Seed after: 8855422959429213468,14426950258250697445
