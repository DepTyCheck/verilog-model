-- Seed: 10578666637095744876,6290177331721581829

use std.reflection.all;

entity thl is
  port (g : linkage string(3 to 2); uf : inout floating_value_mirror; sqhhzsses : buffer integer);
end thl;

architecture k of thl is
  
begin
  -- Single-driven assignments
  sqhhzsses <= 1_2;
end k;

use std.reflection.all;

entity nhi is
  port (v : inout integer; y : inout value_mirror; ovsqhcr : out time_vector(0 to 0));
end nhi;

use std.reflection.all;

architecture itnwg of nhi is
  shared variable xsshmu : floating_value_mirror;
  signal bn : string(3 to 2);
begin
  wclylfjz : entity work.thl
    port map (g => bn, uf => xsshmu, sqhhzsses => v);
  
  -- Single-driven assignments
  ovsqhcr <= (others => 4 ps);
end itnwg;



-- Seed after: 12933119490724720401,6290177331721581829
