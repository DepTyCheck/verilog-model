-- Seed: 6649411533945399589,7726014785203345639

use std.reflection.all;

entity l is
  port (aa : inout floating_subtype_mirror; xh : in time);
end l;

architecture eysouesvs of l is
  
begin
  
end eysouesvs;

use std.reflection.all;

entity c is
  port (lnhkr : linkage bit; jmjfyhjb : inout floating_value_mirror);
end c;

use std.reflection.all;

architecture vorphqxx of c is
  signal ppmngmvc : time;
  shared variable adtm : floating_subtype_mirror;
  signal kodazw : time;
  shared variable cycmjdvhzg : floating_subtype_mirror;
begin
  eqpp : entity work.l
    port map (aa => cycmjdvhzg, xh => kodazw);
  blrk : entity work.l
    port map (aa => adtm, xh => ppmngmvc);
  
  -- Single-driven assignments
  kodazw <= 12.23141 ns;
  ppmngmvc <= kodazw;
end vorphqxx;



-- Seed after: 11512486070340852684,7726014785203345639
