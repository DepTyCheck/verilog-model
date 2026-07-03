-- Seed: 18046396350955284180,2158184632809654795

use std.reflection.all;

entity uslle is
  port (yakcb : inout enumeration_subtype_mirror);
end uslle;

architecture gtfetwmsq of uslle is
  
begin
  
end gtfetwmsq;

use std.reflection.all;

entity pkggqmb is
  port (fyaqbwis : inout value_mirror);
end pkggqmb;

use std.reflection.all;

architecture mvt of pkggqmb is
  shared variable fqchvtxap : enumeration_subtype_mirror;
  shared variable oc : enumeration_subtype_mirror;
  shared variable igoy : enumeration_subtype_mirror;
  shared variable vgdsokvh : enumeration_subtype_mirror;
begin
  qdv : entity work.uslle
    port map (yakcb => vgdsokvh);
  nyrqwa : entity work.uslle
    port map (yakcb => igoy);
  n : entity work.uslle
    port map (yakcb => oc);
  mz : entity work.uslle
    port map (yakcb => fqchvtxap);
end mvt;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity f is
  port (gzn : buffer std_logic; sct : inout access_subtype_mirror; ixobmld : out integer);
end f;

use std.reflection.all;

architecture xfxwjv of f is
  shared variable gu : enumeration_subtype_mirror;
  shared variable lqxfejgjm : value_mirror;
begin
  m : entity work.pkggqmb
    port map (fyaqbwis => lqxfejgjm);
  x : entity work.uslle
    port map (yakcb => gu);
  
  -- Single-driven assignments
  ixobmld <= ixobmld;
  
  -- Multi-driven assignments
  gzn <= gzn;
end xfxwjv;



-- Seed after: 12624998434994918500,2158184632809654795
