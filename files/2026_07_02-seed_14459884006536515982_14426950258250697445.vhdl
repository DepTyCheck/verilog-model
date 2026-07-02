-- Seed: 14459884006536515982,14426950258250697445

use std.reflection.all;

entity fxjf is
  port (oug : inout file_subtype_mirror; jw : inout integer_subtype_mirror; dka : linkage boolean_vector(2 to 4));
end fxjf;

architecture ux of fxjf is
  
begin
  
end ux;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nj is
  port (wupgn : inout std_logic; feioolk : inout enumeration_value_mirror);
end nj;

use std.reflection.all;

architecture cjcfbfluz of nj is
  signal bh : boolean_vector(2 to 4);
  shared variable mzgxnkrw : integer_subtype_mirror;
  shared variable sjvot : file_subtype_mirror;
  signal rmtt : boolean_vector(2 to 4);
  shared variable wgvwhn : integer_subtype_mirror;
  shared variable fyvbi : file_subtype_mirror;
  signal jrcmlm : boolean_vector(2 to 4);
  shared variable auidc : integer_subtype_mirror;
  shared variable qcpkiujwfb : file_subtype_mirror;
begin
  w : entity work.fxjf
    port map (oug => qcpkiujwfb, jw => auidc, dka => jrcmlm);
  qkrfawnr : entity work.fxjf
    port map (oug => fyvbi, jw => wgvwhn, dka => rmtt);
  t : entity work.fxjf
    port map (oug => sjvot, jw => mzgxnkrw, dka => bh);
  
  -- Multi-driven assignments
  wupgn <= '1';
  wupgn <= 'U';
end cjcfbfluz;



-- Seed after: 13673405565035804887,14426950258250697445
