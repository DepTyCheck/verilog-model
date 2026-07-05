-- Seed: 4596230734589650595,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ggidn is
  port (vlnxs : inout floating_subtype_mirror; gz : in std_logic);
end ggidn;

architecture wolyyv of ggidn is
  
begin
  
end wolyyv;

use std.reflection.all;

entity cjp is
  port (cbrgxopa : inout record_subtype_mirror; skvx : linkage time_vector(0 to 0));
end cjp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture banapykm of cjp is
  signal jbxofgi : std_logic;
  shared variable v : floating_subtype_mirror;
begin
  iphwavjzx : entity work.ggidn
    port map (vlnxs => v, gz => jbxofgi);
end banapykm;

entity bm is
  port (kbvfn : inout time);
end bm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wh of bm is
  shared variable tiwlzori : floating_subtype_mirror;
  shared variable mzrhuswmw : floating_subtype_mirror;
  signal ignpesyooe : std_logic;
  shared variable cvbtkbxa : floating_subtype_mirror;
begin
  edehjlnvkk : entity work.ggidn
    port map (vlnxs => cvbtkbxa, gz => ignpesyooe);
  kctbji : entity work.ggidn
    port map (vlnxs => mzrhuswmw, gz => ignpesyooe);
  lz : entity work.ggidn
    port map (vlnxs => tiwlzori, gz => ignpesyooe);
  
  -- Single-driven assignments
  kbvfn <= 440 us;
  
  -- Multi-driven assignments
  ignpesyooe <= 'W';
  ignpesyooe <= 'U';
  ignpesyooe <= ignpesyooe;
end wh;



-- Seed after: 17542038131759437203,3181554006726329157
