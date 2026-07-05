-- Seed: 18322207684154203625,3181554006726329157

use std.reflection.all;

entity bcm is
  port (hxovhw : inout record_value_mirror);
end bcm;

architecture birdbfmb of bcm is
  
begin
  
end birdbfmb;

library ieee;
use ieee.std_logic_1164.all;

entity naoievln is
  port (zoyvytge : buffer std_logic);
end naoievln;

use std.reflection.all;

architecture s of naoievln is
  shared variable qlb : record_value_mirror;
  shared variable qqnsw : record_value_mirror;
  shared variable wmw : record_value_mirror;
begin
  rxjhbfzx : entity work.bcm
    port map (hxovhw => wmw);
  c : entity work.bcm
    port map (hxovhw => qqnsw);
  dixve : entity work.bcm
    port map (hxovhw => qlb);
  
  -- Multi-driven assignments
  zoyvytge <= '0';
  zoyvytge <= 'W';
end s;



-- Seed after: 1897278897638163888,3181554006726329157
