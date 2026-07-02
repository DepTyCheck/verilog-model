-- Seed: 2805121447772771492,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity svusie is
  port (b : in std_logic_vector(4 to 4); t : inout physical_value_mirror);
end svusie;

architecture hevng of svusie is
  
begin
  
end hevng;

use std.reflection.all;

entity gxez is
  port (m : inout protected_value_mirror; bv : inout protected_subtype_mirror; tprtex : inout integer; xqpqok : inout floating_subtype_mirror);
end gxez;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xozeup of gxez is
  shared variable e : physical_value_mirror;
  signal vfmu : std_logic_vector(4 to 4);
begin
  n : entity work.svusie
    port map (b => vfmu, t => e);
  
  -- Single-driven assignments
  tprtex <= 8#3_3_2_5_0#;
end xozeup;

use std.reflection.all;

entity v is
  port (y : out bit; iu : inout file_subtype_mirror);
end v;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture aoydcopd of v is
  shared variable pvlbk : physical_value_mirror;
  shared variable zfksnuzt : physical_value_mirror;
  signal pluhaydcj : std_logic_vector(4 to 4);
  shared variable xflyjk : physical_value_mirror;
  shared variable tbudst : physical_value_mirror;
  signal ne : std_logic_vector(4 to 4);
begin
  r : entity work.svusie
    port map (b => ne, t => tbudst);
  d : entity work.svusie
    port map (b => ne, t => xflyjk);
  c : entity work.svusie
    port map (b => pluhaydcj, t => zfksnuzt);
  bwneblcouk : entity work.svusie
    port map (b => ne, t => pvlbk);
  
  -- Single-driven assignments
  y <= '0';
  
  -- Multi-driven assignments
  pluhaydcj <= "-";
  pluhaydcj <= "L";
  pluhaydcj <= ne;
  ne <= "1";
end aoydcopd;



-- Seed after: 946385479059715815,14426950258250697445
