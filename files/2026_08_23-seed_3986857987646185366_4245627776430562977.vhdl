-- Seed: 3986857987646185366,4245627776430562977

entity yvkaxuqra is
  port (lf : buffer real_vector(1 to 3));
end yvkaxuqra;

architecture nkb of yvkaxuqra is
  
begin
  -- Single-driven assignments
  lf <= (8#2_0_7_3.6_7_1#, 4_2_2_3.4_0_1_4, 0144.22432);
end nkb;

library ieee;
use ieee.std_logic_1164.all;

entity yrhkcg is
  port (xxco : buffer std_logic_vector(0 to 2); m : buffer time; bkyllibnmr : in integer; ddjt : in time);
end yrhkcg;

architecture pslta of yrhkcg is
  signal szj : real_vector(1 to 3);
  signal gzmhhqjhe : real_vector(1 to 3);
  signal bxshf : real_vector(1 to 3);
  signal debeorenyj : real_vector(1 to 3);
begin
  ab : entity work.yvkaxuqra
    port map (lf => debeorenyj);
  mjs : entity work.yvkaxuqra
    port map (lf => bxshf);
  nmlfflmv : entity work.yvkaxuqra
    port map (lf => gzmhhqjhe);
  zfxlfmyp : entity work.yvkaxuqra
    port map (lf => szj);
  
  -- Single-driven assignments
  m <= 8#6_0_7# ms;
  
  -- Multi-driven assignments
  xxco <= "1-Z";
end pslta;

library ieee;
use ieee.std_logic_1164.all;

entity djaomf is
  port (x : inout severity_level; degfzkbl : out time; vk : buffer std_logic; v : buffer integer);
end djaomf;

architecture ecwpx of djaomf is
  signal ghsqxkp : real_vector(1 to 3);
  signal c : real_vector(1 to 3);
begin
  ifj : entity work.yvkaxuqra
    port map (lf => c);
  s : entity work.yvkaxuqra
    port map (lf => ghsqxkp);
end ecwpx;



-- Seed after: 7612498201742903570,4245627776430562977
