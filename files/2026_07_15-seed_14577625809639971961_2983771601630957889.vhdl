-- Seed: 14577625809639971961,2983771601630957889

use std.reflection.all;

entity u is
  port (variable afjhys : inout access_value_mirror_pt);
end u;

architecture p of u is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vujfwfb is
  port (variable nzt : inout record_subtype_mirror_pt; tef : out std_logic; wflrisovhm : linkage time; oqki : in std_logic);
end vujfwfb;

use std.reflection.all;

architecture rygbzhkbk of vujfwfb is
  shared variable vcnhnkkaz : access_value_mirror_pt;
  shared variable mwxo : access_value_mirror_pt;
begin
  fhhhrjwq : entity work.u
    port map (afjhys => mwxo);
  sy : entity work.u
    port map (afjhys => vcnhnkkaz);
  
  -- Multi-driven assignments
  tef <= '0';
  tef <= oqki;
end rygbzhkbk;

library ieee;
use ieee.std_logic_1164.all;

entity tnnmnn is
  port (wjemaxycz : inout boolean_vector(4 to 1); kdrwihkazy : out std_logic_vector(2 downto 0));
end tnnmnn;

use std.reflection.all;

architecture ihrc of tnnmnn is
  shared variable tvcy : access_value_mirror_pt;
  shared variable wilhmw : access_value_mirror_pt;
  shared variable wxzqdmc : access_value_mirror_pt;
begin
  sgnb : entity work.u
    port map (afjhys => wxzqdmc);
  yiw : entity work.u
    port map (afjhys => wilhmw);
  wvztdb : entity work.u
    port map (afjhys => tvcy);
  
  -- Single-driven assignments
  wjemaxycz <= (others => TRUE);
end ihrc;

use std.reflection.all;

entity teft is
  port (variable ghiagdj : inout integer_subtype_mirror_pt; variable ulthn : inout floating_value_mirror_pt);
end teft;

use std.reflection.all;

architecture gwfmr of teft is
  shared variable qh : access_value_mirror_pt;
  shared variable vipvtlu : access_value_mirror_pt;
begin
  caeqklfx : entity work.u
    port map (afjhys => vipvtlu);
  wx : entity work.u
    port map (afjhys => qh);
end gwfmr;



-- Seed after: 9331297453791871362,2983771601630957889
