-- Seed: 4808200738721201222,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bsowau is
  port (jxhhz : inout file_value_mirror; vzmxbkj : inout std_logic_vector(3 to 3));
end bsowau;

architecture yxbijvhx of bsowau is
  
begin
  -- Multi-driven assignments
  vzmxbkj <= (others => 'U');
  vzmxbkj <= (others => '0');
end yxbijvhx;

use std.reflection.all;

entity afsim is
  port (mherckvrzp : buffer integer; frc : inout record_value_mirror);
end afsim;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture avwaxz of afsim is
  shared variable qip : file_value_mirror;
  signal yohz : std_logic_vector(3 to 3);
  shared variable davbwl : file_value_mirror;
begin
  rushsza : entity work.bsowau
    port map (jxhhz => davbwl, vzmxbkj => yohz);
  zdvxfzj : entity work.bsowau
    port map (jxhhz => qip, vzmxbkj => yohz);
  
  -- Single-driven assignments
  mherckvrzp <= mherckvrzp;
  
  -- Multi-driven assignments
  yohz <= yohz;
  yohz <= "0";
  yohz <= yohz;
end avwaxz;



-- Seed after: 18337747761521383103,7726014785203345639
