-- Seed: 16138306101016663788,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zao is
  port (variable mz : inout enumeration_value_mirror_pt; kgdvcuvrw : inout std_logic_vector(0 to 3));
end zao;

architecture kpqtkzwnh of zao is
  
begin
  -- Multi-driven assignments
  kgdvcuvrw <= "HLU0";
  kgdvcuvrw <= "UWHZ";
  kgdvcuvrw <= kgdvcuvrw;
end kpqtkzwnh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ex is
  port (variable oo : inout subtype_mirror_pt; yozt : linkage time_vector(3 to 2); r : buffer std_logic_vector(1 downto 0));
end ex;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture aw of ex is
  signal lusnvfm : std_logic_vector(0 to 3);
  shared variable occedy : enumeration_value_mirror_pt;
  signal bvnn : std_logic_vector(0 to 3);
  shared variable aigwn : enumeration_value_mirror_pt;
  shared variable ora : enumeration_value_mirror_pt;
  signal w : std_logic_vector(0 to 3);
  shared variable pmomsyb : enumeration_value_mirror_pt;
begin
  mumx : entity work.zao
    port map (mz => pmomsyb, kgdvcuvrw => w);
  iqhfarf : entity work.zao
    port map (mz => ora, kgdvcuvrw => w);
  rlbuddqpun : entity work.zao
    port map (mz => aigwn, kgdvcuvrw => bvnn);
  vdtzmakknn : entity work.zao
    port map (mz => occedy, kgdvcuvrw => lusnvfm);
  
  -- Multi-driven assignments
  r <= "X-";
  r <= r;
  r <= "0Z";
  w <= w;
end aw;



-- Seed after: 3436357565821718538,2983771601630957889
