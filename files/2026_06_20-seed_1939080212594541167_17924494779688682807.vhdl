-- Seed: 1939080212594541167,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity xoayus is
  port (xnvbi : linkage time; ir : out std_logic);
end xoayus;

architecture mtou of xoayus is
  
begin
  -- Multi-driven assignments
  ir <= '1';
  ir <= 'W';
  ir <= 'W';
end mtou;

entity aznrzb is
  port (elusztyd : linkage time);
end aznrzb;

library ieee;
use ieee.std_logic_1164.all;

architecture dtijwqd of aznrzb is
  signal jphh : time;
  signal tuekrvfxz : time;
  signal luazkddf : std_logic;
  signal wjyx : time;
begin
  rnkelu : entity work.xoayus
    port map (xnvbi => wjyx, ir => luazkddf);
  zxlsorzpgz : entity work.xoayus
    port map (xnvbi => tuekrvfxz, ir => luazkddf);
  kaeahmetu : entity work.xoayus
    port map (xnvbi => jphh, ir => luazkddf);
  imyxpsmad : entity work.xoayus
    port map (xnvbi => elusztyd, ir => luazkddf);
  
  -- Multi-driven assignments
  luazkddf <= '-';
  luazkddf <= 'H';
end dtijwqd;



-- Seed after: 3513328812942867570,17924494779688682807
