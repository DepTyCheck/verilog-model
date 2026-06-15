-- Seed: 4143714949599213194,1834764876137802293

entity kexxcmfc is
  port (pf : linkage boolean_vector(1 to 1));
end kexxcmfc;

architecture xtgwjf of kexxcmfc is
  
begin
  
end xtgwjf;

library ieee;
use ieee.std_logic_1164.all;

entity ziatpcta is
  port (w : linkage std_logic_vector(1 downto 2); vnbbnnvj : linkage boolean);
end ziatpcta;

architecture sg of ziatpcta is
  signal scibvk : boolean_vector(1 to 1);
  signal kqexfcjpo : boolean_vector(1 to 1);
  signal enek : boolean_vector(1 to 1);
begin
  ikzonsoqm : entity work.kexxcmfc
    port map (pf => enek);
  lv : entity work.kexxcmfc
    port map (pf => kqexfcjpo);
  qgvr : entity work.kexxcmfc
    port map (pf => scibvk);
end sg;

entity ugsj is
  port (lxlitsar : out time_vector(4 downto 3));
end ugsj;

library ieee;
use ieee.std_logic_1164.all;

architecture dditcewcfq of ugsj is
  signal l : boolean;
  signal twrftes : std_logic_vector(1 downto 2);
begin
  mbjb : entity work.ziatpcta
    port map (w => twrftes, vnbbnnvj => l);
  
  -- Single-driven assignments
  lxlitsar <= (2#0_1_1_1_0# ms, 0 min);
  
  -- Multi-driven assignments
  twrftes <= (others => '0');
end dditcewcfq;



-- Seed after: 5861370415334676100,1834764876137802293
