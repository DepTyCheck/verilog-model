-- Seed: 6929816550732797445,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity arvvi is
  port (fbbf : out std_logic_vector(0 to 3); tgxbavp : linkage std_logic_vector(3 downto 3); be : buffer severity_level; kcjepebuig : inout bit);
end arvvi;

architecture ysyivgcat of arvvi is
  
begin
  -- Single-driven assignments
  be <= FAILURE;
  kcjepebuig <= '0';
end ysyivgcat;

entity t is
  port (evvbg : in boolean; ylivbn : inout real);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture jfmcswjzz of t is
  signal bfutu : bit;
  signal njsktj : severity_level;
  signal oyr : std_logic_vector(3 downto 3);
  signal djwogxwgbd : bit;
  signal swoxytl : severity_level;
  signal hzyfwiak : std_logic_vector(3 downto 3);
  signal bvofe : std_logic_vector(0 to 3);
begin
  grzmzm : entity work.arvvi
    port map (fbbf => bvofe, tgxbavp => hzyfwiak, be => swoxytl, kcjepebuig => djwogxwgbd);
  x : entity work.arvvi
    port map (fbbf => bvofe, tgxbavp => oyr, be => njsktj, kcjepebuig => bfutu);
  
  -- Single-driven assignments
  ylivbn <= ylivbn;
end jfmcswjzz;

library ieee;
use ieee.std_logic_1164.all;

entity rc is
  port (f : out time; bqh : out std_logic_vector(0 downto 0));
end rc;

library ieee;
use ieee.std_logic_1164.all;

architecture bhwsldwz of rc is
  signal l : bit;
  signal v : severity_level;
  signal udiwl : std_logic_vector(3 downto 3);
  signal iytxyicaep : std_logic_vector(0 to 3);
begin
  jnfnvhyx : entity work.arvvi
    port map (fbbf => iytxyicaep, tgxbavp => udiwl, be => v, kcjepebuig => l);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  udiwl <= bqh;
end bhwsldwz;



-- Seed after: 7193444732108072328,6377581586043967497
