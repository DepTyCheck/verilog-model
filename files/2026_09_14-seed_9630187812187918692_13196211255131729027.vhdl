-- Seed: 9630187812187918692,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity dekppnm is
  port (vx : out time; zrgmrug : buffer time_vector(0 downto 0); wmijcjt : linkage std_logic; txjfv : out std_logic);
end dekppnm;

architecture j of dekppnm is
  
begin
  -- Single-driven assignments
  vx <= 0.3_1 fs;
  
  -- Multi-driven assignments
  txjfv <= '1';
  txjfv <= 'Z';
end j;

library ieee;
use ieee.std_logic_1164.all;

entity obn is
  port (xa : buffer bit; wex : inout bit_vector(2 downto 3); lwrgjib : inout std_logic_vector(2 downto 1));
end obn;

library ieee;
use ieee.std_logic_1164.all;

architecture pyn of obn is
  signal tgxjo : std_logic;
  signal aurdynf : time_vector(0 downto 0);
  signal qfg : time;
begin
  plu : entity work.dekppnm
    port map (vx => qfg, zrgmrug => aurdynf, wmijcjt => tgxjo, txjfv => tgxjo);
  
  -- Single-driven assignments
  wex <= (others => '0');
  xa <= '1';
  
  -- Multi-driven assignments
  lwrgjib <= ('H', 'X');
  lwrgjib <= ('0', 'U');
end pyn;

library ieee;
use ieee.std_logic_1164.all;

entity pgjsxdefz is
  port (iwgugffbw : linkage std_logic_vector(2 to 0); ywe : out time);
end pgjsxdefz;

architecture ozv of pgjsxdefz is
  
begin
  -- Single-driven assignments
  ywe <= 8#5746.2362# ps;
end ozv;



-- Seed after: 15605532150237242689,13196211255131729027
