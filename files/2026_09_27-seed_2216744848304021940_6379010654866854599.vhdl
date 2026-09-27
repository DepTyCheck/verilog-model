-- Seed: 2216744848304021940,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity rjybfv is
  port (cejj : linkage std_logic_vector(3 downto 1); qvlq : inout std_logic; hvljcpem : inout bit);
end rjybfv;

architecture lu of rjybfv is
  
begin
  -- Single-driven assignments
  hvljcpem <= '0';
  
  -- Multi-driven assignments
  qvlq <= qvlq;
end lu;

entity jugy is
  port (oz : inout integer);
end jugy;

library ieee;
use ieee.std_logic_1164.all;

architecture hlwglhrvdo of jugy is
  signal vqgp : bit;
  signal i : std_logic;
  signal dcrejauxn : std_logic_vector(3 downto 1);
  signal kgy : bit;
  signal slf : std_logic;
  signal wmmjtkyr : bit;
  signal hiaywe : std_logic;
  signal ba : bit;
  signal gpneobc : std_logic;
  signal hqnarzvyx : std_logic_vector(3 downto 1);
begin
  fkyap : entity work.rjybfv
    port map (cejj => hqnarzvyx, qvlq => gpneobc, hvljcpem => ba);
  p : entity work.rjybfv
    port map (cejj => hqnarzvyx, qvlq => hiaywe, hvljcpem => wmmjtkyr);
  tuvsnzj : entity work.rjybfv
    port map (cejj => hqnarzvyx, qvlq => slf, hvljcpem => kgy);
  diokzb : entity work.rjybfv
    port map (cejj => dcrejauxn, qvlq => i, hvljcpem => vqgp);
  
  -- Single-driven assignments
  oz <= 2#0_0_0_1_1#;
end hlwglhrvdo;



-- Seed after: 6823597174370493239,6379010654866854599
