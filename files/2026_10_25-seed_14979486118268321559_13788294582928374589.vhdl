-- Seed: 14979486118268321559,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity hgrl is
  port (h : out integer_vector(0 downto 3); xs : inout std_logic; pm : in time; oqjvc : inout std_logic);
end hgrl;

architecture oggrmwdynd of hgrl is
  
begin
  -- Multi-driven assignments
  oqjvc <= 'Z';
  oqjvc <= oqjvc;
  xs <= oqjvc;
end oggrmwdynd;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (wz : out time_vector(1 to 1); pgugvp : in integer; pov : buffer std_logic; lmmal : buffer std_logic_vector(4 downto 1));
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture bqnkmitr of c is
  signal fkxcdiaca : std_logic;
  signal zh : time;
  signal faiwf : std_logic;
  signal ijfsmpbnr : integer_vector(0 downto 3);
  signal lloerrqwlk : time;
  signal nqyjrkerge : integer_vector(0 downto 3);
  signal xsuumn : integer_vector(0 downto 3);
  signal bmwsxpbrz : std_logic;
  signal ufkoklwoqw : time;
  signal urq : std_logic;
  signal ydsdruwjo : integer_vector(0 downto 3);
begin
  iy : entity work.hgrl
    port map (h => ydsdruwjo, xs => urq, pm => ufkoklwoqw, oqjvc => bmwsxpbrz);
  qqecbpgft : entity work.hgrl
    port map (h => xsuumn, xs => urq, pm => ufkoklwoqw, oqjvc => pov);
  tqs : entity work.hgrl
    port map (h => nqyjrkerge, xs => pov, pm => lloerrqwlk, oqjvc => pov);
  chejifgkbm : entity work.hgrl
    port map (h => ijfsmpbnr, xs => faiwf, pm => zh, oqjvc => fkxcdiaca);
  
  -- Single-driven assignments
  zh <= 1_3_3 fs;
  wz <= wz;
  ufkoklwoqw <= zh;
  
  -- Multi-driven assignments
  faiwf <= pov;
end bqnkmitr;

library ieee;
use ieee.std_logic_1164.all;

entity tspogati is
  port (b : out std_logic_vector(4 downto 1); gwya : buffer time; amzag : in integer; n : out integer_vector(1 downto 2));
end tspogati;

library ieee;
use ieee.std_logic_1164.all;

architecture f of tspogati is
  signal aqy : std_logic;
  signal igbsnlej : std_logic;
begin
  byi : entity work.hgrl
    port map (h => n, xs => igbsnlej, pm => gwya, oqjvc => aqy);
  
  -- Single-driven assignments
  gwya <= 2#0_1_0# ns;
  
  -- Multi-driven assignments
  aqy <= 'W';
end f;



-- Seed after: 845899814816593536,13788294582928374589
