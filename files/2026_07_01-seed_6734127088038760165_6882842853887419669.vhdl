-- Seed: 6734127088038760165,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity zfb is
  port (zqnjzmt : linkage std_logic_vector(1 to 2); e : out std_logic; ew : linkage boolean);
end zfb;

architecture o of zfb is
  
begin
  -- Multi-driven assignments
  e <= 'W';
  e <= '1';
  e <= 'W';
end o;

entity rnccn is
  port (jk : inout real_vector(1 downto 3); ifhw : inout time);
end rnccn;

library ieee;
use ieee.std_logic_1164.all;

architecture j of rnccn is
  signal eukfwqkkco : boolean;
  signal achxnbf : std_logic_vector(1 to 2);
  signal ntsojy : boolean;
  signal kli : std_logic;
  signal qfqopdi : boolean;
  signal lu : std_logic;
  signal as : std_logic_vector(1 to 2);
begin
  hdyyjwlub : entity work.zfb
    port map (zqnjzmt => as, e => lu, ew => qfqopdi);
  yifbqwv : entity work.zfb
    port map (zqnjzmt => as, e => kli, ew => ntsojy);
  y : entity work.zfb
    port map (zqnjzmt => achxnbf, e => kli, ew => eukfwqkkco);
end j;

library ieee;
use ieee.std_logic_1164.all;

entity yweqkun is
  port (sk : in std_logic_vector(3 downto 0));
end yweqkun;

library ieee;
use ieee.std_logic_1164.all;

architecture gqq of yweqkun is
  signal qjborzamca : boolean;
  signal iju : std_logic;
  signal ozs : std_logic_vector(1 to 2);
begin
  wxfygd : entity work.zfb
    port map (zqnjzmt => ozs, e => iju, ew => qjborzamca);
end gqq;



-- Seed after: 5142620438186736176,6882842853887419669
