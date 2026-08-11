-- Seed: 4300641303729808294,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity brfahxzi is
  port (mylaxnrv : in time; bbyuvc : buffer std_logic_vector(0 downto 2));
end brfahxzi;

architecture drki of brfahxzi is
  
begin
  -- Multi-driven assignments
  bbyuvc <= (others => '0');
end drki;

library ieee;
use ieee.std_logic_1164.all;

entity fiiqriix is
  port (eii : in std_logic_vector(3 downto 4));
end fiiqriix;

architecture ecqyab of fiiqriix is
  
begin
  
end ecqyab;

library ieee;
use ieee.std_logic_1164.all;

entity fthov is
  port (bps : linkage real; pqtgfpkboc : linkage boolean; bzn : out std_logic_vector(2 downto 0); qavmtw : linkage std_logic_vector(2 downto 3));
end fthov;

library ieee;
use ieee.std_logic_1164.all;

architecture mullpn of fthov is
  signal ljexagrxjo : std_logic_vector(3 downto 4);
  signal kpyz : time;
begin
  nduuq : entity work.brfahxzi
    port map (mylaxnrv => kpyz, bbyuvc => ljexagrxjo);
  jfkexqn : entity work.fiiqriix
    port map (eii => ljexagrxjo);
  
  -- Single-driven assignments
  kpyz <= 4_0_4.022 ns;
  
  -- Multi-driven assignments
  bzn <= ('1', '1', 'U');
  bzn <= ('Z', 'L', '1');
  bzn <= bzn;
  ljexagrxjo <= ljexagrxjo;
end mullpn;



-- Seed after: 16260881546474623815,10594830431004325987
