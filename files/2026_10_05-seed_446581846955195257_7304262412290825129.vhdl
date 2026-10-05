-- Seed: 446581846955195257,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity exkutv is
  port (ldiptmtlt : linkage std_logic_vector(2 to 4); yglats : in std_logic_vector(3 downto 3); yzf : buffer std_logic; pah : linkage std_logic);
end exkutv;

architecture srusjbko of exkutv is
  
begin
  -- Multi-driven assignments
  yzf <= yzf;
  yzf <= '0';
  yzf <= 'U';
end srusjbko;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (j : inout std_logic; sukdypj : out std_logic);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture n of g is
  signal wtiewj : std_logic;
  signal eih : std_logic_vector(3 downto 3);
  signal gfvuyspav : std_logic_vector(2 to 4);
begin
  p : entity work.exkutv
    port map (ldiptmtlt => gfvuyspav, yglats => eih, yzf => wtiewj, pah => j);
  jggjjtw : entity work.exkutv
    port map (ldiptmtlt => gfvuyspav, yglats => eih, yzf => wtiewj, pah => sukdypj);
  
  -- Multi-driven assignments
  sukdypj <= sukdypj;
  eih <= eih;
end n;



-- Seed after: 16948603226192814065,7304262412290825129
