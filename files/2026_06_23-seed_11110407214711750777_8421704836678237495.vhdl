-- Seed: 11110407214711750777,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity sstwgth is
  port (bx : buffer std_logic; ztg : linkage std_logic; yndbonf : out time);
end sstwgth;

architecture rarfwozxnh of sstwgth is
  
begin
  -- Single-driven assignments
  yndbonf <= 8#253.1_1# ns;
  
  -- Multi-driven assignments
  bx <= '-';
  bx <= 'U';
  bx <= '-';
end rarfwozxnh;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (evmjlztoaj : in std_logic_vector(4 to 0); suny : linkage time);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture wj of d is
  signal elxshhiatv : time;
  signal xucsozd : time;
  signal fk : std_logic;
  signal ufvc : time;
  signal x : time;
  signal odijps : std_logic;
begin
  zaiuk : entity work.sstwgth
    port map (bx => odijps, ztg => odijps, yndbonf => x);
  zgjrsk : entity work.sstwgth
    port map (bx => odijps, ztg => odijps, yndbonf => ufvc);
  vghvxlk : entity work.sstwgth
    port map (bx => odijps, ztg => fk, yndbonf => xucsozd);
  gxxr : entity work.sstwgth
    port map (bx => fk, ztg => odijps, yndbonf => elxshhiatv);
  
  -- Multi-driven assignments
  fk <= '1';
  odijps <= 'X';
  odijps <= 'U';
  odijps <= 'X';
end wj;

library ieee;
use ieee.std_logic_1164.all;

entity ngxfint is
  port (skm : out time_vector(3 downto 4); yysgcnqkvq : buffer std_logic_vector(3 to 2));
end ngxfint;

library ieee;
use ieee.std_logic_1164.all;

architecture baaqb of ngxfint is
  signal vwi : time;
  signal ueoltnkap : std_logic;
begin
  ieom : entity work.sstwgth
    port map (bx => ueoltnkap, ztg => ueoltnkap, yndbonf => vwi);
  
  -- Single-driven assignments
  skm <= (others => 0 ns);
end baaqb;

library ieee;
use ieee.std_logic_1164.all;

entity zikbavzsv is
  port (lawkwfzqi : out std_logic_vector(2 downto 1); ds : in time; lerk : in time);
end zikbavzsv;

library ieee;
use ieee.std_logic_1164.all;

architecture b of zikbavzsv is
  signal ffujlvovku : time;
  signal kzx : std_logic;
  signal qbvm : time;
  signal kfwsi : std_logic;
  signal kqpdqvg : std_logic_vector(3 to 2);
  signal biphvfc : time_vector(3 downto 4);
begin
  hcuvgjvgpj : entity work.ngxfint
    port map (skm => biphvfc, yysgcnqkvq => kqpdqvg);
  jaxvmxf : entity work.sstwgth
    port map (bx => kfwsi, ztg => kfwsi, yndbonf => qbvm);
  gc : entity work.sstwgth
    port map (bx => kzx, ztg => kfwsi, yndbonf => ffujlvovku);
  
  -- Multi-driven assignments
  lawkwfzqi <= "10";
  kzx <= 'H';
  lawkwfzqi <= ('-', 'L');
  kqpdqvg <= (others => '0');
end b;



-- Seed after: 2253168875077869333,8421704836678237495
