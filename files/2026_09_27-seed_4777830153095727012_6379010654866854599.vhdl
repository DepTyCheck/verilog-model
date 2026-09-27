-- Seed: 4777830153095727012,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity osz is
  port (z : in std_logic_vector(0 to 3); jpwfw : inout std_logic_vector(3 downto 0); lcsjndyvs : in bit);
end osz;

architecture bma of osz is
  
begin
  -- Multi-driven assignments
  jpwfw <= ('W', 'H', '-', 'W');
  jpwfw <= jpwfw;
  jpwfw <= jpwfw;
  jpwfw <= ('X', 'Z', 'H', 'H');
end bma;

entity cbdepp is
  port (lbndzxu : out bit_vector(2 downto 4));
end cbdepp;

library ieee;
use ieee.std_logic_1164.all;

architecture nlvhf of cbdepp is
  signal cdzopfxijk : bit;
  signal nbh : std_logic_vector(3 downto 0);
begin
  yzv : entity work.osz
    port map (z => nbh, jpwfw => nbh, lcsjndyvs => cdzopfxijk);
  
  -- Single-driven assignments
  lbndzxu <= (others => '0');
  cdzopfxijk <= cdzopfxijk;
  
  -- Multi-driven assignments
  nbh <= nbh;
  nbh <= nbh;
  nbh <= "U-XZ";
  nbh <= ('W', 'H', 'W', 'Z');
end nlvhf;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (v : buffer std_logic_vector(2 to 1));
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture i of a is
  signal igiyblk : std_logic_vector(0 to 3);
  signal ufe : bit_vector(2 downto 4);
  signal xlg : bit;
  signal xyplaqlz : std_logic_vector(3 downto 0);
  signal cbykny : std_logic_vector(3 downto 0);
begin
  rroyv : entity work.osz
    port map (z => cbykny, jpwfw => xyplaqlz, lcsjndyvs => xlg);
  xskcws : entity work.cbdepp
    port map (lbndzxu => ufe);
  dxvycoeut : entity work.osz
    port map (z => cbykny, jpwfw => igiyblk, lcsjndyvs => xlg);
  tydr : entity work.osz
    port map (z => igiyblk, jpwfw => cbykny, lcsjndyvs => xlg);
  
  -- Single-driven assignments
  xlg <= '0';
  
  -- Multi-driven assignments
  xyplaqlz <= ('H', 'L', 'Z', '-');
  xyplaqlz <= xyplaqlz;
  cbykny <= ('X', 'L', '1', 'W');
  igiyblk <= cbykny;
end i;



-- Seed after: 2800498762862388661,6379010654866854599
