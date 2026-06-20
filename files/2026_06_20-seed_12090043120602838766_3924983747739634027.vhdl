-- Seed: 12090043120602838766,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity cyjrb is
  port (dmcttqmmro : buffer std_logic_vector(0 downto 0); whzbs : buffer integer_vector(2 to 0));
end cyjrb;

architecture pniwl of cyjrb is
  
begin
  -- Multi-driven assignments
  dmcttqmmro <= (others => '-');
end pniwl;

library ieee;
use ieee.std_logic_1164.all;

entity zkhinzt is
  port (risqfgbkmr : inout std_logic; wbzsgsif : in std_logic; n : buffer std_logic_vector(1 to 2); dzttghhnmx : in integer);
end zkhinzt;

library ieee;
use ieee.std_logic_1164.all;

architecture nncrnfo of zkhinzt is
  signal xiu : integer_vector(2 to 0);
  signal bh : std_logic_vector(0 downto 0);
  signal crsjnwqaaq : integer_vector(2 to 0);
  signal xtzlcz : std_logic_vector(0 downto 0);
begin
  loboej : entity work.cyjrb
    port map (dmcttqmmro => xtzlcz, whzbs => crsjnwqaaq);
  urisorjttv : entity work.cyjrb
    port map (dmcttqmmro => bh, whzbs => xiu);
  
  -- Multi-driven assignments
  n <= ('X', '0');
end nncrnfo;



-- Seed after: 2471805102461322900,3924983747739634027
