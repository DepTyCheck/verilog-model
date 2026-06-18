-- Seed: 18295607217032406331,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity pmwoba is
  port (uekcz : linkage std_logic_vector(0 to 3); pvruvsbgaa : in real; tcfjollzi : buffer std_logic_vector(2 downto 1));
end pmwoba;

architecture vboawtp of pmwoba is
  
begin
  -- Multi-driven assignments
  tcfjollzi <= ('H', '1');
  tcfjollzi <= "HW";
  tcfjollzi <= "ZX";
end vboawtp;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (soxnm : buffer real; uhsiknxuim : linkage std_logic);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture uwr of b is
  signal gjibhmxhoz : std_logic_vector(2 downto 1);
  signal q : std_logic_vector(0 to 3);
  signal p : std_logic_vector(0 to 3);
  signal qbr : std_logic_vector(2 downto 1);
  signal rezfmeiow : std_logic_vector(0 to 3);
begin
  bkwgur : entity work.pmwoba
    port map (uekcz => rezfmeiow, pvruvsbgaa => soxnm, tcfjollzi => qbr);
  fjieddpk : entity work.pmwoba
    port map (uekcz => p, pvruvsbgaa => soxnm, tcfjollzi => qbr);
  mf : entity work.pmwoba
    port map (uekcz => q, pvruvsbgaa => soxnm, tcfjollzi => gjibhmxhoz);
  
  -- Single-driven assignments
  soxnm <= 02.4311;
  
  -- Multi-driven assignments
  gjibhmxhoz <= ('W', 'X');
  rezfmeiow <= "LLWZ";
  qbr <= ('-', '-');
  rezfmeiow <= ('W', 'L', '-', 'W');
end uwr;



-- Seed after: 202963585208203569,8118127366649987907
