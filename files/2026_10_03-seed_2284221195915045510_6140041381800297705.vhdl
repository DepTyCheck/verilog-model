-- Seed: 2284221195915045510,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity qpse is
  port (gkxsrnne : buffer std_logic);
end qpse;

architecture satd of qpse is
  
begin
  -- Multi-driven assignments
  gkxsrnne <= gkxsrnne;
  gkxsrnne <= 'X';
  gkxsrnne <= gkxsrnne;
  gkxsrnne <= 'U';
end satd;

entity onrzjn is
  port (ldcj : buffer boolean_vector(2 downto 2));
end onrzjn;

library ieee;
use ieee.std_logic_1164.all;

architecture zawxm of onrzjn is
  signal renbzks : std_logic;
begin
  leben : entity work.qpse
    port map (gkxsrnne => renbzks);
  
  -- Multi-driven assignments
  renbzks <= renbzks;
end zawxm;

library ieee;
use ieee.std_logic_1164.all;

entity nkw is
  port (tcqehv : out severity_level; nyrlbtzxwz : inout std_logic; r : out std_logic_vector(0 to 3); lypfkwdcvz : inout boolean);
end nkw;

architecture hbxk of nkw is
  signal fdmmkwolir : boolean_vector(2 downto 2);
begin
  vajepedn : entity work.onrzjn
    port map (ldcj => fdmmkwolir);
  muryhvugc : entity work.qpse
    port map (gkxsrnne => nyrlbtzxwz);
  
  -- Single-driven assignments
  lypfkwdcvz <= lypfkwdcvz;
  tcqehv <= WARNING;
  
  -- Multi-driven assignments
  nyrlbtzxwz <= nyrlbtzxwz;
  r <= ('W', 'W', 'X', 'L');
  nyrlbtzxwz <= 'Z';
  nyrlbtzxwz <= '-';
end hbxk;



-- Seed after: 13746515751065464261,6140041381800297705
