-- Seed: 10375285598013022062,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity qsuofi is
  port (hucswl : out std_logic_vector(1 downto 3); nkieg : inout real_vector(0 to 4); ixdn : out std_logic);
end qsuofi;

architecture psf of qsuofi is
  
begin
  -- Single-driven assignments
  nkieg <= (4224.1344, 16#B91.6#, 2#1_1.1#, 0_4_3.442, 43241.000);
  
  -- Multi-driven assignments
  ixdn <= ixdn;
  ixdn <= '0';
  ixdn <= 'L';
end psf;

library ieee;
use ieee.std_logic_1164.all;

entity em is
  port (w : buffer integer; uoxz : buffer bit_vector(0 to 2); soh : buffer std_logic_vector(1 to 1));
end em;

library ieee;
use ieee.std_logic_1164.all;

architecture mdxygdzjc of em is
  signal gwhyeozcqd : real_vector(0 to 4);
  signal qj : std_logic_vector(1 downto 3);
  signal koqbmpzwvh : real_vector(0 to 4);
  signal efyhddkh : std_logic;
  signal lkptlzxpdm : real_vector(0 to 4);
  signal onrxusbxrk : std_logic_vector(1 downto 3);
begin
  vjlqpdd : entity work.qsuofi
    port map (hucswl => onrxusbxrk, nkieg => lkptlzxpdm, ixdn => efyhddkh);
  uldwazdooe : entity work.qsuofi
    port map (hucswl => onrxusbxrk, nkieg => koqbmpzwvh, ixdn => efyhddkh);
  llovgj : entity work.qsuofi
    port map (hucswl => qj, nkieg => gwhyeozcqd, ixdn => efyhddkh);
  
  -- Single-driven assignments
  w <= 2#1#;
  
  -- Multi-driven assignments
  soh <= soh;
  qj <= onrxusbxrk;
  soh <= soh;
  soh <= "0";
end mdxygdzjc;



-- Seed after: 18276617262803554968,8437298063418820479
