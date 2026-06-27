-- Seed: 12189412400979255979,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity zw is
  port (ogxtfxbeq : in boolean_vector(0 to 3); aubu : linkage std_logic; dclpvinxc : linkage character);
end zw;

architecture y of zw is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity iia is
  port (kfjhvle : out integer; wfvui : linkage std_logic; wc : in std_logic_vector(2 downto 1); wgmvy : buffer time);
end iia;

library ieee;
use ieee.std_logic_1164.all;

architecture un of iia is
  signal eslsrdvadb : character;
  signal leembuv : std_logic;
  signal lcbp : boolean_vector(0 to 3);
  signal vazchugz : character;
  signal nddmptzd : std_logic;
  signal wlynfv : boolean_vector(0 to 3);
begin
  y : entity work.zw
    port map (ogxtfxbeq => wlynfv, aubu => nddmptzd, dclpvinxc => vazchugz);
  esbcd : entity work.zw
    port map (ogxtfxbeq => lcbp, aubu => leembuv, dclpvinxc => eslsrdvadb);
  
  -- Single-driven assignments
  wlynfv <= (FALSE, FALSE, FALSE, TRUE);
  kfjhvle <= 02013;
  wgmvy <= 2#0_1# us;
  lcbp <= (TRUE, FALSE, FALSE, TRUE);
  
  -- Multi-driven assignments
  leembuv <= 'X';
  nddmptzd <= 'L';
  leembuv <= '-';
  nddmptzd <= 'L';
end un;

library ieee;
use ieee.std_logic_1164.all;

entity xpwz is
  port (vcfxgdt : inout string(4 downto 5); dx : buffer std_logic);
end xpwz;

architecture uqa of xpwz is
  
begin
  -- Single-driven assignments
  vcfxgdt <= "";
end uqa;



-- Seed after: 14614916773921036126,4860866131898729603
