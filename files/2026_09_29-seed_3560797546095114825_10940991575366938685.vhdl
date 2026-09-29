-- Seed: 3560797546095114825,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity ltzddsko is
  port (zwi : linkage real; g : linkage std_logic; y : inout std_logic_vector(1 to 2));
end ltzddsko;

architecture ifljpn of ltzddsko is
  
begin
  -- Multi-driven assignments
  y <= y;
  y <= y;
  y <= "XH";
  y <= ('U', 'U');
end ifljpn;

entity e is
  port (gdfkziya : linkage real; l : buffer boolean_vector(4 to 0); ya : buffer time);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture otye of e is
  signal qsjupz : std_logic_vector(1 to 2);
  signal fn : real;
  signal kxj : std_logic_vector(1 to 2);
  signal kevmy : std_logic;
  signal wdihl : real;
begin
  uhsajdq : entity work.ltzddsko
    port map (zwi => wdihl, g => kevmy, y => kxj);
  ozjyiyz : entity work.ltzddsko
    port map (zwi => fn, g => kevmy, y => qsjupz);
  mkhlltmruu : entity work.ltzddsko
    port map (zwi => gdfkziya, g => kevmy, y => kxj);
  
  -- Single-driven assignments
  ya <= 0 min;
  l <= (others => TRUE);
end otye;

library ieee;
use ieee.std_logic_1164.all;

entity qifg is
  port (ft : buffer std_logic_vector(4 to 0); layxrdjnma : buffer std_logic);
end qifg;

architecture ekgipu of qifg is
  signal ynug : time;
  signal kri : boolean_vector(4 to 0);
  signal rrzku : real;
begin
  ywsobgevb : entity work.e
    port map (gdfkziya => rrzku, l => kri, ya => ynug);
  
  -- Multi-driven assignments
  ft <= (others => '0');
  ft <= ft;
  layxrdjnma <= layxrdjnma;
end ekgipu;



-- Seed after: 14276515544261198420,10940991575366938685
