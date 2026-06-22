-- Seed: 15191434011307266553,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity bnsblpd is
  port (rzwtuv : buffer time_vector(2 downto 4); pgrqgh : inout boolean; chkuyghoib : linkage std_logic_vector(4 to 2));
end bnsblpd;

architecture rdbrhefnje of bnsblpd is
  
begin
  -- Single-driven assignments
  pgrqgh <= FALSE;
  rzwtuv <= (others => 0 ns);
end rdbrhefnje;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (yfisretm : out std_logic_vector(2 downto 0); ezzwpv : in real; ondxcvtez : linkage integer; cdfaiijbji : inout severity_level);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture wbgfjqj of j is
  signal zsfhi : boolean;
  signal zlcvc : time_vector(2 downto 4);
  signal d : std_logic_vector(4 to 2);
  signal twmnixba : boolean;
  signal n : time_vector(2 downto 4);
begin
  ndugdjbtq : entity work.bnsblpd
    port map (rzwtuv => n, pgrqgh => twmnixba, chkuyghoib => d);
  kgluq : entity work.bnsblpd
    port map (rzwtuv => zlcvc, pgrqgh => zsfhi, chkuyghoib => d);
  
  -- Multi-driven assignments
  yfisretm <= "HWZ";
  yfisretm <= ('1', 'X', '-');
  d <= "";
  d <= (others => '0');
end wbgfjqj;



-- Seed after: 9683891416651786716,13479070923501788437
