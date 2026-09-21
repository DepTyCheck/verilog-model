-- Seed: 6409162573453816247,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity rizthxaor is
  port (pmyt : inout std_logic_vector(4 downto 3));
end rizthxaor;

architecture eaqhm of rizthxaor is
  
begin
  
end eaqhm;

library ieee;
use ieee.std_logic_1164.all;

entity kgkttai is
  port (suoiky : linkage string(4 downto 3); rvzreisr : linkage time; cng : out std_logic);
end kgkttai;

library ieee;
use ieee.std_logic_1164.all;

architecture fc of kgkttai is
  signal fvqnyimhn : std_logic_vector(4 downto 3);
begin
  vu : entity work.rizthxaor
    port map (pmyt => fvqnyimhn);
  jaoq : entity work.rizthxaor
    port map (pmyt => fvqnyimhn);
  
  -- Multi-driven assignments
  cng <= '1';
  cng <= cng;
  cng <= '1';
  fvqnyimhn <= "XX";
end fc;



-- Seed after: 4305741936462797906,12143220691580258643
