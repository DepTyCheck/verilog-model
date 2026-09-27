-- Seed: 4248972921794151548,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (v : linkage std_logic_vector(2 to 4));
end p;

architecture mfcvyalg of p is
  
begin
  
end mfcvyalg;

entity ozwdo is
  port (m : in time; jnkzbj : out bit_vector(1 to 0); pucghaa : out integer);
end ozwdo;

library ieee;
use ieee.std_logic_1164.all;

architecture sy of ozwdo is
  signal nw : std_logic_vector(2 to 4);
begin
  fpiyx : entity work.p
    port map (v => nw);
  
  -- Single-driven assignments
  jnkzbj <= (others => '0');
  
  -- Multi-driven assignments
  nw <= nw;
  nw <= "1XH";
  nw <= ('1', 'H', 'H');
  nw <= nw;
end sy;



-- Seed after: 10405273212270607685,6379010654866854599
