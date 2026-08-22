-- Seed: 5058713242454387539,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity po is
  port (nsimru : out std_logic_vector(4 downto 3));
end po;

architecture e of po is
  
begin
  -- Multi-driven assignments
  nsimru <= nsimru;
  nsimru <= nsimru;
  nsimru <= "-W";
end e;



-- Seed after: 4276704976138603856,5805648483995786113
