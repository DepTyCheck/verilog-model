-- Seed: 14193675308183990451,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (dgawcf : out std_logic_vector(0 to 0); hkpsrjjdra : out std_logic_vector(4 downto 3); hgs : buffer time; twrevuyq : out time);
end v;

architecture jmrxbc of v is
  
begin
  -- Single-driven assignments
  twrevuyq <= 8#7_3_2_6_3# ps;
  hgs <= 034 ms;
  
  -- Multi-driven assignments
  hkpsrjjdra <= "X1";
  hkpsrjjdra <= "01";
  dgawcf <= "1";
end jmrxbc;

entity piiujj is
  port (ywndwmamu : out boolean_vector(4 downto 3));
end piiujj;

library ieee;
use ieee.std_logic_1164.all;

architecture uzbcffzffy of piiujj is
  signal pdxrhf : time;
  signal bzna : time;
  signal jtctkn : std_logic_vector(4 downto 3);
  signal wmjbkbi : std_logic_vector(0 to 0);
begin
  uvu : entity work.v
    port map (dgawcf => wmjbkbi, hkpsrjjdra => jtctkn, hgs => bzna, twrevuyq => pdxrhf);
  
  -- Multi-driven assignments
  wmjbkbi <= "1";
end uzbcffzffy;



-- Seed after: 13642010075276521288,13694093582652240945
