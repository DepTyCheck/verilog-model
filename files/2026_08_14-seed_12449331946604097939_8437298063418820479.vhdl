-- Seed: 12449331946604097939,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (ff : out std_logic_vector(4 downto 2); zye : out time; uxgar : linkage std_logic; ytftvw : in character);
end t;

architecture di of t is
  
begin
  -- Single-driven assignments
  zye <= 3.4 ns;
  
  -- Multi-driven assignments
  ff <= "1ZZ";
  ff <= ff;
  ff <= ff;
  ff <= ff;
end di;

entity l is
  port (llbewtfgtl : out boolean_vector(2 downto 0));
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture miwlrcxk of l is
  signal yctaea : character;
  signal bbzmnhmnz : std_logic;
  signal fkvdd : time;
  signal vtmhsqfof : std_logic_vector(4 downto 2);
begin
  o : entity work.t
    port map (ff => vtmhsqfof, zye => fkvdd, uxgar => bbzmnhmnz, ytftvw => yctaea);
  
  -- Single-driven assignments
  llbewtfgtl <= (FALSE, FALSE, TRUE);
  yctaea <= yctaea;
end miwlrcxk;



-- Seed after: 10536682192639094904,8437298063418820479
