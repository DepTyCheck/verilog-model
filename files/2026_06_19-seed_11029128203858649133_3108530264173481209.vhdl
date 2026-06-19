-- Seed: 11029128203858649133,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity pt is
  port (sb : in std_logic_vector(1 to 2); fplemfeu : inout std_logic_vector(1 downto 0); czluclfwuu : in boolean; l : linkage bit_vector(3 to 0));
end pt;

architecture hurh of pt is
  
begin
  -- Multi-driven assignments
  fplemfeu <= ('1', '1');
  fplemfeu <= ('Z', 'X');
  fplemfeu <= ('X', '-');
end hurh;

library ieee;
use ieee.std_logic_1164.all;

entity lnygp is
  port (dygzw : buffer real; fdpl : out character; xgdr : inout std_logic_vector(1 to 0));
end lnygp;

library ieee;
use ieee.std_logic_1164.all;

architecture q of lnygp is
  signal vujh : bit_vector(3 to 0);
  signal zajmuu : boolean;
  signal ta : std_logic_vector(1 downto 0);
  signal axkma : std_logic_vector(1 to 2);
begin
  ubyceenrmv : entity work.pt
    port map (sb => axkma, fplemfeu => ta, czluclfwuu => zajmuu, l => vujh);
  
  -- Single-driven assignments
  fdpl <= 'n';
  zajmuu <= FALSE;
  dygzw <= 3_3.4;
  
  -- Multi-driven assignments
  xgdr <= "";
end q;



-- Seed after: 15361811369452676536,3108530264173481209
