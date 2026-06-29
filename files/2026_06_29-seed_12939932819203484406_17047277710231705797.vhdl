-- Seed: 12939932819203484406,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity esmt is
  port (tmzsjn : out character; zmxhrmeh : buffer std_logic_vector(1 to 3));
end esmt;

architecture n of esmt is
  
begin
  -- Single-driven assignments
  tmzsjn <= 'y';
  
  -- Multi-driven assignments
  zmxhrmeh <= ('X', 'L', 'X');
  zmxhrmeh <= "W1-";
  zmxhrmeh <= "LUW";
  zmxhrmeh <= "ZX0";
end n;

library ieee;
use ieee.std_logic_1164.all;

entity rnuvlf is
  port (se : in integer; tiomkuack : out std_logic);
end rnuvlf;

library ieee;
use ieee.std_logic_1164.all;

architecture n of rnuvlf is
  signal mcceutjmm : std_logic_vector(1 to 3);
  signal llzc : character;
begin
  gtodol : entity work.esmt
    port map (tmzsjn => llzc, zmxhrmeh => mcceutjmm);
end n;



-- Seed after: 1740255236649568164,17047277710231705797
