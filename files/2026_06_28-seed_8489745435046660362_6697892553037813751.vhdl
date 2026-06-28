-- Seed: 8489745435046660362,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity mqqp is
  port (r : in std_logic_vector(1 downto 0); mer : buffer std_logic_vector(0 to 4));
end mqqp;

architecture dkmb of mqqp is
  
begin
  
end dkmb;

library ieee;
use ieee.std_logic_1164.all;

entity xabj is
  port (arqnozf : out std_logic_vector(2 downto 3); cg : linkage std_logic_vector(3 to 0));
end xabj;

library ieee;
use ieee.std_logic_1164.all;

architecture biavqohic of xabj is
  signal kmelv : std_logic_vector(0 to 4);
  signal gfjmvg : std_logic_vector(1 downto 0);
begin
  cqrujh : entity work.mqqp
    port map (r => gfjmvg, mer => kmelv);
  ygcuq : entity work.mqqp
    port map (r => gfjmvg, mer => kmelv);
  
  -- Multi-driven assignments
  kmelv <= ('X', 'U', 'U', 'H', 'W');
  arqnozf <= "";
  gfjmvg <= ('H', 'U');
  arqnozf <= "";
end biavqohic;



-- Seed after: 14985033201217275738,6697892553037813751
