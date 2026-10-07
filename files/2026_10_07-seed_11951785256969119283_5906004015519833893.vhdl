-- Seed: 11951785256969119283,5906004015519833893

entity ecqnoskvhe is
  port (kqngvo : in boolean_vector(0 to 4));
end ecqnoskvhe;

architecture cmxjj of ecqnoskvhe is
  
begin
  
end cmxjj;

library ieee;
use ieee.std_logic_1164.all;

entity iqmda is
  port (grfqcbdhn : inout std_logic_vector(2 downto 4); aanjglqo : in std_logic; nif : buffer std_logic);
end iqmda;

architecture cubdxob of iqmda is
  signal xkc : boolean_vector(0 to 4);
  signal qyz : boolean_vector(0 to 4);
begin
  o : entity work.ecqnoskvhe
    port map (kqngvo => qyz);
  ewr : entity work.ecqnoskvhe
    port map (kqngvo => xkc);
  mhqagb : entity work.ecqnoskvhe
    port map (kqngvo => xkc);
  
  -- Single-driven assignments
  xkc <= qyz;
  
  -- Multi-driven assignments
  nif <= 'U';
end cubdxob;



-- Seed after: 13388715836159281535,5906004015519833893
