-- Seed: 2979972235097137319,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity rsfpsuty is
  port (r : inout std_logic; lkck : out std_logic_vector(2 downto 4); shtbcylso : in std_logic);
end rsfpsuty;

architecture ufl of rsfpsuty is
  
begin
  -- Multi-driven assignments
  lkck <= "";
  lkck <= (others => '0');
  lkck <= lkck;
  lkck <= "";
end ufl;



-- Seed after: 973885440115529501,5444032936641997477
