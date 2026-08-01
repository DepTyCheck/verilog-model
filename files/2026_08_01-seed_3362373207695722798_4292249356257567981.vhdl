-- Seed: 3362373207695722798,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity ldu is
  port (gdawva : inout std_logic_vector(4 downto 1));
end ldu;

architecture lgetxgivl of ldu is
  
begin
  -- Multi-driven assignments
  gdawva <= gdawva;
  gdawva <= gdawva;
end lgetxgivl;

entity dmed is
  port (gdpq : out time);
end dmed;

library ieee;
use ieee.std_logic_1164.all;

architecture fjbub of dmed is
  signal ecf : std_logic_vector(4 downto 1);
begin
  hweqf : entity work.ldu
    port map (gdawva => ecf);
  xyk : entity work.ldu
    port map (gdawva => ecf);
  
  -- Single-driven assignments
  gdpq <= 1 hr;
  
  -- Multi-driven assignments
  ecf <= "XX1X";
  ecf <= "110U";
  ecf <= ('-', 'H', '1', '1');
end fjbub;



-- Seed after: 6441445500571569122,4292249356257567981
