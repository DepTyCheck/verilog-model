-- Seed: 3664139242115840928,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity ootlwv is
  port (limao : buffer severity_level; lnp : inout std_logic);
end ootlwv;

architecture nqecwiu of ootlwv is
  
begin
  -- Single-driven assignments
  limao <= WARNING;
  
  -- Multi-driven assignments
  lnp <= 'H';
  lnp <= '0';
  lnp <= '0';
  lnp <= lnp;
end nqecwiu;



-- Seed after: 5385083566604813950,3316342841050048249
