-- Seed: 3411740187733626820,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity dcr is
  port (zjwc : in real; dihprm : in std_logic_vector(4 downto 1));
end dcr;

architecture gevm of dcr is
  
begin
  
end gevm;

entity txwplsm is
  port (okdcwa : in integer_vector(1 to 2));
end txwplsm;

library ieee;
use ieee.std_logic_1164.all;

architecture vi of txwplsm is
  signal lyvw : std_logic_vector(4 downto 1);
  signal rfg : std_logic_vector(4 downto 1);
  signal rliqvhwkv : real;
begin
  mgckwgtra : entity work.dcr
    port map (zjwc => rliqvhwkv, dihprm => rfg);
  mq : entity work.dcr
    port map (zjwc => rliqvhwkv, dihprm => lyvw);
  lfvtkzqzg : entity work.dcr
    port map (zjwc => rliqvhwkv, dihprm => rfg);
  
  -- Multi-driven assignments
  rfg <= rfg;
  rfg <= lyvw;
  rfg <= "LWXU";
end vi;

library ieee;
use ieee.std_logic_1164.all;

entity oifryg is
  port (uuqjfwqict : inout std_logic_vector(4 to 3); w : in integer);
end oifryg;

library ieee;
use ieee.std_logic_1164.all;

architecture ditobf of oifryg is
  signal wpwix : std_logic_vector(4 downto 1);
  signal zlkwzorh : real;
begin
  bkuyj : entity work.dcr
    port map (zjwc => zlkwzorh, dihprm => wpwix);
  
  -- Single-driven assignments
  zlkwzorh <= 16#B51.40#;
  
  -- Multi-driven assignments
  uuqjfwqict <= "";
  wpwix <= ('0', 'U', 'U', '0');
  uuqjfwqict <= uuqjfwqict;
  wpwix <= wpwix;
end ditobf;



-- Seed after: 5356312418184440522,8068158652091157513
