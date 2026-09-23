-- Seed: 10051230114551371189,8067602802092121131

entity uc is
  port (fpbopn : buffer integer_vector(4 to 2));
end uc;

architecture ansubo of uc is
  
begin
  -- Single-driven assignments
  fpbopn <= (others => 0);
end ansubo;

library ieee;
use ieee.std_logic_1164.all;

entity tmklns is
  port (m : in std_logic_vector(0 downto 0); pihrfcv : buffer real);
end tmklns;

architecture v of tmklns is
  signal klp : integer_vector(4 to 2);
  signal zejerbz : integer_vector(4 to 2);
  signal xujebklekc : integer_vector(4 to 2);
  signal cvp : integer_vector(4 to 2);
begin
  vnryxclz : entity work.uc
    port map (fpbopn => cvp);
  sarmpijq : entity work.uc
    port map (fpbopn => xujebklekc);
  wuht : entity work.uc
    port map (fpbopn => zejerbz);
  tvd : entity work.uc
    port map (fpbopn => klp);
  
  -- Single-driven assignments
  pihrfcv <= pihrfcv;
end v;



-- Seed after: 4838656303937462572,8067602802092121131
