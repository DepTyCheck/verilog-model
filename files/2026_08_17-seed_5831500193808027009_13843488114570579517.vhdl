-- Seed: 5831500193808027009,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity dv is
  port (chbkwzh : linkage std_logic_vector(2 downto 4));
end dv;

architecture bxlelnv of dv is
  
begin
  
end bxlelnv;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (u : linkage real; mjwcm : inout std_logic_vector(0 downto 1));
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture eeplqxu of b is
  signal glr : std_logic_vector(2 downto 4);
  signal qlnxkka : std_logic_vector(2 downto 4);
begin
  zelxg : entity work.dv
    port map (chbkwzh => mjwcm);
  fumawmu : entity work.dv
    port map (chbkwzh => qlnxkka);
  fjxutmtzec : entity work.dv
    port map (chbkwzh => mjwcm);
  zoba : entity work.dv
    port map (chbkwzh => glr);
  
  -- Multi-driven assignments
  mjwcm <= "";
end eeplqxu;



-- Seed after: 5070576919530891714,13843488114570579517
