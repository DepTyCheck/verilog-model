-- Seed: 1202204207257276329,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity iih is
  port (xumxkvcafl : in std_logic_vector(4 downto 2); xewuljcj : in integer; mhokxhfrsy : inout boolean);
end iih;

architecture dzqcf of iih is
  
begin
  -- Single-driven assignments
  mhokxhfrsy <= FALSE;
end dzqcf;

library ieee;
use ieee.std_logic_1164.all;

entity njegasqjt is
  port (rzmcq : buffer std_logic);
end njegasqjt;

library ieee;
use ieee.std_logic_1164.all;

architecture nuyz of njegasqjt is
  signal xnfwrszrg : boolean;
  signal b : std_logic_vector(4 downto 2);
  signal q : boolean;
  signal nxfwyx : integer;
  signal c : std_logic_vector(4 downto 2);
  signal puc : boolean;
  signal z : integer;
  signal katlseudty : std_logic_vector(4 downto 2);
begin
  plxfix : entity work.iih
    port map (xumxkvcafl => katlseudty, xewuljcj => z, mhokxhfrsy => puc);
  djdoczz : entity work.iih
    port map (xumxkvcafl => c, xewuljcj => nxfwyx, mhokxhfrsy => q);
  khkkv : entity work.iih
    port map (xumxkvcafl => b, xewuljcj => z, mhokxhfrsy => xnfwrszrg);
  
  -- Single-driven assignments
  z <= z;
  nxfwyx <= 04;
  
  -- Multi-driven assignments
  rzmcq <= rzmcq;
  c <= katlseudty;
end nuyz;



-- Seed after: 10614536599134881510,3316342841050048249
