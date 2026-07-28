-- Seed: 9263028402724032245,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (cxd : in std_logic_vector(1 to 3));
end o;

architecture haajy of o is
  
begin
  
end haajy;

entity biasngvski is
  port (owrlt : inout time; znweynbs : out integer; lehs : linkage integer; ux : linkage real_vector(1 downto 2));
end biasngvski;

library ieee;
use ieee.std_logic_1164.all;

architecture p of biasngvski is
  signal n : std_logic_vector(1 to 3);
  signal lrvguxgfgf : std_logic_vector(1 to 3);
begin
  bddrelge : entity work.o
    port map (cxd => lrvguxgfgf);
  dn : entity work.o
    port map (cxd => lrvguxgfgf);
  gvg : entity work.o
    port map (cxd => n);
  
  -- Multi-driven assignments
  lrvguxgfgf <= ('Z', 'X', 'L');
  lrvguxgfgf <= ('L', 'X', 'X');
  lrvguxgfgf <= n;
  n <= lrvguxgfgf;
end p;



-- Seed after: 8185134187612569716,2511821214772927453
