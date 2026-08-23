-- Seed: 5403032558404222820,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity txhnurce is
  port (eaznird : inout std_logic_vector(2 downto 0));
end txhnurce;

architecture ovjkm of txhnurce is
  
begin
  -- Multi-driven assignments
  eaznird <= eaznird;
  eaznird <= ('X', 'X', 'L');
  eaznird <= ('X', 'U', 'U');
  eaznird <= eaznird;
end ovjkm;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (eizabhb : in std_logic_vector(2 downto 0); vwi : inout std_logic_vector(1 to 1));
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture thbenflj of s is
  signal nkxgrmp : std_logic_vector(2 downto 0);
begin
  e : entity work.txhnurce
    port map (eaznird => nkxgrmp);
  dxpptmfzbt : entity work.txhnurce
    port map (eaznird => nkxgrmp);
  
  -- Multi-driven assignments
  vwi <= (others => '1');
  vwi <= (others => 'X');
  nkxgrmp <= nkxgrmp;
  vwi <= (others => 'H');
end thbenflj;



-- Seed after: 15921580844495554647,4245627776430562977
