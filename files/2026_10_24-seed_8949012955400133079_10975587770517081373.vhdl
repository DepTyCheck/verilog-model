-- Seed: 8949012955400133079,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity ngxp is
  port (x : out std_logic_vector(0 to 3); l : linkage integer_vector(3 downto 3));
end ngxp;

architecture jf of ngxp is
  
begin
  -- Multi-driven assignments
  x <= "U-UX";
  x <= x;
  x <= x;
  x <= x;
end jf;

library ieee;
use ieee.std_logic_1164.all;

entity pfrzjmyxic is
  port (y : linkage std_logic_vector(1 to 0); o : buffer boolean; bunjy : buffer bit);
end pfrzjmyxic;

library ieee;
use ieee.std_logic_1164.all;

architecture jqry of pfrzjmyxic is
  signal ennzfn : integer_vector(3 downto 3);
  signal txrkiaw : integer_vector(3 downto 3);
  signal wjyq : std_logic_vector(0 to 3);
begin
  tesxhktdi : entity work.ngxp
    port map (x => wjyq, l => txrkiaw);
  mrxkqh : entity work.ngxp
    port map (x => wjyq, l => ennzfn);
  
  -- Multi-driven assignments
  wjyq <= wjyq;
  wjyq <= "U0XU";
end jqry;



-- Seed after: 1687148786306191606,10975587770517081373
