-- Seed: 14610446775444578518,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity gvy is
  port (g : out std_logic_vector(1 to 3); un : buffer integer);
end gvy;

architecture vpzsopa of gvy is
  
begin
  -- Single-driven assignments
  un <= 8#5#;
  
  -- Multi-driven assignments
  g <= ('Z', '0', 'H');
  g <= g;
  g <= ('W', 'X', 'W');
  g <= g;
end vpzsopa;

entity fpahxr is
  port (i : in bit);
end fpahxr;

library ieee;
use ieee.std_logic_1164.all;

architecture qp of fpahxr is
  signal uppefgubrc : integer;
  signal muyekcebry : std_logic_vector(1 to 3);
begin
  ai : entity work.gvy
    port map (g => muyekcebry, un => uppefgubrc);
  
  -- Multi-driven assignments
  muyekcebry <= "ULU";
end qp;



-- Seed after: 14122417606319930850,12269339630485015285
