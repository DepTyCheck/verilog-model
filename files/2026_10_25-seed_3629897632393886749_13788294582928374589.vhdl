-- Seed: 3629897632393886749,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity bd is
  port (t : buffer std_logic; kxyugw : in real_vector(3 to 3));
end bd;

architecture ss of bd is
  
begin
  -- Multi-driven assignments
  t <= 'X';
  t <= t;
  t <= '1';
end ss;

library ieee;
use ieee.std_logic_1164.all;

entity lte is
  port (bgmtqk : in std_logic);
end lte;

library ieee;
use ieee.std_logic_1164.all;

architecture gvkdqdw of lte is
  signal hzcml : std_logic;
  signal y : real_vector(3 to 3);
  signal kxhvqutziw : real_vector(3 to 3);
  signal khlhjcy : std_logic;
begin
  riytsdjtiw : entity work.bd
    port map (t => khlhjcy, kxyugw => kxhvqutziw);
  dp : entity work.bd
    port map (t => khlhjcy, kxyugw => kxhvqutziw);
  am : entity work.bd
    port map (t => khlhjcy, kxyugw => y);
  mcushtneyo : entity work.bd
    port map (t => hzcml, kxyugw => kxhvqutziw);
  
  -- Single-driven assignments
  kxhvqutziw <= kxhvqutziw;
  y <= (others => 8#0_3_4_0.7_1_2_6_7#);
  
  -- Multi-driven assignments
  khlhjcy <= bgmtqk;
  hzcml <= '0';
  khlhjcy <= 'U';
  hzcml <= 'Z';
end gvkdqdw;



-- Seed after: 16959182441496820253,13788294582928374589
