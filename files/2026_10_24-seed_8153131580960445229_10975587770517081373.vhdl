-- Seed: 8153131580960445229,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity lzkaz is
  port (gsuq : linkage std_logic; niwa : in std_logic_vector(1 to 1); ldn : buffer real);
end lzkaz;

architecture omrdbshgzz of lzkaz is
  
begin
  -- Single-driven assignments
  ldn <= 3330.00;
end omrdbshgzz;

library ieee;
use ieee.std_logic_1164.all;

entity dthlleyo is
  port (mjppq : in real; g : buffer std_logic_vector(0 downto 3); dj : out boolean_vector(0 to 4); lhyhi : in integer);
end dthlleyo;

library ieee;
use ieee.std_logic_1164.all;

architecture jeggipal of dthlleyo is
  signal naipau : real;
  signal tkedvqmvyg : std_logic_vector(1 to 1);
  signal dyryenfj : real;
  signal roipnrs : real;
  signal bbrsqoscsw : std_logic_vector(1 to 1);
  signal qx : std_logic;
begin
  xohbardigz : entity work.lzkaz
    port map (gsuq => qx, niwa => bbrsqoscsw, ldn => roipnrs);
  tbgx : entity work.lzkaz
    port map (gsuq => qx, niwa => bbrsqoscsw, ldn => dyryenfj);
  ekrhmq : entity work.lzkaz
    port map (gsuq => qx, niwa => tkedvqmvyg, ldn => naipau);
  
  -- Single-driven assignments
  dj <= (TRUE, TRUE, TRUE, FALSE, FALSE);
  
  -- Multi-driven assignments
  g <= "";
end jeggipal;

library ieee;
use ieee.std_logic_1164.all;

entity cddfmopev is
  port (umopda : out real; xc : out std_logic; sj : buffer integer);
end cddfmopev;

library ieee;
use ieee.std_logic_1164.all;

architecture cx of cddfmopev is
  signal hynpdbjpz : std_logic_vector(1 to 1);
begin
  bsp : entity work.lzkaz
    port map (gsuq => xc, niwa => hynpdbjpz, ldn => umopda);
  
  -- Single-driven assignments
  sj <= 2#1_0_1#;
  
  -- Multi-driven assignments
  xc <= xc;
  xc <= 'X';
  xc <= '1';
  xc <= xc;
end cx;



-- Seed after: 8965953570064648994,10975587770517081373
