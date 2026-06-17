-- Seed: 1664124329665578988,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity bwlklwvum is
  port (rpgvbhcvq : out integer; fd : buffer bit_vector(3 to 4); vjopc : out std_logic; ym : inout character);
end bwlklwvum;

architecture dsgyjwc of bwlklwvum is
  
begin
  -- Single-driven assignments
  ym <= 'a';
  rpgvbhcvq <= 8#5#;
  fd <= ('0', '1');
end dsgyjwc;

entity esnz is
  port (goxtqez : in boolean_vector(0 to 1));
end esnz;

library ieee;
use ieee.std_logic_1164.all;

architecture nklrvsn of esnz is
  signal orpczke : character;
  signal adpa : bit_vector(3 to 4);
  signal rb : integer;
  signal d : character;
  signal w : std_logic;
  signal rbivswatlj : bit_vector(3 to 4);
  signal muz : integer;
begin
  ie : entity work.bwlklwvum
    port map (rpgvbhcvq => muz, fd => rbivswatlj, vjopc => w, ym => d);
  idhagzl : entity work.bwlklwvum
    port map (rpgvbhcvq => rb, fd => adpa, vjopc => w, ym => orpczke);
  
  -- Multi-driven assignments
  w <= '1';
end nklrvsn;



-- Seed after: 11516061380240242396,10557070023141912087
