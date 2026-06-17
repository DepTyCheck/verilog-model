-- Seed: 14029750060296117686,10557070023141912087

entity ykwhj is
  port (itpxjrn : in bit_vector(1 to 1));
end ykwhj;

architecture fnw of ykwhj is
  
begin
  
end fnw;

library ieee;
use ieee.std_logic_1164.all;

entity qwjgsj is
  port (zsgqpbaa : inout std_logic; qonjgbwsjp : linkage time_vector(1 to 0));
end qwjgsj;

architecture wm of qwjgsj is
  signal bcnng : bit_vector(1 to 1);
  signal db : bit_vector(1 to 1);
begin
  vtkrjdeuu : entity work.ykwhj
    port map (itpxjrn => db);
  eohxljzx : entity work.ykwhj
    port map (itpxjrn => bcnng);
  
  -- Single-driven assignments
  bcnng <= (others => '0');
  db <= (others => '1');
  
  -- Multi-driven assignments
  zsgqpbaa <= 'X';
  zsgqpbaa <= 'H';
  zsgqpbaa <= 'Z';
end wm;



-- Seed after: 3955916344212903163,10557070023141912087
