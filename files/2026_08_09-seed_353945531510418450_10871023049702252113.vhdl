-- Seed: 353945531510418450,10871023049702252113

entity sjierkqybu is
  port (r : inout time);
end sjierkqybu;

architecture zdnxplsu of sjierkqybu is
  
begin
  -- Single-driven assignments
  r <= 0_2.24203 us;
end zdnxplsu;

library ieee;
use ieee.std_logic_1164.all;

entity nczexjm is
  port (zby : out std_logic; vgavz : inout std_logic; totql : buffer character);
end nczexjm;

architecture cj of nczexjm is
  signal lgflkl : time;
  signal snxvcj : time;
  signal gjpgt : time;
begin
  xkvqscagib : entity work.sjierkqybu
    port map (r => gjpgt);
  ahblv : entity work.sjierkqybu
    port map (r => snxvcj);
  iwibaf : entity work.sjierkqybu
    port map (r => lgflkl);
  
  -- Single-driven assignments
  totql <= 'd';
  
  -- Multi-driven assignments
  zby <= 'H';
  vgavz <= zby;
end cj;



-- Seed after: 10755810126413607236,10871023049702252113
