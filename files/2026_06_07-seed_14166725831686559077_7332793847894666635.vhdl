-- Seed: 14166725831686559077,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (ed : out std_logic_vector(2 to 3); kclbsxu : in std_logic);
end e;



architecture qvygf of e is
  
begin
  
end qvygf;



entity blmgndd is
  port (kqcgxhxgrx : in integer);
end blmgndd;

library ieee;
use ieee.std_logic_1164.all;

architecture zdpsxbrr of blmgndd is
  signal iy : std_logic;
  signal yuscp : std_logic_vector(2 to 3);
  signal tgddgig : std_logic;
  signal dqdbimxx : std_logic_vector(2 to 3);
begin
  oxttqhytse : entity work.e
    port map (ed => dqdbimxx, kclbsxu => tgddgig);
  mllah : entity work.e
    port map (ed => yuscp, kclbsxu => iy);
end zdpsxbrr;

library ieee;
use ieee.std_logic_1164.all;

entity xibb is
  port (vyxxiwrt : in std_logic);
end xibb;



architecture hkt of xibb is
  signal faim : integer;
  signal p : integer;
begin
  aej : entity work.blmgndd
    port map (kqcgxhxgrx => p);
  qrerfzuepm : entity work.blmgndd
    port map (kqcgxhxgrx => faim);
end hkt;



-- Seed after: 15622365497955392752,7332793847894666635
