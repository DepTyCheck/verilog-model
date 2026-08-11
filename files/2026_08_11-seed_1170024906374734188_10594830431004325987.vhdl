-- Seed: 1170024906374734188,10594830431004325987

entity e is
  port (wtbsbwgzp : out boolean_vector(1 to 3));
end e;

architecture jzw of e is
  
begin
  -- Single-driven assignments
  wtbsbwgzp <= (FALSE, FALSE, TRUE);
end jzw;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (ynvuixh : inout std_logic_vector(2 to 3));
end r;

architecture bzbiyxx of r is
  signal y : boolean_vector(1 to 3);
begin
  fzx : entity work.e
    port map (wtbsbwgzp => y);
  
  -- Multi-driven assignments
  ynvuixh <= ynvuixh;
  ynvuixh <= ('1', 'X');
  ynvuixh <= ynvuixh;
end bzbiyxx;

entity qjojgroyoc is
  port (eue : buffer severity_level; so : buffer severity_level);
end qjojgroyoc;

library ieee;
use ieee.std_logic_1164.all;

architecture vyi of qjojgroyoc is
  signal hr : boolean_vector(1 to 3);
  signal umzner : std_logic_vector(2 to 3);
  signal amyemyabtz : boolean_vector(1 to 3);
begin
  xhyoqt : entity work.e
    port map (wtbsbwgzp => amyemyabtz);
  apkuenzrf : entity work.r
    port map (ynvuixh => umzner);
  gs : entity work.e
    port map (wtbsbwgzp => hr);
  
  -- Single-driven assignments
  so <= NOTE;
end vyi;



-- Seed after: 16154269941841700676,10594830431004325987
