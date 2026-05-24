-- Seed: 3614132044356636509,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (iaaulfnz : buffer std_logic_vector(4 downto 3); mgs : out std_logic_vector(4 to 1); xiwcndwzi : buffer time; v : inout integer_vector(1 to 1));
end m;



architecture azdflzbb of m is
  
begin
  
end azdflzbb;

library ieee;
use ieee.std_logic_1164.all;

entity ssuvup is
  port (pfi : in integer; mvhmfpide : in std_logic);
end ssuvup;

library ieee;
use ieee.std_logic_1164.all;

architecture nb of ssuvup is
  signal qime : integer_vector(1 to 1);
  signal vy : time;
  signal hdvob : std_logic_vector(4 to 1);
  signal jfzlfvi : std_logic_vector(4 downto 3);
begin
  vzlzgvh : entity work.m
    port map (iaaulfnz => jfzlfvi, mgs => hdvob, xiwcndwzi => vy, v => qime);
end nb;

library ieee;
use ieee.std_logic_1164.all;

entity sthixls is
  port (chalzub : out integer; c : inout std_logic; xbvwxmq : in time; hvlh : buffer integer);
end sthixls;

library ieee;
use ieee.std_logic_1164.all;

architecture j of sthixls is
  signal nyyuwjug : integer_vector(1 to 1);
  signal z : time;
  signal kbkos : std_logic_vector(4 downto 3);
  signal djmb : integer_vector(1 to 1);
  signal d : time;
  signal izamsconhi : std_logic_vector(4 to 1);
  signal p : std_logic_vector(4 downto 3);
begin
  iqdjc : entity work.m
    port map (iaaulfnz => p, mgs => izamsconhi, xiwcndwzi => d, v => djmb);
  pkfvpxqst : entity work.m
    port map (iaaulfnz => kbkos, mgs => izamsconhi, xiwcndwzi => z, v => nyyuwjug);
end j;



entity wurexouk is
  port (vzeq : buffer real);
end wurexouk;

library ieee;
use ieee.std_logic_1164.all;

architecture mroavswt of wurexouk is
  signal mdxakmroy : integer_vector(1 to 1);
  signal uxkc : time;
  signal ef : std_logic_vector(4 downto 3);
  signal xhzwxpqgao : integer_vector(1 to 1);
  signal x : time;
  signal vmuviwd : std_logic_vector(4 to 1);
  signal wmijdyuv : std_logic_vector(4 downto 3);
  signal vz : std_logic;
  signal oq : integer;
begin
  nwavxzflv : entity work.ssuvup
    port map (pfi => oq, mvhmfpide => vz);
  zst : entity work.m
    port map (iaaulfnz => wmijdyuv, mgs => vmuviwd, xiwcndwzi => x, v => xhzwxpqgao);
  ldvbll : entity work.m
    port map (iaaulfnz => ef, mgs => vmuviwd, xiwcndwzi => uxkc, v => mdxakmroy);
end mroavswt;



-- Seed after: 2543618415264179644,11387579217500963635
