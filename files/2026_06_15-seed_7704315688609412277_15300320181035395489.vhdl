-- Seed: 7704315688609412277,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity vnxtfjyh is
  port (bhresmcxuh : out time; x : inout time_vector(2 to 4); xzqdddzpk : inout std_logic);
end vnxtfjyh;

architecture qkk of vnxtfjyh is
  
begin
  -- Single-driven assignments
  x <= (8#217# ps, 0 min, 2#1_0.1010# us);
  bhresmcxuh <= 4_2 ps;
  
  -- Multi-driven assignments
  xzqdddzpk <= '-';
  xzqdddzpk <= '-';
end qkk;

library ieee;
use ieee.std_logic_1164.all;

entity ghoggq is
  port (armiuvh : out std_logic; dnktnzajg : buffer integer; gsq : inout time);
end ghoggq;

architecture bx of ghoggq is
  signal bdlmpk : time_vector(2 to 4);
begin
  wa : entity work.vnxtfjyh
    port map (bhresmcxuh => gsq, x => bdlmpk, xzqdddzpk => armiuvh);
  
  -- Single-driven assignments
  dnktnzajg <= 41312;
  
  -- Multi-driven assignments
  armiuvh <= '0';
  armiuvh <= '0';
  armiuvh <= 'U';
  armiuvh <= 'W';
end bx;

entity lkveaikel is
  port (etamqs : buffer integer);
end lkveaikel;

architecture rg of lkveaikel is
  
begin
  -- Single-driven assignments
  etamqs <= 3;
end rg;

library ieee;
use ieee.std_logic_1164.all;

entity lfmd is
  port (jl : linkage std_logic);
end lfmd;

library ieee;
use ieee.std_logic_1164.all;

architecture kejcbc of lfmd is
  signal clux : time;
  signal iisbzry : integer;
  signal zslh : std_logic;
  signal swcakmfiu : time_vector(2 to 4);
  signal gwa : time;
  signal zamgphayrw : integer;
  signal dejjj : time;
  signal q : integer;
  signal dzvtuugv : std_logic;
begin
  qydqvvtbkq : entity work.ghoggq
    port map (armiuvh => dzvtuugv, dnktnzajg => q, gsq => dejjj);
  rbkx : entity work.lkveaikel
    port map (etamqs => zamgphayrw);
  oqcqwrvc : entity work.vnxtfjyh
    port map (bhresmcxuh => gwa, x => swcakmfiu, xzqdddzpk => dzvtuugv);
  uca : entity work.ghoggq
    port map (armiuvh => zslh, dnktnzajg => iisbzry, gsq => clux);
  
  -- Multi-driven assignments
  dzvtuugv <= 'W';
  dzvtuugv <= 'W';
  zslh <= '0';
  zslh <= '0';
end kejcbc;



-- Seed after: 4473630293619308533,15300320181035395489
