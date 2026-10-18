-- Seed: 2832642912539359593,14108379317686958507

entity g is
  port (bmsl : buffer integer; no : in time);
end g;

architecture z of g is
  
begin
  -- Single-driven assignments
  bmsl <= bmsl;
end z;

library ieee;
use ieee.std_logic_1164.all;

entity gqdji is
  port (qceiufr : inout boolean; kyzop : out std_logic; iqqlh : buffer time; xhv : inout integer);
end gqdji;

architecture znygnr of gqdji is
  signal hm : integer;
  signal parxjdduat : integer;
begin
  do : entity work.g
    port map (bmsl => parxjdduat, no => iqqlh);
  yyypnfq : entity work.g
    port map (bmsl => xhv, no => iqqlh);
  kadsso : entity work.g
    port map (bmsl => hm, no => iqqlh);
  
  -- Single-driven assignments
  qceiufr <= TRUE;
  iqqlh <= iqqlh;
  
  -- Multi-driven assignments
  kyzop <= '0';
  kyzop <= kyzop;
end znygnr;

library ieee;
use ieee.std_logic_1164.all;

entity jrqeoztl is
  port (hrsrahxjz : linkage bit; bivilslzc : in std_logic);
end jrqeoztl;

library ieee;
use ieee.std_logic_1164.all;

architecture uffguyixuu of jrqeoztl is
  signal qvug : integer;
  signal wotknr : time;
  signal emyszpmygn : std_logic;
  signal ujtsnuuqgz : boolean;
  signal lext : integer;
  signal cmimbq : time;
  signal vyyoedbt : integer;
  signal wfkypyszwm : integer;
  signal kgbp : time;
  signal e : std_logic;
  signal bqos : boolean;
begin
  fb : entity work.gqdji
    port map (qceiufr => bqos, kyzop => e, iqqlh => kgbp, xhv => wfkypyszwm);
  zz : entity work.g
    port map (bmsl => vyyoedbt, no => cmimbq);
  eqi : entity work.g
    port map (bmsl => lext, no => kgbp);
  jddtuxefov : entity work.gqdji
    port map (qceiufr => ujtsnuuqgz, kyzop => emyszpmygn, iqqlh => wotknr, xhv => qvug);
  
  -- Single-driven assignments
  cmimbq <= 31 ps;
  
  -- Multi-driven assignments
  emyszpmygn <= 'Z';
  emyszpmygn <= bivilslzc;
  e <= bivilslzc;
  e <= 'U';
end uffguyixuu;

library ieee;
use ieee.std_logic_1164.all;

entity fkb is
  port (pevtfdy : inout std_logic_vector(0 to 4); z : linkage bit; pvzowqnl : linkage time_vector(2 to 3));
end fkb;

architecture jxiinbhqd of fkb is
  signal egbtgld : time;
  signal duubeoqr : integer;
begin
  bbhofoix : entity work.g
    port map (bmsl => duubeoqr, no => egbtgld);
  
  -- Multi-driven assignments
  pevtfdy <= ('1', 'H', 'Z', 'W', 'W');
  pevtfdy <= pevtfdy;
  pevtfdy <= ('Z', '0', '-', 'X', '0');
  pevtfdy <= "U0L1Z";
end jxiinbhqd;



-- Seed after: 6305391143097601013,14108379317686958507
