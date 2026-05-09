-- Seed: 7713054566243611027,16716360695494742805



entity aeqlwbs is
  port (lw : linkage integer; xn : linkage integer; q : in integer);
end aeqlwbs;



architecture moyb of aeqlwbs is
  
begin
  
end moyb;



entity dspjyjpvl is
  port (b : buffer real; yeyfzukeao : out bit; ysendmmlwz : out time; vfabrxtch : inout integer);
end dspjyjpvl;



architecture sodjtf of dspjyjpvl is
  signal bgafs : integer;
  signal fullmi : integer;
  signal gt : integer;
begin
  x : entity work.aeqlwbs
    port map (lw => gt, xn => vfabrxtch, q => fullmi);
  v : entity work.aeqlwbs
    port map (lw => bgafs, xn => bgafs, q => gt);
end sodjtf;

library ieee;
use ieee.std_logic_1164.all;

entity daiomacq is
  port (joq : linkage std_logic; xy : out std_logic; sxaoz : linkage integer; pukryamu : linkage real);
end daiomacq;



architecture eueb of daiomacq is
  signal r : time;
  signal numifz : bit;
  signal vmiqpkze : real;
  signal grbcpz : integer;
  signal siejn : integer;
  signal pqtfmojy : integer;
  signal zqh : integer;
  signal rthklm : integer;
  signal qbbos : integer;
  signal u : integer;
begin
  okhdxhlcc : entity work.aeqlwbs
    port map (lw => u, xn => qbbos, q => rthklm);
  chejbhwqo : entity work.aeqlwbs
    port map (lw => rthklm, xn => zqh, q => pqtfmojy);
  gni : entity work.aeqlwbs
    port map (lw => siejn, xn => qbbos, q => grbcpz);
  twbr : entity work.dspjyjpvl
    port map (b => vmiqpkze, yeyfzukeao => numifz, ysendmmlwz => r, vfabrxtch => zqh);
end eueb;



-- Seed after: 985519399745919187,16716360695494742805
