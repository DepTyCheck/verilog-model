-- Seed: 10179814164406163927,11181851762153539145



entity ytdest is
  port (xycksz : out time_vector(3 to 1); ppa : inout boolean_vector(4 to 1); lqynncclf : buffer time);
end ytdest;



architecture ngc of ytdest is
  
begin
  
end ngc;

library ieee;
use ieee.std_logic_1164.all;

entity exoui is
  port (xoppufyceh : inout time; poqzumfait : inout bit; wk : linkage std_logic; jxaxt : buffer std_logic_vector(4 to 4));
end exoui;



architecture batvpcdw of exoui is
  signal mgslbrsrkd : time;
  signal qzxf : boolean_vector(4 to 1);
  signal wx : time_vector(3 to 1);
  signal baezzilik : boolean_vector(4 to 1);
  signal xiiz : time_vector(3 to 1);
  signal zqydiwko : time;
  signal xiqh : boolean_vector(4 to 1);
  signal m : time_vector(3 to 1);
begin
  vvejwcbjum : entity work.ytdest
    port map (xycksz => m, ppa => xiqh, lqynncclf => zqydiwko);
  izbceu : entity work.ytdest
    port map (xycksz => xiiz, ppa => baezzilik, lqynncclf => xoppufyceh);
  h : entity work.ytdest
    port map (xycksz => wx, ppa => qzxf, lqynncclf => mgslbrsrkd);
end batvpcdw;

library ieee;
use ieee.std_logic_1164.all;

entity znccrz is
  port (kytvhhqpj : linkage real; cr : buffer time; qznkmlyrx : linkage std_logic; rbnxeoie : out std_logic);
end znccrz;

library ieee;
use ieee.std_logic_1164.all;

architecture tbjcnajt of znccrz is
  signal vr : std_logic_vector(4 to 4);
  signal htaefhwl : std_logic;
  signal f : bit;
  signal novdcphewl : time;
  signal ul : boolean_vector(4 to 1);
  signal wtw : time_vector(3 to 1);
  signal spl : time;
  signal wkbjsp : boolean_vector(4 to 1);
  signal hyvr : time_vector(3 to 1);
  signal uy : time;
  signal ctcp : boolean_vector(4 to 1);
  signal mryfdgelev : time_vector(3 to 1);
begin
  rbt : entity work.ytdest
    port map (xycksz => mryfdgelev, ppa => ctcp, lqynncclf => uy);
  cb : entity work.ytdest
    port map (xycksz => hyvr, ppa => wkbjsp, lqynncclf => spl);
  qssc : entity work.ytdest
    port map (xycksz => wtw, ppa => ul, lqynncclf => novdcphewl);
  shizvenbj : entity work.exoui
    port map (xoppufyceh => cr, poqzumfait => f, wk => htaefhwl, jxaxt => vr);
end tbjcnajt;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (rx : inout std_logic);
end k;



architecture lpouboety of k is
  signal ye : time;
  signal ojercgb : boolean_vector(4 to 1);
  signal z : time_vector(3 to 1);
begin
  nozotjsvns : entity work.ytdest
    port map (xycksz => z, ppa => ojercgb, lqynncclf => ye);
end lpouboety;



-- Seed after: 3740140749986604155,11181851762153539145
