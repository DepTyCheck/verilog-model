-- Seed: 14021792199300650104,15306146742274979323

entity dnkosd is
  port (uaoolc : inout integer; bn : in integer; gk : linkage time);
end dnkosd;

architecture rhbyt of dnkosd is
  
begin
  -- Single-driven assignments
  uaoolc <= 3_4;
end rhbyt;

library ieee;
use ieee.std_logic_1164.all;

entity uzmz is
  port (qyp : linkage std_logic_vector(0 downto 4); dfctcrmayd : in std_logic; nyiafq : buffer severity_level);
end uzmz;

architecture hbaqfqqz of uzmz is
  signal sxddqgw : time;
  signal exfrqotlbe : integer;
  signal arw : time;
  signal xnzfybsc : integer;
  signal owmbss : integer;
  signal vciwydup : time;
  signal vatwtxpg : integer;
begin
  mqytuutf : entity work.dnkosd
    port map (uaoolc => vatwtxpg, bn => vatwtxpg, gk => vciwydup);
  qsdjebwgvz : entity work.dnkosd
    port map (uaoolc => owmbss, bn => xnzfybsc, gk => arw);
  fyl : entity work.dnkosd
    port map (uaoolc => exfrqotlbe, bn => exfrqotlbe, gk => sxddqgw);
  
  -- Single-driven assignments
  xnzfybsc <= exfrqotlbe;
  nyiafq <= WARNING;
end hbaqfqqz;

entity mvuopjjnkw is
  port (duhnfkady : in time; ydvewrq : buffer real_vector(4 to 4));
end mvuopjjnkw;

architecture cefz of mvuopjjnkw is
  signal mwbsrq : time;
  signal thtibtphl : integer;
  signal pd : integer;
begin
  npggbrlmf : entity work.dnkosd
    port map (uaoolc => pd, bn => thtibtphl, gk => mwbsrq);
  
  -- Single-driven assignments
  thtibtphl <= 8#220#;
end cefz;

entity rn is
  port (h : buffer integer; tbdcjvte : inout time);
end rn;

library ieee;
use ieee.std_logic_1164.all;

architecture qng of rn is
  signal pbreipm : severity_level;
  signal wjlqnqfah : std_logic;
  signal mx : std_logic_vector(0 downto 4);
  signal rqlsrxt : integer;
  signal iuqdnhxml : integer;
  signal ryffovsvef : real_vector(4 to 4);
begin
  d : entity work.mvuopjjnkw
    port map (duhnfkady => tbdcjvte, ydvewrq => ryffovsvef);
  zitqwx : entity work.dnkosd
    port map (uaoolc => iuqdnhxml, bn => rqlsrxt, gk => tbdcjvte);
  roz : entity work.uzmz
    port map (qyp => mx, dfctcrmayd => wjlqnqfah, nyiafq => pbreipm);
  
  -- Single-driven assignments
  h <= 2#11001#;
  rqlsrxt <= 1;
end qng;



-- Seed after: 16807544805811048388,15306146742274979323
