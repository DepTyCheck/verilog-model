-- Seed: 3835987308610643085,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity nyarkfcbfc is
  port (wwavsyeb : linkage integer; likghyhyyj : in std_logic_vector(2 to 2));
end nyarkfcbfc;

architecture i of nyarkfcbfc is
  
begin
  
end i;

library ieee;
use ieee.std_logic_1164.all;

entity zogbt is
  port (qin : buffer time; o : buffer real; fmpa : inout std_logic_vector(4 downto 4); hbz : buffer bit_vector(3 to 4));
end zogbt;

library ieee;
use ieee.std_logic_1164.all;

architecture ixnx of zogbt is
  signal qcrwsquag : std_logic_vector(2 to 2);
  signal rzilwv : integer;
  signal pjpt : std_logic_vector(2 to 2);
  signal qjfwgm : integer;
  signal qd : integer;
  signal eqvujnseu : std_logic_vector(2 to 2);
  signal d : integer;
begin
  marqevgr : entity work.nyarkfcbfc
    port map (wwavsyeb => d, likghyhyyj => eqvujnseu);
  afroiphonb : entity work.nyarkfcbfc
    port map (wwavsyeb => qd, likghyhyyj => eqvujnseu);
  gcwk : entity work.nyarkfcbfc
    port map (wwavsyeb => qjfwgm, likghyhyyj => pjpt);
  tku : entity work.nyarkfcbfc
    port map (wwavsyeb => rzilwv, likghyhyyj => qcrwsquag);
  
  -- Single-driven assignments
  qin <= qin;
  
  -- Multi-driven assignments
  pjpt <= "U";
  pjpt <= qcrwsquag;
  qcrwsquag <= pjpt;
  eqvujnseu <= eqvujnseu;
end ixnx;

library ieee;
use ieee.std_logic_1164.all;

entity zzeo is
  port (ydm : buffer integer; xfhehjki : out std_logic_vector(3 to 3); sdpivupni : in std_logic_vector(1 to 2); h : in boolean);
end zzeo;

library ieee;
use ieee.std_logic_1164.all;

architecture mfwd of zzeo is
  signal lb : bit_vector(3 to 4);
  signal oiqemaqhu : std_logic_vector(4 downto 4);
  signal ixxk : real;
  signal rftiw : time;
  signal azxhkle : std_logic_vector(2 to 2);
  signal lere : integer;
  signal pziffeqbc : std_logic_vector(2 to 2);
  signal k : integer;
begin
  kguubj : entity work.nyarkfcbfc
    port map (wwavsyeb => k, likghyhyyj => pziffeqbc);
  jjanzznlmy : entity work.nyarkfcbfc
    port map (wwavsyeb => lere, likghyhyyj => azxhkle);
  mbfs : entity work.zogbt
    port map (qin => rftiw, o => ixxk, fmpa => oiqemaqhu, hbz => lb);
  
  -- Multi-driven assignments
  pziffeqbc <= xfhehjki;
  xfhehjki <= "Z";
  xfhehjki <= "-";
  xfhehjki <= (others => 'L');
end mfwd;



-- Seed after: 10203504215181712319,12269339630485015285
