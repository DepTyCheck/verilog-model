-- Seed: 9042055226665863852,3108530264173481209

entity nzduaew is
  port (sng : buffer real; qbtpcct : buffer severity_level; iblerkk : in time; thobenzpbw : inout time);
end nzduaew;

architecture yfrnee of nzduaew is
  
begin
  -- Single-driven assignments
  thobenzpbw <= 16#4.9B678# us;
  sng <= 3_0_0_2_2.02223;
  qbtpcct <= NOTE;
end yfrnee;

entity udcuvx is
  port (grwo : buffer real);
end udcuvx;

architecture ane of udcuvx is
  signal yyyow : time;
  signal kkukmi : time;
  signal ufxdzllf : severity_level;
  signal jtjbjxqjye : real;
  signal agg : time;
  signal utcjhqhbu : severity_level;
  signal lpxyvw : time;
  signal cvy : severity_level;
  signal rbaveazhx : real;
  signal tibjwawkme : time;
  signal xylxrhqry : time;
  signal mfylylexf : severity_level;
  signal dpblngc : real;
begin
  hticcino : entity work.nzduaew
    port map (sng => dpblngc, qbtpcct => mfylylexf, iblerkk => xylxrhqry, thobenzpbw => tibjwawkme);
  gekywg : entity work.nzduaew
    port map (sng => rbaveazhx, qbtpcct => cvy, iblerkk => lpxyvw, thobenzpbw => xylxrhqry);
  zhzwxsxkvr : entity work.nzduaew
    port map (sng => grwo, qbtpcct => utcjhqhbu, iblerkk => agg, thobenzpbw => lpxyvw);
  ad : entity work.nzduaew
    port map (sng => jtjbjxqjye, qbtpcct => ufxdzllf, iblerkk => kkukmi, thobenzpbw => yyyow);
end ane;

library ieee;
use ieee.std_logic_1164.all;

entity hirm is
  port (wdxm : buffer std_logic);
end hirm;

architecture kl of hirm is
  signal b : time;
  signal ue : time;
  signal z : severity_level;
  signal uam : real;
  signal mfvcacl : time;
  signal fq : severity_level;
  signal pgf : real;
  signal usasnvkylj : time;
  signal vqerg : time;
  signal mkccgzjng : severity_level;
  signal hej : real;
begin
  cb : entity work.nzduaew
    port map (sng => hej, qbtpcct => mkccgzjng, iblerkk => vqerg, thobenzpbw => usasnvkylj);
  etphmxlkx : entity work.nzduaew
    port map (sng => pgf, qbtpcct => fq, iblerkk => vqerg, thobenzpbw => mfvcacl);
  nqo : entity work.nzduaew
    port map (sng => uam, qbtpcct => z, iblerkk => ue, thobenzpbw => b);
  
  -- Single-driven assignments
  vqerg <= 0 min;
  ue <= 0_1_4_0.3_0 ps;
  
  -- Multi-driven assignments
  wdxm <= 'U';
  wdxm <= 'Z';
  wdxm <= 'X';
  wdxm <= 'L';
end kl;



-- Seed after: 8706770599589253029,3108530264173481209
