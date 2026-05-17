-- Seed: 2491823321871628884,3820899062418988741



entity eoyanxmwdn is
  port (ulqqu : buffer time; qkbp : in real; xprqrdxvd : linkage time);
end eoyanxmwdn;



architecture bdhlui of eoyanxmwdn is
  
begin
  
end bdhlui;

library ieee;
use ieee.std_logic_1164.all;

entity ggdmxrguqm is
  port (qb : out std_logic; ejlptrk : buffer time);
end ggdmxrguqm;



architecture xuiyszjff of ggdmxrguqm is
  signal zekjhqzrb : real;
  signal pgvpt : time;
  signal vj : real;
  signal darfhjhn : real;
  signal yvjvnyrfp : time;
begin
  padx : entity work.eoyanxmwdn
    port map (ulqqu => yvjvnyrfp, qkbp => darfhjhn, xprqrdxvd => yvjvnyrfp);
  oaxyxga : entity work.eoyanxmwdn
    port map (ulqqu => ejlptrk, qkbp => vj, xprqrdxvd => pgvpt);
  oiotlxwf : entity work.eoyanxmwdn
    port map (ulqqu => pgvpt, qkbp => zekjhqzrb, xprqrdxvd => pgvpt);
end xuiyszjff;



entity iwbw is
  port (adkykebc : linkage integer);
end iwbw;



architecture drzzild of iwbw is
  signal oxujydyv : time;
  signal fzzf : real;
  signal ohwcscwqk : time;
  signal npyo : time;
  signal enn : real;
  signal qobcatgi : time;
begin
  sbr : entity work.eoyanxmwdn
    port map (ulqqu => qobcatgi, qkbp => enn, xprqrdxvd => npyo);
  xq : entity work.eoyanxmwdn
    port map (ulqqu => npyo, qkbp => enn, xprqrdxvd => ohwcscwqk);
  vov : entity work.eoyanxmwdn
    port map (ulqqu => ohwcscwqk, qkbp => fzzf, xprqrdxvd => oxujydyv);
end drzzild;

library ieee;
use ieee.std_logic_1164.all;

entity gdxs is
  port (xaxcfuzjn : linkage real; femv : buffer real; dga : out std_logic; jtzuyqzgny : linkage severity_level);
end gdxs;



architecture f of gdxs is
  signal rwkwepz : time;
  signal zb : real;
  signal qpcjfxns : integer;
  signal bbo : time;
  signal tpsab : time;
  signal ymkoak : time;
  signal sxgfiu : real;
  signal j : time;
begin
  mlmtrcur : entity work.eoyanxmwdn
    port map (ulqqu => j, qkbp => sxgfiu, xprqrdxvd => ymkoak);
  otcf : entity work.eoyanxmwdn
    port map (ulqqu => tpsab, qkbp => sxgfiu, xprqrdxvd => bbo);
  qkriw : entity work.iwbw
    port map (adkykebc => qpcjfxns);
  dmofxtxzd : entity work.eoyanxmwdn
    port map (ulqqu => ymkoak, qkbp => zb, xprqrdxvd => rwkwepz);
end f;



-- Seed after: 15778119071784113243,3820899062418988741
