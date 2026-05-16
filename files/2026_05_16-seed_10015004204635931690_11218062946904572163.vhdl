-- Seed: 10015004204635931690,11218062946904572163

library ieee;
use ieee.std_logic_1164.all;

entity ukrvh is
  port (qiwjumiht : linkage std_logic; axqexzy : inout integer);
end ukrvh;



architecture zhk of ukrvh is
  
begin
  
end zhk;



entity v is
  port (fzths : inout integer; knovtdvszm : out real; wywawj : inout time; mjlmynq : out severity_level);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture seay of v is
  signal wxxqu : std_logic;
  signal fjq : integer;
  signal fyfmgzznpx : std_logic;
  signal edjw : integer;
  signal zogs : integer;
  signal zmlf : std_logic;
begin
  wcm : entity work.ukrvh
    port map (qiwjumiht => zmlf, axqexzy => zogs);
  dsvvwqxwwe : entity work.ukrvh
    port map (qiwjumiht => zmlf, axqexzy => edjw);
  dqlvk : entity work.ukrvh
    port map (qiwjumiht => fyfmgzznpx, axqexzy => fjq);
  isbmj : entity work.ukrvh
    port map (qiwjumiht => wxxqu, axqexzy => fzths);
end seay;



entity qwwwrktknp is
  port (mutgyzvd : buffer time);
end qwwwrktknp;

library ieee;
use ieee.std_logic_1164.all;

architecture nigcfgrses of qwwwrktknp is
  signal nvxfayl : severity_level;
  signal cmhmwuhm : real;
  signal yd : integer;
  signal cqfgklggtb : integer;
  signal mghnmpnj : std_logic;
begin
  tp : entity work.ukrvh
    port map (qiwjumiht => mghnmpnj, axqexzy => cqfgklggtb);
  xrtosy : entity work.v
    port map (fzths => yd, knovtdvszm => cmhmwuhm, wywawj => mutgyzvd, mjlmynq => nvxfayl);
end nigcfgrses;



entity t is
  port (jxbnbht : inout real; ddpogkijxn : in real; u : in character);
end t;



architecture jvvvu of t is
  signal q : time;
begin
  mgzljnvui : entity work.qwwwrktknp
    port map (mutgyzvd => q);
end jvvvu;



-- Seed after: 14839390607345019305,11218062946904572163
