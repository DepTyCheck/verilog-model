-- Seed: 6735556672310728636,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity fqbsezd is
  port (owdjnmtmrg : out std_logic_vector(4 downto 4); c : in time; prqhfa : in boolean);
end fqbsezd;

architecture wkk of fqbsezd is
  
begin
  
end wkk;

library ieee;
use ieee.std_logic_1164.all;

entity dgkaxffjrc is
  port (bi : out std_logic; hpx : linkage boolean; amm : in character; vwrgez : linkage time);
end dgkaxffjrc;

library ieee;
use ieee.std_logic_1164.all;

architecture gmzrvj of dgkaxffjrc is
  signal kkxq : time;
  signal cwlpdz : boolean;
  signal yfek : boolean;
  signal gjifsiml : time;
  signal wjfedb : std_logic_vector(4 downto 4);
begin
  piorz : entity work.fqbsezd
    port map (owdjnmtmrg => wjfedb, c => gjifsiml, prqhfa => yfek);
  gcqsm : entity work.fqbsezd
    port map (owdjnmtmrg => wjfedb, c => gjifsiml, prqhfa => cwlpdz);
  hovidzsv : entity work.fqbsezd
    port map (owdjnmtmrg => wjfedb, c => kkxq, prqhfa => yfek);
  mdck : entity work.fqbsezd
    port map (owdjnmtmrg => wjfedb, c => gjifsiml, prqhfa => yfek);
  
  -- Multi-driven assignments
  bi <= bi;
end gmzrvj;

library ieee;
use ieee.std_logic_1164.all;

entity jgsxxg is
  port (tgkkxcb : inout time; gs : buffer time_vector(4 downto 0); qjuqsa : buffer std_logic);
end jgsxxg;

library ieee;
use ieee.std_logic_1164.all;

architecture czmfc of jgsxxg is
  signal rrqulf : std_logic_vector(4 downto 4);
  signal oab : boolean;
  signal octnilyc : time;
  signal hnwq : std_logic_vector(4 downto 4);
begin
  exnwjrey : entity work.fqbsezd
    port map (owdjnmtmrg => hnwq, c => octnilyc, prqhfa => oab);
  unqulyxs : entity work.fqbsezd
    port map (owdjnmtmrg => rrqulf, c => tgkkxcb, prqhfa => oab);
  
  -- Single-driven assignments
  oab <= oab;
  gs <= gs;
  octnilyc <= 1_2 ms;
end czmfc;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (o : out real; xtyxb : linkage real; gkp : out std_logic_vector(2 downto 4); oo : in std_logic_vector(0 to 0));
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture bmamiouvrh of w is
  signal udhhasxsp : time;
  signal lyhorzpcb : character;
  signal wbqnxl : boolean;
  signal oqh : std_logic;
  signal axe : time_vector(4 downto 0);
  signal azej : time;
begin
  zr : entity work.jgsxxg
    port map (tgkkxcb => azej, gs => axe, qjuqsa => oqh);
  ewljz : entity work.dgkaxffjrc
    port map (bi => oqh, hpx => wbqnxl, amm => lyhorzpcb, vwrgez => udhhasxsp);
  
  -- Single-driven assignments
  lyhorzpcb <= 'l';
  o <= o;
  
  -- Multi-driven assignments
  gkp <= "";
  gkp <= gkp;
  oqh <= oqh;
  gkp <= (others => '0');
end bmamiouvrh;



-- Seed after: 253524990604291432,10594830431004325987
