-- Seed: 10822895746113204404,15141888397681078541

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (z : in std_logic; betlmqbu : inout std_logic; c : in time; tywxq : inout time);
end l;



architecture hpkt of l is
  
begin
  
end hpkt;



entity rkwtjpu is
  port (gv : out bit);
end rkwtjpu;

library ieee;
use ieee.std_logic_1164.all;

architecture rjkvyysu of rkwtjpu is
  signal chblsysfjl : time;
  signal rsqmic : time;
  signal djenpjtkzi : time;
  signal fyi : std_logic;
  signal vcuwfvu : std_logic;
begin
  tiuadtxaru : entity work.l
    port map (z => vcuwfvu, betlmqbu => fyi, c => djenpjtkzi, tywxq => rsqmic);
  trmhxfmlu : entity work.l
    port map (z => vcuwfvu, betlmqbu => vcuwfvu, c => djenpjtkzi, tywxq => djenpjtkzi);
  wuxiakri : entity work.l
    port map (z => vcuwfvu, betlmqbu => vcuwfvu, c => chblsysfjl, tywxq => chblsysfjl);
end rjkvyysu;

library ieee;
use ieee.std_logic_1164.all;

entity ianqmpwsr is
  port (wtvtcwvgfw : linkage severity_level; evueoo : linkage std_logic; oeqcqji : inout time);
end ianqmpwsr;

library ieee;
use ieee.std_logic_1164.all;

architecture u of ianqmpwsr is
  signal ood : std_logic;
  signal v : bit;
  signal dnwyo : bit;
begin
  ojcsl : entity work.rkwtjpu
    port map (gv => dnwyo);
  rglnodrhwv : entity work.rkwtjpu
    port map (gv => v);
  umnxc : entity work.l
    port map (z => ood, betlmqbu => ood, c => oeqcqji, tywxq => oeqcqji);
end u;



-- Seed after: 8248069990921478378,15141888397681078541
