-- Seed: 13496516557918745477,12431929723978769775

library ieee;
use ieee.std_logic_1164.all;

entity cqkhis is
  port (vjwga : buffer severity_level; mdmmzo : linkage std_logic_vector(0 to 0); kcwlfuhz : inout time; awulwaangy : out boolean);
end cqkhis;



architecture bvdbz of cqkhis is
  
begin
  
end bvdbz;

library ieee;
use ieee.std_logic_1164.all;

entity zlxoco is
  port (nvgj : buffer std_logic; bnn : out real);
end zlxoco;

library ieee;
use ieee.std_logic_1164.all;

architecture nernl of zlxoco is
  signal loxftitud : boolean;
  signal lzyelysqy : time;
  signal xqa : std_logic_vector(0 to 0);
  signal bzlhy : severity_level;
  signal netukgvo : boolean;
  signal gtfab : time;
  signal nxjrb : std_logic_vector(0 to 0);
  signal xyn : severity_level;
begin
  nwwtzzo : entity work.cqkhis
    port map (vjwga => xyn, mdmmzo => nxjrb, kcwlfuhz => gtfab, awulwaangy => netukgvo);
  jmvjz : entity work.cqkhis
    port map (vjwga => bzlhy, mdmmzo => xqa, kcwlfuhz => lzyelysqy, awulwaangy => loxftitud);
end nernl;



entity vmw is
  port (nl : out integer);
end vmw;

library ieee;
use ieee.std_logic_1164.all;

architecture hknsi of vmw is
  signal assve : boolean;
  signal qpqhnlzhyt : time;
  signal yumrkuix : std_logic_vector(0 to 0);
  signal yxofqzoueu : severity_level;
  signal hkuiugoih : boolean;
  signal vrydebbsux : time;
  signal xaary : std_logic_vector(0 to 0);
  signal gyzv : severity_level;
  signal lqoe : real;
  signal dcdnyzdn : std_logic;
  signal jpgji : boolean;
  signal yvxz : time;
  signal jty : std_logic_vector(0 to 0);
  signal dtd : severity_level;
begin
  asxvcrz : entity work.cqkhis
    port map (vjwga => dtd, mdmmzo => jty, kcwlfuhz => yvxz, awulwaangy => jpgji);
  u : entity work.zlxoco
    port map (nvgj => dcdnyzdn, bnn => lqoe);
  gr : entity work.cqkhis
    port map (vjwga => gyzv, mdmmzo => xaary, kcwlfuhz => vrydebbsux, awulwaangy => hkuiugoih);
  vyeb : entity work.cqkhis
    port map (vjwga => yxofqzoueu, mdmmzo => yumrkuix, kcwlfuhz => qpqhnlzhyt, awulwaangy => assve);
end hknsi;

library ieee;
use ieee.std_logic_1164.all;

entity jdwjxc is
  port (mppauzfsc : out std_logic_vector(3 to 4); pvsqmwc : buffer integer_vector(3 downto 0));
end jdwjxc;



architecture drf of jdwjxc is
  signal kbsc : integer;
begin
  ud : entity work.vmw
    port map (nl => kbsc);
end drf;



-- Seed after: 11329680103683885722,12431929723978769775
