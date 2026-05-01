-- Seed: 17238204555487387138,779721673283024895



entity otxd is
  port (birgzhzqu : in integer; trolhtdnxr : linkage integer);
end otxd;



architecture tlkxk of otxd is
  
begin
  
end tlkxk;



entity siljvx is
  port (heb : linkage integer; oiywsh : linkage time);
end siljvx;



architecture mtlvfed of siljvx is
  signal vtjhyrnmhq : integer;
begin
  pyjxlksnrr : entity work.otxd
    port map (birgzhzqu => vtjhyrnmhq, trolhtdnxr => heb);
end mtlvfed;

library ieee;
use ieee.std_logic_1164.all;

entity didqqhmdya is
  port (la : out integer; ttcdnxonm : in integer; eahrzylfek : linkage real; envibva : out std_logic);
end didqqhmdya;



architecture lehyjjs of didqqhmdya is
  signal kpzwf : integer;
begin
  kclspyoh : entity work.otxd
    port map (birgzhzqu => ttcdnxonm, trolhtdnxr => kpzwf);
  rgicrger : entity work.otxd
    port map (birgzhzqu => ttcdnxonm, trolhtdnxr => la);
  zi : entity work.otxd
    port map (birgzhzqu => la, trolhtdnxr => ttcdnxonm);
end lehyjjs;



entity akf is
  port (huayfsmf : buffer bit; llqtbwxnm : buffer time);
end akf;

library ieee;
use ieee.std_logic_1164.all;

architecture rpgxq of akf is
  signal xt : std_logic;
  signal akpxvv : real;
  signal yrafwak : integer;
begin
  tjvepwc : entity work.didqqhmdya
    port map (la => yrafwak, ttcdnxonm => yrafwak, eahrzylfek => akpxvv, envibva => xt);
  yvnkhhu : entity work.otxd
    port map (birgzhzqu => yrafwak, trolhtdnxr => yrafwak);
end rpgxq;



-- Seed after: 12861043292071668799,779721673283024895
