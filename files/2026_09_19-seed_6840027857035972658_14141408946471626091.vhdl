-- Seed: 6840027857035972658,14141408946471626091

entity ven is
  port (yivvbsin : in real);
end ven;

architecture i of ven is
  
begin
  
end i;

entity nbfxck is
  port (wmhynxqqo : inout bit; vxtb : buffer integer; dzsgygs : out time; yvpsm : buffer time_vector(0 to 4));
end nbfxck;

architecture ydmd of nbfxck is
  signal yynuegr : real;
begin
  hqyksgbun : entity work.ven
    port map (yivvbsin => yynuegr);
  iowzqtpz : entity work.ven
    port map (yivvbsin => yynuegr);
  gx : entity work.ven
    port map (yivvbsin => yynuegr);
  
  -- Single-driven assignments
  yvpsm <= yvpsm;
end ydmd;

entity hbzpqux is
  port (oq : in real);
end hbzpqux;

architecture plpea of hbzpqux is
  
begin
  
end plpea;

library ieee;
use ieee.std_logic_1164.all;

entity xmiyi is
  port (ntoe : out real; prgwb : buffer std_logic; izzenggq : in std_logic_vector(1 to 0); bhnm : buffer real);
end xmiyi;

architecture zaxxekyh of xmiyi is
  signal zxtltfy : time_vector(0 to 4);
  signal oegzabrlou : time;
  signal bf : integer;
  signal jtasymg : bit;
  signal xzkzofhht : time_vector(0 to 4);
  signal dfcjskdzke : time;
  signal heurejsxub : integer;
  signal jmumqbye : bit;
  signal ynvpqszo : real;
begin
  ehoi : entity work.ven
    port map (yivvbsin => ynvpqszo);
  rfiuulkg : entity work.nbfxck
    port map (wmhynxqqo => jmumqbye, vxtb => heurejsxub, dzsgygs => dfcjskdzke, yvpsm => xzkzofhht);
  egdk : entity work.nbfxck
    port map (wmhynxqqo => jtasymg, vxtb => bf, dzsgygs => oegzabrlou, yvpsm => zxtltfy);
  
  -- Single-driven assignments
  ntoe <= 1323.0_1;
  ynvpqszo <= 2#1_1_0_0_0.0_0_1#;
  
  -- Multi-driven assignments
  prgwb <= 'H';
  prgwb <= 'L';
end zaxxekyh;



-- Seed after: 18098714685034645007,14141408946471626091
