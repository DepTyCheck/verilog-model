-- Seed: 13036995036790324416,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity qjudlijn is
  port (lcodnb : buffer std_logic_vector(2 downto 1); xpgoggszwm : in time; doptwui : in integer; x : buffer real);
end qjudlijn;

architecture mkfuvxqxg of qjudlijn is
  
begin
  -- Single-driven assignments
  x <= 2#0.0#;
end mkfuvxqxg;

entity wu is
  port (xqbdld : in time_vector(1 downto 3); pryurp : out bit; yulrkop : buffer severity_level);
end wu;

library ieee;
use ieee.std_logic_1164.all;

architecture gatgemu of wu is
  signal dk : real;
  signal urjnm : real;
  signal s : integer;
  signal vmlyrln : time;
  signal grxt : std_logic_vector(2 downto 1);
begin
  u : entity work.qjudlijn
    port map (lcodnb => grxt, xpgoggszwm => vmlyrln, doptwui => s, x => urjnm);
  wgjuaxnmzh : entity work.qjudlijn
    port map (lcodnb => grxt, xpgoggszwm => vmlyrln, doptwui => s, x => dk);
  
  -- Single-driven assignments
  pryurp <= pryurp;
  yulrkop <= yulrkop;
  
  -- Multi-driven assignments
  grxt <= grxt;
  grxt <= grxt;
end gatgemu;



-- Seed after: 16302025005659981434,6379010654866854599
