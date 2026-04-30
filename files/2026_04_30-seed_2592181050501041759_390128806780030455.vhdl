-- Seed: 2592181050501041759,390128806780030455

library ieee;
use ieee.std_logic_1164.all;

entity ekksmbepar is
  port (yausqbma : linkage integer; wpsukfw : in std_logic; dncxhtxale : linkage integer);
end ekksmbepar;



architecture zfzhwoa of ekksmbepar is
  
begin
  
end zfzhwoa;



entity gf is
  port (brgnnkssf : out time; uxry : linkage time);
end gf;

library ieee;
use ieee.std_logic_1164.all;

architecture jen of gf is
  signal ogtry : integer;
  signal bymvs : std_logic;
  signal clweagp : integer;
begin
  ffpdtqt : entity work.ekksmbepar
    port map (yausqbma => clweagp, wpsukfw => bymvs, dncxhtxale => clweagp);
  mnsjkamoou : entity work.ekksmbepar
    port map (yausqbma => clweagp, wpsukfw => bymvs, dncxhtxale => ogtry);
end jen;



entity e is
  port (pf : out time; iydjby : inout time);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture vitpdd of e is
  signal iscgytah : integer;
  signal bchtx : time;
  signal jqfzqm : integer;
  signal slrv : std_logic;
  signal b : integer;
begin
  fivjsvpz : entity work.ekksmbepar
    port map (yausqbma => b, wpsukfw => slrv, dncxhtxale => jqfzqm);
  gnbwkccyd : entity work.gf
    port map (brgnnkssf => iydjby, uxry => bchtx);
  w : entity work.ekksmbepar
    port map (yausqbma => iscgytah, wpsukfw => slrv, dncxhtxale => iscgytah);
end vitpdd;



-- Seed after: 4956293472893844122,390128806780030455
