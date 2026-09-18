-- Seed: 17404029861265918689,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity mgsoi is
  port (pohdaz : inout std_logic_vector(4 downto 3); rl : inout real);
end mgsoi;

architecture q of mgsoi is
  
begin
  -- Single-driven assignments
  rl <= rl;
end q;

entity rmkvuoq is
  port (pft : in time_vector(0 to 2));
end rmkvuoq;

library ieee;
use ieee.std_logic_1164.all;

architecture hgxw of rmkvuoq is
  signal eitlq : real;
  signal yppdmkb : std_logic_vector(4 downto 3);
  signal hfsomife : real;
  signal jcbemysr : std_logic_vector(4 downto 3);
begin
  oommagwroo : entity work.mgsoi
    port map (pohdaz => jcbemysr, rl => hfsomife);
  iwucz : entity work.mgsoi
    port map (pohdaz => yppdmkb, rl => eitlq);
  
  -- Multi-driven assignments
  jcbemysr <= jcbemysr;
end hgxw;

library ieee;
use ieee.std_logic_1164.all;

entity uwv is
  port (h : inout std_logic; tlfw : buffer std_logic_vector(1 to 2); kszmutyfh : in std_logic);
end uwv;

architecture gygzdo of uwv is
  signal kw : real;
  signal dcekkly : real;
  signal oxhgicvpdr : time_vector(0 to 2);
begin
  mgpr : entity work.rmkvuoq
    port map (pft => oxhgicvpdr);
  ovy : entity work.mgsoi
    port map (pohdaz => tlfw, rl => dcekkly);
  xxu : entity work.mgsoi
    port map (pohdaz => tlfw, rl => kw);
  
  -- Single-driven assignments
  oxhgicvpdr <= oxhgicvpdr;
  
  -- Multi-driven assignments
  h <= '-';
  h <= kszmutyfh;
end gygzdo;



-- Seed after: 11344928539731599365,3316342841050048249
