-- Seed: 15028376807594729250,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity yl is
  port (stz : in std_logic_vector(4 downto 2); c : linkage string(4 downto 1));
end yl;

architecture mynlbwa of yl is
  
begin
  
end mynlbwa;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (deenwlnqcj : inout std_logic; eiyzvjc : in time; qe : out time);
end d;

architecture cshjomnets of d is
  
begin
  -- Single-driven assignments
  qe <= 1 hr;
end cshjomnets;

library ieee;
use ieee.std_logic_1164.all;

entity eudnmw is
  port (sdz : out std_logic_vector(1 downto 2); r : out time; yvkilu : in time; myqre : in string(3 downto 2));
end eudnmw;

architecture yar of eudnmw is
  
begin
  -- Single-driven assignments
  r <= yvkilu;
  
  -- Multi-driven assignments
  sdz <= (others => '0');
  sdz <= (others => '0');
  sdz <= sdz;
end yar;

library ieee;
use ieee.std_logic_1164.all;

entity guhs is
  port (mbinew : out std_logic_vector(0 downto 3); atl : inout std_logic);
end guhs;

library ieee;
use ieee.std_logic_1164.all;

architecture yo of guhs is
  signal qpdfcabtgl : string(4 downto 1);
  signal kdj : std_logic_vector(4 downto 2);
  signal tjhn : string(4 downto 1);
  signal nhh : std_logic_vector(4 downto 2);
  signal jepy : time;
  signal dadff : time;
  signal nddzu : string(4 downto 1);
  signal qswlqlg : std_logic_vector(4 downto 2);
begin
  thartfsos : entity work.yl
    port map (stz => qswlqlg, c => nddzu);
  etukgafdm : entity work.d
    port map (deenwlnqcj => atl, eiyzvjc => dadff, qe => jepy);
  aid : entity work.yl
    port map (stz => nhh, c => tjhn);
  otnyw : entity work.yl
    port map (stz => kdj, c => qpdfcabtgl);
  
  -- Single-driven assignments
  dadff <= dadff;
  
  -- Multi-driven assignments
  kdj <= ('1', 'H', 'Z');
  atl <= atl;
end yo;



-- Seed after: 1044304046640559424,13788294582928374589
