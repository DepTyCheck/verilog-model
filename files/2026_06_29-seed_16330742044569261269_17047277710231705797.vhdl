-- Seed: 16330742044569261269,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity btinnsfvy is
  port (z : in time; p : out std_logic_vector(3 downto 1); nqhdxmdzpw : buffer time_vector(1 downto 4); akapx : out character);
end btinnsfvy;

architecture lrkmwkyegz of btinnsfvy is
  
begin
  -- Single-driven assignments
  akapx <= 'p';
  
  -- Multi-driven assignments
  p <= "W-X";
  p <= ('1', '-', 'L');
  p <= "W-1";
end lrkmwkyegz;

library ieee;
use ieee.std_logic_1164.all;

entity sx is
  port (uktd : linkage integer; sjqpbaacl : out bit; zzwil : out std_logic);
end sx;

library ieee;
use ieee.std_logic_1164.all;

architecture she of sx is
  signal fmjcpsn : character;
  signal koul : time_vector(1 downto 4);
  signal luupi : std_logic_vector(3 downto 1);
  signal gjyy : time;
begin
  pzv : entity work.btinnsfvy
    port map (z => gjyy, p => luupi, nqhdxmdzpw => koul, akapx => fmjcpsn);
  
  -- Single-driven assignments
  sjqpbaacl <= '1';
  gjyy <= 8#1573# ms;
end she;

library ieee;
use ieee.std_logic_1164.all;

entity xsadeam is
  port (rssscv : in std_logic_vector(4 downto 3); qhedtu : inout real);
end xsadeam;

library ieee;
use ieee.std_logic_1164.all;

architecture ibk of xsadeam is
  signal dfhpds : character;
  signal mlahqpsoll : time_vector(1 downto 4);
  signal nlsfbvtd : std_logic_vector(3 downto 1);
  signal lenvdc : time;
  signal yipcmf : character;
  signal cnzvtk : time_vector(1 downto 4);
  signal uvyhxzvxa : std_logic_vector(3 downto 1);
  signal igowbax : character;
  signal xkqov : time_vector(1 downto 4);
  signal xwdwaowcs : character;
  signal hxgtz : time_vector(1 downto 4);
  signal olu : std_logic_vector(3 downto 1);
  signal qann : time;
begin
  krwk : entity work.btinnsfvy
    port map (z => qann, p => olu, nqhdxmdzpw => hxgtz, akapx => xwdwaowcs);
  nnmj : entity work.btinnsfvy
    port map (z => qann, p => olu, nqhdxmdzpw => xkqov, akapx => igowbax);
  hdit : entity work.btinnsfvy
    port map (z => qann, p => uvyhxzvxa, nqhdxmdzpw => cnzvtk, akapx => yipcmf);
  umpgis : entity work.btinnsfvy
    port map (z => lenvdc, p => nlsfbvtd, nqhdxmdzpw => mlahqpsoll, akapx => dfhpds);
  
  -- Single-driven assignments
  qann <= 0 sec;
  lenvdc <= 2#0_1_0_1_1.10# ms;
  qhedtu <= 0_2_4_4_0.2;
  
  -- Multi-driven assignments
  olu <= ('1', '0', 'Z');
end ibk;



-- Seed after: 12890529412937795227,17047277710231705797
