-- Seed: 10699103395986406280,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (ggp : linkage time; jlq : inout integer; wpznf : out std_logic_vector(3 downto 3); cfrragguh : out std_logic);
end a;

architecture qrry of a is
  
begin
  -- Single-driven assignments
  jlq <= 2#1_0#;
  
  -- Multi-driven assignments
  cfrragguh <= '1';
  cfrragguh <= 'X';
  wpznf <= "W";
end qrry;

entity icsw is
  port (lvzdkplos : inout severity_level; hwbmdzers : buffer integer; danckshz : out time);
end icsw;

library ieee;
use ieee.std_logic_1164.all;

architecture bfpzhlom of icsw is
  signal o : std_logic;
  signal uwvrkro : std_logic_vector(3 downto 3);
  signal mazqmtgbaf : integer;
  signal ejsk : time;
  signal rrc : integer;
  signal ffgzeis : time;
  signal g : std_logic;
  signal bzvguuob : std_logic_vector(3 downto 3);
  signal cjmhdyv : integer;
  signal plp : time;
begin
  rbdoxdcz : entity work.a
    port map (ggp => plp, jlq => cjmhdyv, wpznf => bzvguuob, cfrragguh => g);
  sitxd : entity work.a
    port map (ggp => ffgzeis, jlq => rrc, wpznf => bzvguuob, cfrragguh => g);
  jpfauqjexo : entity work.a
    port map (ggp => ejsk, jlq => mazqmtgbaf, wpznf => uwvrkro, cfrragguh => g);
  bambs : entity work.a
    port map (ggp => danckshz, jlq => hwbmdzers, wpznf => bzvguuob, cfrragguh => o);
  
  -- Single-driven assignments
  lvzdkplos <= ERROR;
  
  -- Multi-driven assignments
  o <= 'Z';
  bzvguuob <= (others => 'H');
  g <= 'Z';
end bfpzhlom;

library ieee;
use ieee.std_logic_1164.all;

entity axzbfihr is
  port (jgvi : linkage severity_level; fo : inout std_logic_vector(2 downto 0); kz : linkage std_logic_vector(4 downto 3));
end axzbfihr;

architecture fcv of axzbfihr is
  
begin
  -- Multi-driven assignments
  fo <= fo;
  fo <= fo;
  fo <= fo;
end fcv;



-- Seed after: 12769911865503148939,511364357853360275
