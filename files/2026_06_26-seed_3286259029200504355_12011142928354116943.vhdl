-- Seed: 3286259029200504355,12011142928354116943

entity fveykwxg is
  port (cjual : linkage time_vector(4 to 1); cppybntq : linkage time_vector(1 downto 0); iiew : inout time; py : linkage bit);
end fveykwxg;

architecture cbq of fveykwxg is
  
begin
  -- Single-driven assignments
  iiew <= 8#6.0_0_1# ps;
end cbq;

library ieee;
use ieee.std_logic_1164.all;

entity rvguuh is
  port (niajqqld : buffer time; r : buffer std_logic);
end rvguuh;

architecture cgj of rvguuh is
  signal uuwzmw : bit;
  signal dm : time_vector(1 downto 0);
  signal ixu : time_vector(4 to 1);
begin
  pbcupurkkl : entity work.fveykwxg
    port map (cjual => ixu, cppybntq => dm, iiew => niajqqld, py => uuwzmw);
  
  -- Multi-driven assignments
  r <= 'X';
  r <= '1';
  r <= 'W';
end cgj;

entity qoipwqtam is
  port (zzg : out boolean_vector(1 downto 4));
end qoipwqtam;

library ieee;
use ieee.std_logic_1164.all;

architecture yyuccuuns of qoipwqtam is
  signal xghzxfna : std_logic;
  signal amkvqsnko : time;
begin
  b : entity work.rvguuh
    port map (niajqqld => amkvqsnko, r => xghzxfna);
  
  -- Single-driven assignments
  zzg <= (others => TRUE);
  
  -- Multi-driven assignments
  xghzxfna <= '-';
end yyuccuuns;



-- Seed after: 15887577521501627806,12011142928354116943
