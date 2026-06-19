-- Seed: 3314521191609017231,3108530264173481209

entity flig is
  port (tvpfnxes : inout bit);
end flig;

architecture mfjyvclh of flig is
  
begin
  -- Single-driven assignments
  tvpfnxes <= '1';
end mfjyvclh;

library ieee;
use ieee.std_logic_1164.all;

entity wvelx is
  port (yeloe : linkage real; yxrxcqpg : out bit; jtgtdq : in std_logic; wt : in real_vector(4 downto 0));
end wvelx;

architecture h of wvelx is
  signal otkzmkyj : bit;
  signal s : bit;
  signal rt : bit;
begin
  fjodt : entity work.flig
    port map (tvpfnxes => rt);
  dtrtpil : entity work.flig
    port map (tvpfnxes => s);
  bdm : entity work.flig
    port map (tvpfnxes => yxrxcqpg);
  o : entity work.flig
    port map (tvpfnxes => otkzmkyj);
end h;

entity nhtusadu is
  port (hcdzpvelpm : in real; qp : in real_vector(4 downto 2));
end nhtusadu;

architecture gqqgsdnq of nhtusadu is
  signal mmacxkx : bit;
  signal dk : bit;
  signal oqhwxa : bit;
begin
  dr : entity work.flig
    port map (tvpfnxes => oqhwxa);
  rvofvgv : entity work.flig
    port map (tvpfnxes => dk);
  hcgiyopd : entity work.flig
    port map (tvpfnxes => mmacxkx);
end gqqgsdnq;

library ieee;
use ieee.std_logic_1164.all;

entity rpqv is
  port (gn : out std_logic_vector(2 to 3); hvpux : inout integer; kjlybb : inout real; nlnq : inout std_logic_vector(2 downto 2));
end rpqv;

library ieee;
use ieee.std_logic_1164.all;

architecture y of rpqv is
  signal ej : bit;
  signal vol : real_vector(4 downto 2);
  signal oxjf : real;
  signal dycboq : real_vector(4 downto 0);
  signal ti : std_logic;
  signal mxwtm : bit;
  signal ik : real;
begin
  fyshuld : entity work.wvelx
    port map (yeloe => ik, yxrxcqpg => mxwtm, jtgtdq => ti, wt => dycboq);
  abpls : entity work.nhtusadu
    port map (hcdzpvelpm => oxjf, qp => vol);
  dckstlu : entity work.nhtusadu
    port map (hcdzpvelpm => ik, qp => vol);
  gcs : entity work.flig
    port map (tvpfnxes => ej);
  
  -- Single-driven assignments
  kjlybb <= 16#F.11F4#;
  dycboq <= (4004.4_3_0, 16#F_1_7_2.0_9#, 16#9.8_C#, 1_4_4.1, 04342.20);
  vol <= (2#1.0_1_1_0_0#, 13.3, 16#C_F_A.B#);
  hvpux <= 8#2_3_7_7_5#;
  
  -- Multi-driven assignments
  ti <= '-';
  ti <= 'X';
  gn <= ('W', '-');
end y;



-- Seed after: 10235922111877473504,3108530264173481209
