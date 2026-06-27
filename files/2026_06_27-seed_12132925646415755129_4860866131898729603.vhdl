-- Seed: 12132925646415755129,4860866131898729603

entity t is
  port (yjyczlr : buffer real; pilxxwt : out real; jb : in time);
end t;

architecture cigvguf of t is
  
begin
  
end cigvguf;

library ieee;
use ieee.std_logic_1164.all;

entity yhywnlp is
  port (qvhqcbjane : inout std_logic; hltqa : inout std_logic; x : inout time);
end yhywnlp;

architecture qmnugxwbio of yhywnlp is
  signal y : real;
  signal gtgg : real;
  signal ernrulw : time;
  signal u : real;
  signal ypemhdp : real;
  signal kymgqzq : time;
  signal okqrpodv : real;
  signal xvcbs : real;
begin
  tmzesozxg : entity work.t
    port map (yjyczlr => xvcbs, pilxxwt => okqrpodv, jb => kymgqzq);
  awjmd : entity work.t
    port map (yjyczlr => ypemhdp, pilxxwt => u, jb => ernrulw);
  eta : entity work.t
    port map (yjyczlr => gtgg, pilxxwt => y, jb => ernrulw);
  
  -- Single-driven assignments
  x <= 2#1001# ms;
  ernrulw <= 331 ps;
  kymgqzq <= 2#0_0_0_0.0_1# us;
  
  -- Multi-driven assignments
  qvhqcbjane <= 'Z';
  hltqa <= 'L';
  qvhqcbjane <= 'U';
end qmnugxwbio;

library ieee;
use ieee.std_logic_1164.all;

entity qvjvpxaxu is
  port (spqdo : in std_logic; qtqtqh : out time);
end qvjvpxaxu;

library ieee;
use ieee.std_logic_1164.all;

architecture jkplynll of qvjvpxaxu is
  signal ty : time;
  signal mg : real;
  signal d : real;
  signal qmcbjrbvi : time;
  signal f : real;
  signal nt : real;
  signal wowwco : time;
  signal kf : std_logic;
  signal tsxmggnev : std_logic;
begin
  uucexhled : entity work.yhywnlp
    port map (qvhqcbjane => tsxmggnev, hltqa => kf, x => wowwco);
  kxkqpu : entity work.t
    port map (yjyczlr => nt, pilxxwt => f, jb => qmcbjrbvi);
  xsdsh : entity work.t
    port map (yjyczlr => d, pilxxwt => mg, jb => ty);
  yhqgeck : entity work.yhywnlp
    port map (qvhqcbjane => tsxmggnev, hltqa => tsxmggnev, x => qtqtqh);
  
  -- Single-driven assignments
  ty <= 4 sec;
  qmcbjrbvi <= 8#55# us;
  
  -- Multi-driven assignments
  kf <= '0';
  kf <= '1';
  tsxmggnev <= '1';
  tsxmggnev <= '-';
end jkplynll;

library ieee;
use ieee.std_logic_1164.all;

entity akvac is
  port (clsj : inout std_logic_vector(4 to 4); hbqebw : out std_logic; cmsvdr : out integer_vector(2 downto 0));
end akvac;

architecture esavcar of akvac is
  signal cikz : real;
  signal btpsmr : real;
  signal aq : time;
  signal cpsxl : real;
  signal n : real;
  signal e : real;
  signal omvile : real;
  signal wlnqbi : time;
  signal fwakfwj : real;
  signal mt : real;
begin
  swpt : entity work.t
    port map (yjyczlr => mt, pilxxwt => fwakfwj, jb => wlnqbi);
  eujesw : entity work.t
    port map (yjyczlr => omvile, pilxxwt => e, jb => wlnqbi);
  qolcz : entity work.t
    port map (yjyczlr => n, pilxxwt => cpsxl, jb => aq);
  nvtigub : entity work.t
    port map (yjyczlr => btpsmr, pilxxwt => cikz, jb => aq);
  
  -- Single-driven assignments
  cmsvdr <= (2#0#, 8#4247#, 0_0_4);
  aq <= 3_2_4_1.31 us;
  wlnqbi <= 2_0_1 us;
  
  -- Multi-driven assignments
  clsj <= "1";
  hbqebw <= 'X';
end esavcar;



-- Seed after: 6978452199936127462,4860866131898729603
