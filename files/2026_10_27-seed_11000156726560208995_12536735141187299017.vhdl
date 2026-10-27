-- Seed: 11000156726560208995,12536735141187299017

entity ibmhv is
  port (dtj : buffer time_vector(0 to 0); lowf : in severity_level; vwlub : in real);
end ibmhv;

architecture qk of ibmhv is
  
begin
  -- Single-driven assignments
  dtj <= dtj;
end qk;

entity lvx is
  port (wzkovrjuep : out time);
end lvx;

architecture idgoxlpp of lvx is
  signal yywqkev : severity_level;
  signal sszraldh : time_vector(0 to 0);
  signal zwqks : real;
  signal ytaaauajg : severity_level;
  signal dcfuua : time_vector(0 to 0);
begin
  jpkqsqfjac : entity work.ibmhv
    port map (dtj => dcfuua, lowf => ytaaauajg, vwlub => zwqks);
  bvzji : entity work.ibmhv
    port map (dtj => sszraldh, lowf => yywqkev, vwlub => zwqks);
  
  -- Single-driven assignments
  wzkovrjuep <= 4 fs;
  yywqkev <= ytaaauajg;
  zwqks <= zwqks;
  ytaaauajg <= yywqkev;
end idgoxlpp;

entity glvbqh is
  port (chiamvon : in integer);
end glvbqh;

architecture wedyrexrzp of glvbqh is
  signal tffx : real;
  signal kjwrbgbr : severity_level;
  signal hnvwpag : time_vector(0 to 0);
  signal dudmvvda : time;
begin
  rsc : entity work.lvx
    port map (wzkovrjuep => dudmvvda);
  y : entity work.ibmhv
    port map (dtj => hnvwpag, lowf => kjwrbgbr, vwlub => tffx);
  
  -- Single-driven assignments
  kjwrbgbr <= kjwrbgbr;
end wedyrexrzp;

library ieee;
use ieee.std_logic_1164.all;

entity adnug is
  port (npjhth : linkage time; dhol : out std_logic; ctb : buffer real_vector(1 downto 0); hyatbu : out std_logic_vector(4 downto 1));
end adnug;

architecture gycu of adnug is
  signal wvwk : real;
  signal mgs : severity_level;
  signal uy : time_vector(0 to 0);
  signal oprywmp : real;
  signal rtrrcyv : severity_level;
  signal hyii : time_vector(0 to 0);
begin
  qgpggjs : entity work.ibmhv
    port map (dtj => hyii, lowf => rtrrcyv, vwlub => oprywmp);
  hwjhamklj : entity work.ibmhv
    port map (dtj => uy, lowf => mgs, vwlub => wvwk);
  
  -- Single-driven assignments
  ctb <= ctb;
  oprywmp <= 4.3_0;
  wvwk <= 16#1B.B#;
end gycu;



-- Seed after: 14939144016496579905,12536735141187299017
