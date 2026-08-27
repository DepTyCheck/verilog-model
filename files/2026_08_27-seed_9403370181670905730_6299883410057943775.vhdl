-- Seed: 9403370181670905730,6299883410057943775

entity orugl is
  port (msktahtioz : in real; uflntak : buffer string(3 to 1); gyonyc : inout integer; hi : in time);
end orugl;

architecture x of orugl is
  
begin
  -- Single-driven assignments
  gyonyc <= 2#11#;
  uflntak <= (others => ' ');
end x;

library ieee;
use ieee.std_logic_1164.all;

entity bdmbzls is
  port (ciwsglu : out bit; yk : in time; frfrt : out std_logic_vector(3 downto 2));
end bdmbzls;

architecture dl of bdmbzls is
  
begin
  -- Single-driven assignments
  ciwsglu <= '0';
  
  -- Multi-driven assignments
  frfrt <= frfrt;
end dl;

library ieee;
use ieee.std_logic_1164.all;

entity bg is
  port (zed : in bit_vector(2 to 1); uagxyr : in std_logic);
end bg;

library ieee;
use ieee.std_logic_1164.all;

architecture npwaotfntb of bg is
  signal nxmhv : integer;
  signal semc : string(3 to 1);
  signal syovycwtvs : real;
  signal rod : std_logic_vector(3 downto 2);
  signal mdwydtc : time;
  signal za : bit;
begin
  bd : entity work.bdmbzls
    port map (ciwsglu => za, yk => mdwydtc, frfrt => rod);
  dihtmhgwk : entity work.orugl
    port map (msktahtioz => syovycwtvs, uflntak => semc, gyonyc => nxmhv, hi => mdwydtc);
  
  -- Single-driven assignments
  mdwydtc <= 8#4.334# ms;
  syovycwtvs <= syovycwtvs;
  
  -- Multi-driven assignments
  rod <= rod;
  rod <= ('H', 'U');
  rod <= "-H";
  rod <= rod;
end npwaotfntb;



-- Seed after: 15740027864984347279,6299883410057943775
