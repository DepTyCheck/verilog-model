-- Seed: 7593330207699424069,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity konltpjg is
  port (bppkdyci : in time; srgqcej : inout std_logic; dzblpxpmcz : out integer);
end konltpjg;

architecture bdtz of konltpjg is
  
begin
  -- Single-driven assignments
  dzblpxpmcz <= 4_0_2_2;
  
  -- Multi-driven assignments
  srgqcej <= srgqcej;
  srgqcej <= srgqcej;
  srgqcej <= 'U';
  srgqcej <= 'Z';
end bdtz;

entity wnl is
  port (qvhru : in character);
end wnl;

library ieee;
use ieee.std_logic_1164.all;

architecture vszp of wnl is
  signal yxi : integer;
  signal hmuzmtujh : std_logic;
  signal osfgh : time;
  signal mxccfscn : integer;
  signal ykalssbuc : integer;
  signal cjyxgtsv : std_logic;
  signal cogugrjn : time;
begin
  vaxjkdf : entity work.konltpjg
    port map (bppkdyci => cogugrjn, srgqcej => cjyxgtsv, dzblpxpmcz => ykalssbuc);
  umw : entity work.konltpjg
    port map (bppkdyci => cogugrjn, srgqcej => cjyxgtsv, dzblpxpmcz => mxccfscn);
  jcwkbe : entity work.konltpjg
    port map (bppkdyci => osfgh, srgqcej => hmuzmtujh, dzblpxpmcz => yxi);
  
  -- Single-driven assignments
  cogugrjn <= 2 ms;
  osfgh <= 30 ps;
  
  -- Multi-driven assignments
  cjyxgtsv <= 'L';
  cjyxgtsv <= 'W';
  cjyxgtsv <= 'H';
end vszp;



-- Seed after: 2014906097773036363,9900838946149614537
