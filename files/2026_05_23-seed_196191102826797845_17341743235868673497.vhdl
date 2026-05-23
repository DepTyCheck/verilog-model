-- Seed: 196191102826797845,17341743235868673497

library ieee;
use ieee.std_logic_1164.all;

entity mbgbd is
  port (rjwqu : in bit; wkjhtv : out std_logic; wofoqej : inout time; itqykz : buffer integer);
end mbgbd;



architecture nlokiyexi of mbgbd is
  
begin
  
end nlokiyexi;



entity gou is
  port (sledfehxv : out severity_level; zuicjxn : buffer boolean);
end gou;

library ieee;
use ieee.std_logic_1164.all;

architecture rkve of gou is
  signal jlurtve : integer;
  signal ciykyc : time;
  signal eykjx : std_logic;
  signal tdcvd : bit;
  signal ct : integer;
  signal xbv : time;
  signal rdi : std_logic;
  signal hmgqkmkara : bit;
begin
  bci : entity work.mbgbd
    port map (rjwqu => hmgqkmkara, wkjhtv => rdi, wofoqej => xbv, itqykz => ct);
  vguogl : entity work.mbgbd
    port map (rjwqu => tdcvd, wkjhtv => eykjx, wofoqej => ciykyc, itqykz => jlurtve);
end rkve;



-- Seed after: 488350268553575092,17341743235868673497
