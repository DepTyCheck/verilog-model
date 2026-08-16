-- Seed: 7975140734633006545,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity ttkbiw is
  port (hvckbijtcf : buffer real; zl : inout std_logic_vector(2 to 2));
end ttkbiw;

architecture wikciyqke of ttkbiw is
  
begin
  -- Single-driven assignments
  hvckbijtcf <= 0_3.30;
  
  -- Multi-driven assignments
  zl <= "0";
  zl <= (others => 'Z');
end wikciyqke;

entity dhwzmwt is
  port (zugupp : in time);
end dhwzmwt;

library ieee;
use ieee.std_logic_1164.all;

architecture lrd of dhwzmwt is
  signal rxrk : std_logic_vector(2 to 2);
  signal unaojuw : real;
  signal o : std_logic_vector(2 to 2);
  signal kemmvard : real;
  signal lriyksn : std_logic_vector(2 to 2);
  signal fyqigbliw : real;
begin
  kn : entity work.ttkbiw
    port map (hvckbijtcf => fyqigbliw, zl => lriyksn);
  bamsyc : entity work.ttkbiw
    port map (hvckbijtcf => kemmvard, zl => o);
  somh : entity work.ttkbiw
    port map (hvckbijtcf => unaojuw, zl => rxrk);
  
  -- Multi-driven assignments
  lriyksn <= "W";
  rxrk <= lriyksn;
  lriyksn <= lriyksn;
end lrd;

entity vagrktk is
  port (tycq : inout integer; pwmbarhke : out time_vector(1 to 4));
end vagrktk;

library ieee;
use ieee.std_logic_1164.all;

architecture bwwups of vagrktk is
  signal ymwsyksfzf : std_logic_vector(2 to 2);
  signal lmuynfzyy : real;
begin
  ta : entity work.ttkbiw
    port map (hvckbijtcf => lmuynfzyy, zl => ymwsyksfzf);
  
  -- Single-driven assignments
  pwmbarhke <= (2#1_1_1.10# ms, 4 us, 8#7_1.7_7# us, 4 ms);
  tycq <= 16#6#;
end bwwups;



-- Seed after: 6155676384361270424,13857275728440271305
