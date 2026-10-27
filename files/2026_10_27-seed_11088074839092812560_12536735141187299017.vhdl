-- Seed: 11088074839092812560,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity wvelt is
  port (tzdaqx : buffer std_logic);
end wvelt;

architecture yiqzfx of wvelt is
  
begin
  -- Multi-driven assignments
  tzdaqx <= '1';
  tzdaqx <= tzdaqx;
  tzdaqx <= '0';
end yiqzfx;

entity pt is
  port (ktebwlocbb : inout time; m : buffer time; wakjzyitt : out bit);
end pt;

library ieee;
use ieee.std_logic_1164.all;

architecture dxumzoeiec of pt is
  signal tfkibeekrq : std_logic;
  signal ikue : std_logic;
begin
  ndpxrs : entity work.wvelt
    port map (tzdaqx => ikue);
  kxaxixp : entity work.wvelt
    port map (tzdaqx => tfkibeekrq);
  w : entity work.wvelt
    port map (tzdaqx => ikue);
  kahyhpni : entity work.wvelt
    port map (tzdaqx => ikue);
  
  -- Single-driven assignments
  ktebwlocbb <= 2#0_0# ms;
  m <= ktebwlocbb;
  wakjzyitt <= '1';
  
  -- Multi-driven assignments
  tfkibeekrq <= 'X';
end dxumzoeiec;



-- Seed after: 17205747674800288935,12536735141187299017
