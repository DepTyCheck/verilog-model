-- Seed: 743653401408982446,16461708287571398341

entity nhw is
  port (jpovr : inout boolean_vector(1 to 3));
end nhw;

architecture mpeoomzjon of nhw is
  
begin
  
end mpeoomzjon;

library ieee;
use ieee.std_logic_1164.all;

entity kmj is
  port (ggfv : inout boolean_vector(2 downto 0); bqg : in std_logic_vector(1 downto 3));
end kmj;

architecture ycmkqeo of kmj is
  
begin
  w : entity work.nhw
    port map (jpovr => ggfv);
end ycmkqeo;

entity jmiwqtexs is
  port (cjgwdl : linkage real_vector(0 to 3); uhy : buffer time; srcmhbm : inout severity_level; hoojrhnkf : in integer);
end jmiwqtexs;

architecture iksvorg of jmiwqtexs is
  signal lfaixy : boolean_vector(1 to 3);
begin
  zxx : entity work.nhw
    port map (jpovr => lfaixy);
  
  -- Single-driven assignments
  uhy <= 1 hr;
  srcmhbm <= WARNING;
end iksvorg;

library ieee;
use ieee.std_logic_1164.all;

entity nqldliotg is
  port (kalrx : inout std_logic; vju : in std_logic_vector(1 to 3); hsk : buffer time_vector(4 downto 2); ojccezgg : out real);
end nqldliotg;

architecture wpdiijzol of nqldliotg is
  signal i : boolean_vector(1 to 3);
begin
  egrj : entity work.nhw
    port map (jpovr => i);
  
  -- Single-driven assignments
  ojccezgg <= 341.4_2_0_3_0;
  hsk <= (16#B.256E8# ps, 2#0_0_1_1_0# ps, 2#00# ms);
  
  -- Multi-driven assignments
  kalrx <= kalrx;
end wpdiijzol;



-- Seed after: 13016578137197674732,16461708287571398341
