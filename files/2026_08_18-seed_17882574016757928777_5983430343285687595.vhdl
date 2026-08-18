-- Seed: 17882574016757928777,5983430343285687595

entity uic is
  port (bcs : inout time_vector(2 downto 1); mleyquvt : out time; lmifnpx : buffer severity_level; ugeqini : inout integer);
end uic;

architecture t of uic is
  
begin
  -- Single-driven assignments
  bcs <= (232 us, 16#03B3E# ns);
  mleyquvt <= 2#1.0# ns;
end t;

entity oqkmaywuvl is
  port (cqbrpqbyj : linkage integer);
end oqkmaywuvl;

architecture ilmzfskt of oqkmaywuvl is
  signal k : integer;
  signal fygby : severity_level;
  signal adztp : time;
  signal pukysoi : time_vector(2 downto 1);
  signal ahc : integer;
  signal hckt : severity_level;
  signal lioujwx : time;
  signal lrlkrtk : time_vector(2 downto 1);
begin
  fbkwm : entity work.uic
    port map (bcs => lrlkrtk, mleyquvt => lioujwx, lmifnpx => hckt, ugeqini => ahc);
  edhm : entity work.uic
    port map (bcs => pukysoi, mleyquvt => adztp, lmifnpx => fygby, ugeqini => k);
end ilmzfskt;

library ieee;
use ieee.std_logic_1164.all;

entity jm is
  port (ektnue : linkage std_logic_vector(2 downto 2); vykuniat : in integer; dxdm : linkage integer);
end jm;

architecture ohzduwsbm of jm is
  signal jeljphqdty : integer;
  signal sw : severity_level;
  signal cb : time;
  signal jrp : time_vector(2 downto 1);
begin
  cepochv : entity work.uic
    port map (bcs => jrp, mleyquvt => cb, lmifnpx => sw, ugeqini => jeljphqdty);
end ohzduwsbm;

library ieee;
use ieee.std_logic_1164.all;

entity ixcy is
  port (jqfxau : buffer std_logic_vector(0 to 1); xyyg : inout time);
end ixcy;

library ieee;
use ieee.std_logic_1164.all;

architecture vbycqcso of ixcy is
  signal yieja : integer;
  signal mveynfko : std_logic_vector(2 downto 2);
  signal gxqrqep : integer;
begin
  ezrlzw : entity work.oqkmaywuvl
    port map (cqbrpqbyj => gxqrqep);
  zrrf : entity work.jm
    port map (ektnue => mveynfko, vykuniat => yieja, dxdm => yieja);
  
  -- Single-driven assignments
  xyyg <= xyyg;
  
  -- Multi-driven assignments
  mveynfko <= "1";
  jqfxau <= "-H";
end vbycqcso;



-- Seed after: 10002172266671346069,5983430343285687595
