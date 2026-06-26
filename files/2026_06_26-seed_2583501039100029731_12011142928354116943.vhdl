-- Seed: 2583501039100029731,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (jxscgunaq : out time; spjh : buffer std_logic_vector(2 downto 1); dziznhqvu : out real);
end v;

architecture zr of v is
  
begin
  -- Multi-driven assignments
  spjh <= "1U";
  spjh <= ('L', '0');
end zr;

entity oeraphrb is
  port (ysjlmxxv : inout boolean; d : buffer time; xbyjnqgjy : linkage real_vector(4 downto 1));
end oeraphrb;

library ieee;
use ieee.std_logic_1164.all;

architecture msoqwbbc of oeraphrb is
  signal mmuz : real;
  signal k : real;
  signal jd : time;
  signal nfqplobb : real;
  signal gtgmc : std_logic_vector(2 downto 1);
  signal ntnvnczl : time;
  signal nx : real;
  signal zbvtpnhueq : std_logic_vector(2 downto 1);
  signal ixmialyxtx : time;
begin
  ga : entity work.v
    port map (jxscgunaq => ixmialyxtx, spjh => zbvtpnhueq, dziznhqvu => nx);
  hzgkvokjzy : entity work.v
    port map (jxscgunaq => ntnvnczl, spjh => gtgmc, dziznhqvu => nfqplobb);
  y : entity work.v
    port map (jxscgunaq => jd, spjh => gtgmc, dziznhqvu => k);
  fmqcxrpwwv : entity work.v
    port map (jxscgunaq => d, spjh => gtgmc, dziznhqvu => mmuz);
  
  -- Single-driven assignments
  ysjlmxxv <= TRUE;
  
  -- Multi-driven assignments
  zbvtpnhueq <= ('X', 'H');
end msoqwbbc;



-- Seed after: 3438179408164790926,12011142928354116943
