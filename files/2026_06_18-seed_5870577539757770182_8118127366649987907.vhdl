-- Seed: 5870577539757770182,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (ukv : out integer; obu : linkage bit_vector(3 downto 1); wtpneyesc : in std_logic_vector(4 downto 4); bvmjkvy : linkage real);
end n;

architecture oyrzrxdpuy of n is
  
begin
  -- Single-driven assignments
  ukv <= 1_1_1_2_0;
end oyrzrxdpuy;

entity fftxt is
  port (svlud : in real);
end fftxt;

library ieee;
use ieee.std_logic_1164.all;

architecture z of fftxt is
  signal sczjfnhkw : real;
  signal gzl : bit_vector(3 downto 1);
  signal wfwhsbzfq : integer;
  signal otcijyrpjc : real;
  signal p : bit_vector(3 downto 1);
  signal dko : integer;
  signal pixt : real;
  signal fyyc : std_logic_vector(4 downto 4);
  signal ehvinmfocg : bit_vector(3 downto 1);
  signal w : integer;
begin
  qtkdqaq : entity work.n
    port map (ukv => w, obu => ehvinmfocg, wtpneyesc => fyyc, bvmjkvy => pixt);
  yegozuf : entity work.n
    port map (ukv => dko, obu => p, wtpneyesc => fyyc, bvmjkvy => otcijyrpjc);
  csrd : entity work.n
    port map (ukv => wfwhsbzfq, obu => gzl, wtpneyesc => fyyc, bvmjkvy => sczjfnhkw);
end z;

library ieee;
use ieee.std_logic_1164.all;

entity tbvdyropp is
  port (vjs : buffer severity_level; hefrrg : in time_vector(4 to 3); adwkmydx : buffer std_logic; i : buffer time);
end tbvdyropp;

library ieee;
use ieee.std_logic_1164.all;

architecture mvrf of tbvdyropp is
  signal xkhadsigjq : std_logic_vector(4 downto 4);
  signal dvwnxzcco : bit_vector(3 downto 1);
  signal lbjhzlqean : integer;
  signal soj : real;
  signal enebeimh : std_logic_vector(4 downto 4);
  signal uqlbwiv : bit_vector(3 downto 1);
  signal s : integer;
  signal kqhemdsrt : real;
  signal bbfei : real;
  signal qikps : std_logic_vector(4 downto 4);
  signal navtlyytra : bit_vector(3 downto 1);
  signal nk : integer;
begin
  mglzxz : entity work.n
    port map (ukv => nk, obu => navtlyytra, wtpneyesc => qikps, bvmjkvy => bbfei);
  decxuahiax : entity work.fftxt
    port map (svlud => kqhemdsrt);
  eqo : entity work.n
    port map (ukv => s, obu => uqlbwiv, wtpneyesc => enebeimh, bvmjkvy => soj);
  tpt : entity work.n
    port map (ukv => lbjhzlqean, obu => dvwnxzcco, wtpneyesc => xkhadsigjq, bvmjkvy => kqhemdsrt);
  
  -- Single-driven assignments
  vjs <= NOTE;
  i <= 8#0_3_3_3_3.7# ms;
  
  -- Multi-driven assignments
  enebeimh <= (others => 'H');
  adwkmydx <= 'L';
  enebeimh <= (others => '0');
  adwkmydx <= 'L';
end mvrf;

entity uxpboqznb is
  port (w : linkage integer; ammmezt : inout bit);
end uxpboqznb;

architecture x of uxpboqznb is
  
begin
  -- Single-driven assignments
  ammmezt <= '0';
end x;



-- Seed after: 674246640171735548,8118127366649987907
