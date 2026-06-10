-- Seed: 10631257141475707230,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity nhzskuis is
  port (pjzyarbxl : out time; vpaqtnscy : inout std_logic_vector(0 downto 2); byrmzrbhe : in time; rateqeeed : linkage std_logic_vector(2 to 2));
end nhzskuis;



architecture kvs of nhzskuis is
  
begin
  
end kvs;



entity jnb is
  port (dsdtkf : linkage time);
end jnb;

library ieee;
use ieee.std_logic_1164.all;

architecture lzrhyhhb of jnb is
  signal suuybru : std_logic_vector(2 to 2);
  signal qk : std_logic_vector(0 downto 2);
  signal jhobfhv : time;
  signal cxw : std_logic_vector(0 downto 2);
  signal ettsasvkyz : time;
  signal dv : std_logic_vector(2 to 2);
  signal rbioabixwa : time;
  signal jwq : std_logic_vector(2 to 2);
  signal ylvfoz : time;
  signal bplfyomhn : std_logic_vector(0 downto 2);
  signal uzvoe : time;
begin
  skcavgx : entity work.nhzskuis
    port map (pjzyarbxl => uzvoe, vpaqtnscy => bplfyomhn, byrmzrbhe => ylvfoz, rateqeeed => jwq);
  zmkajcotxw : entity work.nhzskuis
    port map (pjzyarbxl => ylvfoz, vpaqtnscy => bplfyomhn, byrmzrbhe => rbioabixwa, rateqeeed => dv);
  kdivdy : entity work.nhzskuis
    port map (pjzyarbxl => ettsasvkyz, vpaqtnscy => cxw, byrmzrbhe => uzvoe, rateqeeed => jwq);
  zzj : entity work.nhzskuis
    port map (pjzyarbxl => jhobfhv, vpaqtnscy => qk, byrmzrbhe => uzvoe, rateqeeed => suuybru);
end lzrhyhhb;



entity jzvikdtjx is
  port (dxq : inout real_vector(3 downto 1); q : buffer bit_vector(2 to 4));
end jzvikdtjx;



architecture lucjq of jzvikdtjx is
  signal zhsosxahr : time;
begin
  n : entity work.jnb
    port map (dsdtkf => zhsosxahr);
end lucjq;



-- Seed after: 13835919664027918998,5415160250146859793
