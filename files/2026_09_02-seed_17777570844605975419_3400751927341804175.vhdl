-- Seed: 17777570844605975419,3400751927341804175

entity utjnsdcrt is
  port (uxhdtr : inout real; rviqiqle : inout bit; mhpfwtjrg : linkage bit_vector(0 downto 2));
end utjnsdcrt;

architecture ofg of utjnsdcrt is
  
begin
  
end ofg;

library ieee;
use ieee.std_logic_1164.all;

entity sk is
  port (maqwazfmy : inout std_logic; lmgh : linkage severity_level; p : inout std_logic);
end sk;

architecture qsjmixo of sk is
  signal lgu : bit_vector(0 downto 2);
  signal ogeqku : bit;
  signal dz : real;
  signal a : bit_vector(0 downto 2);
  signal vkrv : bit;
  signal o : real;
  signal ir : bit_vector(0 downto 2);
  signal rulkvctkf : bit;
  signal ofiymkbg : real;
begin
  uip : entity work.utjnsdcrt
    port map (uxhdtr => ofiymkbg, rviqiqle => rulkvctkf, mhpfwtjrg => ir);
  vyl : entity work.utjnsdcrt
    port map (uxhdtr => o, rviqiqle => vkrv, mhpfwtjrg => a);
  fvlmduk : entity work.utjnsdcrt
    port map (uxhdtr => dz, rviqiqle => ogeqku, mhpfwtjrg => lgu);
end qsjmixo;

entity dsotx is
  port (hycqt : in integer);
end dsotx;

architecture a of dsotx is
  
begin
  
end a;

entity cbfvo is
  port (xqjrd : buffer real);
end cbfvo;

architecture q of cbfvo is
  signal oyx : bit_vector(0 downto 2);
  signal cn : bit;
  signal pyhvadk : bit_vector(0 downto 2);
  signal ufjivk : bit;
  signal dlpdyh : real;
  signal bk : integer;
begin
  bje : entity work.dsotx
    port map (hycqt => bk);
  toxhqtyjr : entity work.utjnsdcrt
    port map (uxhdtr => dlpdyh, rviqiqle => ufjivk, mhpfwtjrg => pyhvadk);
  bnbwtce : entity work.utjnsdcrt
    port map (uxhdtr => xqjrd, rviqiqle => cn, mhpfwtjrg => oyx);
  
  -- Single-driven assignments
  bk <= 0_2_3_3;
end q;



-- Seed after: 898581787857153090,3400751927341804175
