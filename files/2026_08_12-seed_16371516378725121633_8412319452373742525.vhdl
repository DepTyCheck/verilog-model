-- Seed: 16371516378725121633,8412319452373742525

entity doyjxckw is
  port (povuhej : buffer bit_vector(4 to 1); hqu : in real; qtkhgnfmy : buffer bit_vector(0 to 1); psgbmcqyo : inout integer);
end doyjxckw;

architecture ekncctsyxt of doyjxckw is
  
begin
  -- Single-driven assignments
  psgbmcqyo <= 0;
end ekncctsyxt;

entity bzojphgt is
  port (dzdkriit : inout integer_vector(4 downto 4));
end bzojphgt;

architecture ssavgi of bzojphgt is
  signal zc : integer;
  signal xtdsmmm : bit_vector(0 to 1);
  signal qmjsdajlgt : real;
  signal zviiise : bit_vector(4 to 1);
begin
  idjd : entity work.doyjxckw
    port map (povuhej => zviiise, hqu => qmjsdajlgt, qtkhgnfmy => xtdsmmm, psgbmcqyo => zc);
end ssavgi;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (aiwkvq : in std_logic; qbjsdcfe : inout std_logic; kf : in std_logic; isdemiksp : buffer std_logic);
end t;

architecture me of t is
  signal vxmzeoayu : integer;
  signal vnsjp : bit_vector(0 to 1);
  signal rpaxixnd : real;
  signal nk : bit_vector(4 to 1);
begin
  lnqk : entity work.doyjxckw
    port map (povuhej => nk, hqu => rpaxixnd, qtkhgnfmy => vnsjp, psgbmcqyo => vxmzeoayu);
  
  -- Single-driven assignments
  rpaxixnd <= 1_0_2_1_4.3_0_4_2_1;
  
  -- Multi-driven assignments
  isdemiksp <= 'U';
  qbjsdcfe <= isdemiksp;
end me;



-- Seed after: 13696113205357006883,8412319452373742525
