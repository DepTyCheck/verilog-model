-- Seed: 6321616874941355896,14141408946471626091

entity wetsicdwjc is
  port (kkzhwrw : in boolean; dqzwgb : linkage time_vector(4 to 3));
end wetsicdwjc;

architecture lap of wetsicdwjc is
  
begin
  
end lap;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (s : in std_logic_vector(2 to 4); bagjirloe : linkage integer; kqgrle : buffer std_logic; azgen : in bit_vector(4 to 4));
end b;

architecture shzoca of b is
  signal ywpa : time_vector(4 to 3);
  signal ggony : boolean;
  signal nieckf : time_vector(4 to 3);
  signal nbcnbqdchr : boolean;
begin
  xaqaanpfv : entity work.wetsicdwjc
    port map (kkzhwrw => nbcnbqdchr, dqzwgb => nieckf);
  amqydfh : entity work.wetsicdwjc
    port map (kkzhwrw => ggony, dqzwgb => ywpa);
  
  -- Single-driven assignments
  ggony <= nbcnbqdchr;
  nbcnbqdchr <= FALSE;
  
  -- Multi-driven assignments
  kqgrle <= kqgrle;
  kqgrle <= 'X';
  kqgrle <= 'Z';
  kqgrle <= '0';
end shzoca;



-- Seed after: 3974546996637258472,14141408946471626091
