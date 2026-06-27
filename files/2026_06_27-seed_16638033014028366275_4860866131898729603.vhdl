-- Seed: 16638033014028366275,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity wjq is
  port (jsnnl : inout integer_vector(0 to 2); ukoqfapc : buffer time; vf : inout std_logic_vector(1 to 1); f : out time);
end wjq;

architecture legrcccy of wjq is
  
begin
  -- Single-driven assignments
  f <= 2#1_0.100# us;
  jsnnl <= (8#221#, 1, 16#B1AE4#);
  ukoqfapc <= 1 ps;
  
  -- Multi-driven assignments
  vf <= (others => '0');
  vf <= (others => '-');
end legrcccy;

entity u is
  port (wasomzuqf : in time; qzjstn : out integer_vector(4 to 1));
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture siefo of u is
  signal tnpezyqf : time;
  signal peiiqkwn : std_logic_vector(1 to 1);
  signal xjmmqygtgh : time;
  signal pcferdmid : integer_vector(0 to 2);
  signal o : time;
  signal lrgceeyag : std_logic_vector(1 to 1);
  signal kbjbkqnmbq : time;
  signal ycgofp : integer_vector(0 to 2);
begin
  djfumg : entity work.wjq
    port map (jsnnl => ycgofp, ukoqfapc => kbjbkqnmbq, vf => lrgceeyag, f => o);
  ehdxxzxf : entity work.wjq
    port map (jsnnl => pcferdmid, ukoqfapc => xjmmqygtgh, vf => peiiqkwn, f => tnpezyqf);
  
  -- Single-driven assignments
  qzjstn <= (others => 0);
  
  -- Multi-driven assignments
  lrgceeyag <= (others => 'L');
  lrgceeyag <= "-";
  peiiqkwn <= "0";
end siefo;



-- Seed after: 17160150150776716111,4860866131898729603
