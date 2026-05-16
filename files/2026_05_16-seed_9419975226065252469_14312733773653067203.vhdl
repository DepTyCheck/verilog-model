-- Seed: 9419975226065252469,14312733773653067203



entity pmq is
  port (ple : in real; otcjtz : linkage integer);
end pmq;



architecture xcdpvt of pmq is
  
begin
  
end xcdpvt;

library ieee;
use ieee.std_logic_1164.all;

entity jsk is
  port (g : out std_logic);
end jsk;



architecture xtjcr of jsk is
  signal mzce : integer;
  signal zvhkpdwul : real;
begin
  rub : entity work.pmq
    port map (ple => zvhkpdwul, otcjtz => mzce);
end xtjcr;

library ieee;
use ieee.std_logic_1164.all;

entity jtvrsb is
  port (hrinvjjqh : inout std_logic; fyvnhpxw : linkage std_logic);
end jtvrsb;



architecture nc of jtvrsb is
  signal o : real;
  signal ixmercet : integer;
  signal dvwkfusu : real;
begin
  ypl : entity work.jsk
    port map (g => hrinvjjqh);
  q : entity work.pmq
    port map (ple => dvwkfusu, otcjtz => ixmercet);
  twgrb : entity work.pmq
    port map (ple => o, otcjtz => ixmercet);
end nc;



-- Seed after: 15416065379813133745,14312733773653067203
