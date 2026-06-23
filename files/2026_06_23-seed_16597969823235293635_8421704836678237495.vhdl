-- Seed: 16597969823235293635,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity ultf is
  port (kf : inout std_logic_vector(4 to 4); b : buffer integer; nirhwjzuu : buffer time);
end ultf;

architecture s of ultf is
  
begin
  -- Single-driven assignments
  nirhwjzuu <= 2#1.1_0_0_1_1# ns;
  b <= 2143;
  
  -- Multi-driven assignments
  kf <= "U";
end s;

library ieee;
use ieee.std_logic_1164.all;

entity ch is
  port (rgzkj : linkage boolean; utromalnlx : inout time; zzettjml : out std_logic_vector(2 to 0); mgjxu : out real);
end ch;

library ieee;
use ieee.std_logic_1164.all;

architecture zmjog of ch is
  signal abxpyp : integer;
  signal jab : std_logic_vector(4 to 4);
begin
  qefgnhe : entity work.ultf
    port map (kf => jab, b => abxpyp, nirhwjzuu => utromalnlx);
  
  -- Multi-driven assignments
  zzettjml <= (others => '0');
  zzettjml <= (others => '0');
  zzettjml <= "";
end zmjog;



-- Seed after: 10871400441302286976,8421704836678237495
