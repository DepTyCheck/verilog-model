-- Seed: 9311443671097816037,8421704836678237495

entity ahwuknx is
  port (ssnseomgm : inout boolean_vector(4 to 2));
end ahwuknx;

architecture ejebrrvnj of ahwuknx is
  
begin
  -- Single-driven assignments
  ssnseomgm <= (others => TRUE);
end ejebrrvnj;

library ieee;
use ieee.std_logic_1164.all;

entity imhcjslcoy is
  port (ez : out std_logic);
end imhcjslcoy;

architecture gykl of imhcjslcoy is
  signal uxnee : boolean_vector(4 to 2);
  signal mweo : boolean_vector(4 to 2);
  signal kw : boolean_vector(4 to 2);
begin
  gbk : entity work.ahwuknx
    port map (ssnseomgm => kw);
  mvnebrh : entity work.ahwuknx
    port map (ssnseomgm => mweo);
  mrjcmrc : entity work.ahwuknx
    port map (ssnseomgm => uxnee);
  
  -- Multi-driven assignments
  ez <= '1';
  ez <= 'W';
end gykl;



-- Seed after: 1354334259515098847,8421704836678237495
