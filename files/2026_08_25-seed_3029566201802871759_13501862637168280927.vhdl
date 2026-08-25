-- Seed: 3029566201802871759,13501862637168280927

entity cagnwalm is
  port (xje : inout time_vector(2 to 2));
end cagnwalm;

architecture xq of cagnwalm is
  
begin
  -- Single-driven assignments
  xje <= (others => 12.0_2_4_1 us);
end xq;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (vtds : inout real; rmldcsahg : buffer time; bkum : buffer std_logic_vector(1 downto 1); ealf : linkage time_vector(2 downto 4));
end z;

architecture zhfuozpeba of z is
  
begin
  -- Single-driven assignments
  rmldcsahg <= 2.1214 fs;
  vtds <= vtds;
  
  -- Multi-driven assignments
  bkum <= bkum;
  bkum <= bkum;
  bkum <= "-";
end zhfuozpeba;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (otaqzmxz : inout time; tqyjqdvfoc : linkage std_logic_vector(0 to 0); qepmantcgd : inout integer);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture wgwbi of j is
  signal hmnjts : time_vector(2 downto 4);
  signal mfrxyi : std_logic_vector(1 downto 1);
  signal xgyfpeuo : real;
  signal batljnvgq : time_vector(2 to 2);
  signal lqvop : time_vector(2 to 2);
begin
  cg : entity work.cagnwalm
    port map (xje => lqvop);
  qn : entity work.cagnwalm
    port map (xje => batljnvgq);
  qrnixpb : entity work.z
    port map (vtds => xgyfpeuo, rmldcsahg => otaqzmxz, bkum => mfrxyi, ealf => hmnjts);
  
  -- Multi-driven assignments
  mfrxyi <= mfrxyi;
end wgwbi;



-- Seed after: 16767183043297565535,13501862637168280927
