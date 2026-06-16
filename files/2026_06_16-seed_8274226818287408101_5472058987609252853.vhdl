-- Seed: 8274226818287408101,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity bjrbak is
  port (emv : buffer std_logic_vector(4 to 3); ac : inout real; w : linkage std_logic);
end bjrbak;

architecture vfrh of bjrbak is
  
begin
  -- Single-driven assignments
  ac <= 2.2_0_4;
  
  -- Multi-driven assignments
  emv <= "";
end vfrh;

library ieee;
use ieee.std_logic_1164.all;

entity fu is
  port (ixsuqi : inout real_vector(2 downto 1); hl : linkage std_logic_vector(0 to 4); nkjrz : buffer severity_level; cqf : inout real);
end fu;

library ieee;
use ieee.std_logic_1164.all;

architecture gbszwjpx of fu is
  signal u : std_logic_vector(4 to 3);
  signal oci : std_logic;
  signal mbqc : real;
  signal dcbsh : std_logic_vector(4 to 3);
  signal vnijmnhpfy : std_logic;
  signal i : real;
  signal fm : std_logic;
  signal lnmvavels : real;
  signal zugpasyk : std_logic_vector(4 to 3);
begin
  xob : entity work.bjrbak
    port map (emv => zugpasyk, ac => lnmvavels, w => fm);
  byn : entity work.bjrbak
    port map (emv => zugpasyk, ac => i, w => vnijmnhpfy);
  mssqeu : entity work.bjrbak
    port map (emv => dcbsh, ac => mbqc, w => oci);
  cpntjksm : entity work.bjrbak
    port map (emv => u, ac => cqf, w => oci);
  
  -- Single-driven assignments
  ixsuqi <= (16#E_8_6_0.0964#, 8#7.3_7_3_5#);
  nkjrz <= ERROR;
  
  -- Multi-driven assignments
  zugpasyk <= (others => '0');
  fm <= 'U';
  fm <= 'U';
  zugpasyk <= "";
end gbszwjpx;



-- Seed after: 17138678229690697305,5472058987609252853
