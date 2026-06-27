-- Seed: 2418220525231860960,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity fmbv is
  port (efnu : out time; mo : inout std_logic; katkzr : inout bit);
end fmbv;

architecture hkls of fmbv is
  
begin
  -- Single-driven assignments
  katkzr <= '1';
  efnu <= 3_0_1_2_2 us;
  
  -- Multi-driven assignments
  mo <= 'L';
  mo <= 'X';
  mo <= 'U';
  mo <= 'W';
end hkls;

library ieee;
use ieee.std_logic_1164.all;

entity mr is
  port (ibvz : linkage std_logic_vector(4 downto 2); fmctyf : in integer; dt : inout time);
end mr;

library ieee;
use ieee.std_logic_1164.all;

architecture xpntg of mr is
  signal vkkf : bit;
  signal pysufrz : std_logic;
begin
  d : entity work.fmbv
    port map (efnu => dt, mo => pysufrz, katkzr => vkkf);
  
  -- Multi-driven assignments
  pysufrz <= 'X';
  pysufrz <= '1';
  pysufrz <= 'X';
  pysufrz <= '-';
end xpntg;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (tfbmztr : inout std_logic_vector(0 to 2); g : linkage std_logic);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture rlpp of f is
  signal xnxgtkljj : bit;
  signal t : std_logic;
  signal yloe : time;
  signal z : bit;
  signal umnkfldmwf : std_logic;
  signal vlzbagdlsq : time;
  signal sknjnmqqt : time;
  signal yvhaegvmov : integer;
  signal klnmkzo : std_logic_vector(4 downto 2);
begin
  hsx : entity work.mr
    port map (ibvz => klnmkzo, fmctyf => yvhaegvmov, dt => sknjnmqqt);
  gihwazr : entity work.fmbv
    port map (efnu => vlzbagdlsq, mo => umnkfldmwf, katkzr => z);
  uuravsvrnf : entity work.fmbv
    port map (efnu => yloe, mo => t, katkzr => xnxgtkljj);
  
  -- Single-driven assignments
  yvhaegvmov <= 4303;
  
  -- Multi-driven assignments
  umnkfldmwf <= '1';
  tfbmztr <= "1Z-";
end rlpp;



-- Seed after: 62166392435533142,4860866131898729603
