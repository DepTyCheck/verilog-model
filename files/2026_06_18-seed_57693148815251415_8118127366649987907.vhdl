-- Seed: 57693148815251415,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity sdmkps is
  port (jbbs : in std_logic_vector(1 to 2); xxndpshum : buffer time_vector(1 downto 2); vfj : buffer real; vhcgyy : out string(1 to 5));
end sdmkps;

architecture xpwx of sdmkps is
  
begin
  -- Single-driven assignments
  vhcgyy <= ('h', 'p', 't', 'm', 's');
  xxndpshum <= (others => 0 ns);
  vfj <= 2#0.0_0_0_1#;
end xpwx;

library ieee;
use ieee.std_logic_1164.all;

entity wnrhzdr is
  port (at : in std_logic_vector(4 downto 2));
end wnrhzdr;

library ieee;
use ieee.std_logic_1164.all;

architecture ft of wnrhzdr is
  signal inltcvnjv : string(1 to 5);
  signal nxbbejfk : real;
  signal gxszg : time_vector(1 downto 2);
  signal xgdpd : std_logic_vector(1 to 2);
begin
  crfxzk : entity work.sdmkps
    port map (jbbs => xgdpd, xxndpshum => gxszg, vfj => nxbbejfk, vhcgyy => inltcvnjv);
  
  -- Multi-driven assignments
  xgdpd <= ('Z', 'H');
end ft;

entity n is
  port (qcjr : buffer real; dhsqzmrnxv : inout real_vector(4 downto 4));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture ur of n is
  signal rovzajhrm : string(1 to 5);
  signal pupskx : time_vector(1 downto 2);
  signal thxsjbjimc : std_logic_vector(1 to 2);
begin
  teagszxeq : entity work.sdmkps
    port map (jbbs => thxsjbjimc, xxndpshum => pupskx, vfj => qcjr, vhcgyy => rovzajhrm);
  
  -- Multi-driven assignments
  thxsjbjimc <= ('X', '-');
  thxsjbjimc <= "WW";
  thxsjbjimc <= "-W";
end ur;



-- Seed after: 14783869344067345033,8118127366649987907
