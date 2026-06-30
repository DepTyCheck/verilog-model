-- Seed: 5532929467364702904,14629254427735353553

entity cstqdnxfw is
  port (afotu : buffer real; h : out severity_level; ncgjam : in real);
end cstqdnxfw;

architecture lbzk of cstqdnxfw is
  
begin
  -- Single-driven assignments
  afotu <= 2#10.0_1_1#;
  h <= FAILURE;
end lbzk;

entity eaytnsox is
  port (rjkytmcgmg : in real);
end eaytnsox;

architecture yel of eaytnsox is
  
begin
  
end yel;

library ieee;
use ieee.std_logic_1164.all;

entity phiduhdp is
  port (enjkdlo : buffer time_vector(2 to 0); t : out boolean; oclec : linkage time; gotrmswyie : inout std_logic);
end phiduhdp;

architecture m of phiduhdp is
  
begin
  -- Single-driven assignments
  t <= FALSE;
end m;

entity jk is
  port (xlc : linkage time; jpwdvncnj : in string(1 downto 3));
end jk;

library ieee;
use ieee.std_logic_1164.all;

architecture rh of jk is
  signal ygrdwjakxr : real;
  signal eg : severity_level;
  signal awx : real;
  signal wnggucqfbd : std_logic;
  signal eftylvb : boolean;
  signal dx : time_vector(2 to 0);
begin
  owiyvy : entity work.phiduhdp
    port map (enjkdlo => dx, t => eftylvb, oclec => xlc, gotrmswyie => wnggucqfbd);
  pgwnkd : entity work.eaytnsox
    port map (rjkytmcgmg => awx);
  lb : entity work.cstqdnxfw
    port map (afotu => awx, h => eg, ncgjam => ygrdwjakxr);
  
  -- Single-driven assignments
  ygrdwjakxr <= 3_0_3_1_1.0;
  
  -- Multi-driven assignments
  wnggucqfbd <= 'H';
  wnggucqfbd <= 'L';
  wnggucqfbd <= '-';
  wnggucqfbd <= 'U';
end rh;



-- Seed after: 5445180972741556951,14629254427735353553
