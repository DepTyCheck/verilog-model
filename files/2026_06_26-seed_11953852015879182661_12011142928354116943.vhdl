-- Seed: 11953852015879182661,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity qqhpi is
  port (nvvpfnejeb : out std_logic_vector(1 downto 1); d : out std_logic; amouxb : inout bit; omabpkc : in integer);
end qqhpi;

architecture nshazukj of qqhpi is
  
begin
  -- Multi-driven assignments
  d <= 'H';
end nshazukj;

entity mt is
  port (otgenxzmb : in time; thxhls : inout real_vector(4 downto 2); rrw : out bit; suakob : linkage time);
end mt;

library ieee;
use ieee.std_logic_1164.all;

architecture p of mt is
  signal kxy : integer;
  signal ypulxliqy : std_logic_vector(1 downto 1);
  signal octdgkubde : integer;
  signal mfizhuykyu : bit;
  signal f : std_logic;
  signal spkzhijtii : integer;
  signal fbkhuptiut : bit;
  signal akpk : integer;
  signal thktw : bit;
  signal qm : std_logic;
  signal mhbanuoza : std_logic_vector(1 downto 1);
begin
  odc : entity work.qqhpi
    port map (nvvpfnejeb => mhbanuoza, d => qm, amouxb => thktw, omabpkc => akpk);
  lel : entity work.qqhpi
    port map (nvvpfnejeb => mhbanuoza, d => qm, amouxb => fbkhuptiut, omabpkc => spkzhijtii);
  bsvctnvh : entity work.qqhpi
    port map (nvvpfnejeb => mhbanuoza, d => f, amouxb => mfizhuykyu, omabpkc => octdgkubde);
  up : entity work.qqhpi
    port map (nvvpfnejeb => ypulxliqy, d => qm, amouxb => rrw, omabpkc => kxy);
  
  -- Single-driven assignments
  thxhls <= (0_3_3_4_3.141, 2#00111.10001#, 13.3014);
  
  -- Multi-driven assignments
  mhbanuoza <= "U";
end p;



-- Seed after: 14856115532917364219,12011142928354116943
