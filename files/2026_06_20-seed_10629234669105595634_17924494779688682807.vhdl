-- Seed: 10629234669105595634,17924494779688682807

entity akintxv is
  port (dvsnt : buffer boolean; jkuun : linkage real_vector(3 downto 2));
end akintxv;

architecture jlhchyc of akintxv is
  
begin
  -- Single-driven assignments
  dvsnt <= TRUE;
end jlhchyc;

library ieee;
use ieee.std_logic_1164.all;

entity fegn is
  port (jdpwmds : linkage std_logic; lkfsr : buffer std_logic_vector(3 downto 0); rexbukyse : in real);
end fegn;

architecture vqqfymntix of fegn is
  signal vthqlfjpv : real_vector(3 downto 2);
  signal pvduue : boolean;
  signal tjtoq : real_vector(3 downto 2);
  signal wuqcfvo : boolean;
begin
  iwdzwlwz : entity work.akintxv
    port map (dvsnt => wuqcfvo, jkuun => tjtoq);
  atomzeufso : entity work.akintxv
    port map (dvsnt => pvduue, jkuun => vthqlfjpv);
  
  -- Multi-driven assignments
  lkfsr <= "LW1Z";
end vqqfymntix;

library ieee;
use ieee.std_logic_1164.all;

entity ma is
  port (hsfyzxdf : buffer std_logic_vector(2 to 1));
end ma;

library ieee;
use ieee.std_logic_1164.all;

architecture pju of ma is
  signal qry : real;
  signal eydtn : std_logic_vector(3 downto 0);
  signal m : std_logic;
begin
  jszbwiipje : entity work.fegn
    port map (jdpwmds => m, lkfsr => eydtn, rexbukyse => qry);
  
  -- Single-driven assignments
  qry <= 42.4_4_1_3_4;
  
  -- Multi-driven assignments
  m <= '1';
end pju;



-- Seed after: 9324688736496853867,17924494779688682807
