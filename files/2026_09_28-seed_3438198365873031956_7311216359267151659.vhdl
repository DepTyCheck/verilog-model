-- Seed: 3438198365873031956,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity jdmg is
  port (jfbxswu : inout std_logic_vector(0 downto 1));
end jdmg;

architecture ovvsju of jdmg is
  
begin
  -- Multi-driven assignments
  jfbxswu <= (others => '0');
end ovvsju;

entity cdpljrqhrv is
  port (c : in boolean);
end cdpljrqhrv;

library ieee;
use ieee.std_logic_1164.all;

architecture yiqarrtmu of cdpljrqhrv is
  signal h : std_logic_vector(0 downto 1);
begin
  franwrz : entity work.jdmg
    port map (jfbxswu => h);
end yiqarrtmu;

entity slsrk is
  port (zxgxcbq : buffer integer; uveevlbp : inout integer; twganefayu : buffer real);
end slsrk;

architecture eowkgqzzfp of slsrk is
  signal lezzn : boolean;
begin
  g : entity work.cdpljrqhrv
    port map (c => lezzn);
  
  -- Single-driven assignments
  uveevlbp <= uveevlbp;
  lezzn <= TRUE;
  zxgxcbq <= uveevlbp;
  twganefayu <= twganefayu;
end eowkgqzzfp;

library ieee;
use ieee.std_logic_1164.all;

entity hayw is
  port (hejlavaeh : inout time; ygwtsq : inout real; qboxjygnx : inout std_logic_vector(1 to 2); dkckjmr : out time_vector(4 downto 3));
end hayw;

library ieee;
use ieee.std_logic_1164.all;

architecture omqerd of hayw is
  signal hzryvfbqf : real;
  signal tasnp : integer;
  signal csdi : integer;
  signal rnyvswt : integer;
  signal elvbyer : integer;
  signal pqiwqmhzo : std_logic_vector(0 downto 1);
  signal de : std_logic_vector(0 downto 1);
begin
  r : entity work.jdmg
    port map (jfbxswu => de);
  a : entity work.jdmg
    port map (jfbxswu => pqiwqmhzo);
  kd : entity work.slsrk
    port map (zxgxcbq => elvbyer, uveevlbp => rnyvswt, twganefayu => ygwtsq);
  e : entity work.slsrk
    port map (zxgxcbq => csdi, uveevlbp => tasnp, twganefayu => hzryvfbqf);
  
  -- Single-driven assignments
  dkckjmr <= (00010 ns, 2_2.0 ns);
  hejlavaeh <= hejlavaeh;
end omqerd;



-- Seed after: 15972549147097561245,7311216359267151659
