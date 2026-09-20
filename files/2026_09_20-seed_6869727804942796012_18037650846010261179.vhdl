-- Seed: 6869727804942796012,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity kkvhsxxmw is
  port (sysyjstvmf : out std_logic_vector(4 to 4); upzbipi : buffer std_logic; vdaxng : buffer time; wsyjdkxz : in bit_vector(1 to 2));
end kkvhsxxmw;

architecture r of kkvhsxxmw is
  
begin
  -- Single-driven assignments
  vdaxng <= 0 sec;
end r;

library ieee;
use ieee.std_logic_1164.all;

entity ntdiybzpr is
  port (qzs : linkage std_logic_vector(1 to 0); qu : inout real; pu : in std_logic; flo : in real);
end ntdiybzpr;

library ieee;
use ieee.std_logic_1164.all;

architecture xc of ntdiybzpr is
  signal myzgr : time;
  signal rnarme : std_logic;
  signal ct : std_logic_vector(4 to 4);
  signal khpjqfeq : bit_vector(1 to 2);
  signal lvkem : time;
  signal ifgkh : std_logic_vector(4 to 4);
  signal iacxmoemac : bit_vector(1 to 2);
  signal kzmjcf : time;
  signal xxww : std_logic_vector(4 to 4);
  signal vdrugjg : bit_vector(1 to 2);
  signal siayorgdol : time;
  signal ptck : std_logic;
  signal clnpp : std_logic_vector(4 to 4);
begin
  hz : entity work.kkvhsxxmw
    port map (sysyjstvmf => clnpp, upzbipi => ptck, vdaxng => siayorgdol, wsyjdkxz => vdrugjg);
  mstgezgz : entity work.kkvhsxxmw
    port map (sysyjstvmf => xxww, upzbipi => ptck, vdaxng => kzmjcf, wsyjdkxz => iacxmoemac);
  a : entity work.kkvhsxxmw
    port map (sysyjstvmf => ifgkh, upzbipi => ptck, vdaxng => lvkem, wsyjdkxz => khpjqfeq);
  eqm : entity work.kkvhsxxmw
    port map (sysyjstvmf => ct, upzbipi => rnarme, vdaxng => myzgr, wsyjdkxz => vdrugjg);
  
  -- Single-driven assignments
  qu <= 20.03230;
  
  -- Multi-driven assignments
  ct <= "Z";
  xxww <= xxww;
end xc;



-- Seed after: 3270710066746413583,18037650846010261179
