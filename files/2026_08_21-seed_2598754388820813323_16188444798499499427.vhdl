-- Seed: 2598754388820813323,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity pcomchc is
  port (rkk : inout time_vector(0 to 1); iel : out std_logic_vector(0 to 2); lbznv : inout std_logic);
end pcomchc;

architecture uvi of pcomchc is
  
begin
  -- Single-driven assignments
  rkk <= (2 min, 16#7_A_7.D_C_5# ps);
  
  -- Multi-driven assignments
  lbznv <= lbznv;
  lbznv <= lbznv;
end uvi;

entity orgj is
  port (q : out string(4 downto 1); zfmdtl : out integer; hoallsu : out time; scpnkyaxwr : in real);
end orgj;

library ieee;
use ieee.std_logic_1164.all;

architecture kpkhsvkcy of orgj is
  signal qoeb : std_logic;
  signal wcynn : time_vector(0 to 1);
  signal yl : std_logic;
  signal qye : std_logic_vector(0 to 2);
  signal yawkak : time_vector(0 to 1);
  signal qro : std_logic;
  signal y : std_logic_vector(0 to 2);
  signal opgorq : time_vector(0 to 1);
begin
  rcvxu : entity work.pcomchc
    port map (rkk => opgorq, iel => y, lbznv => qro);
  jfdwmy : entity work.pcomchc
    port map (rkk => yawkak, iel => qye, lbznv => yl);
  dvypmghkvi : entity work.pcomchc
    port map (rkk => wcynn, iel => y, lbznv => qoeb);
  
  -- Multi-driven assignments
  qye <= "HLX";
  qro <= '0';
end kpkhsvkcy;



-- Seed after: 6339842026355994178,16188444798499499427
