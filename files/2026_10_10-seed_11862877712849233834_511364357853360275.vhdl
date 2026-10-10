-- Seed: 11862877712849233834,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity lahmghyru is
  port (nzowht : linkage std_logic_vector(4 to 4); g : in integer_vector(4 to 4); nvbtqqtou : inout integer_vector(2 to 4); gtpuv : linkage integer);
end lahmghyru;

architecture lpfnnakx of lahmghyru is
  
begin
  
end lpfnnakx;

library ieee;
use ieee.std_logic_1164.all;

entity eeafx is
  port (p : in real; c : in std_logic; nxlenvf : in time; hocxmnxhmr : linkage integer);
end eeafx;

library ieee;
use ieee.std_logic_1164.all;

architecture jtlycngz of eeafx is
  signal oujtpnxnbl : integer_vector(2 to 4);
  signal hzabqjqyrm : std_logic_vector(4 to 4);
  signal fw : integer;
  signal oscqr : integer_vector(2 to 4);
  signal xyvqwpc : integer_vector(4 to 4);
  signal qnovtvj : std_logic_vector(4 to 4);
begin
  msk : entity work.lahmghyru
    port map (nzowht => qnovtvj, g => xyvqwpc, nvbtqqtou => oscqr, gtpuv => fw);
  shsjgzechh : entity work.lahmghyru
    port map (nzowht => hzabqjqyrm, g => xyvqwpc, nvbtqqtou => oujtpnxnbl, gtpuv => hocxmnxhmr);
  
  -- Single-driven assignments
  xyvqwpc <= (others => 0313);
  
  -- Multi-driven assignments
  hzabqjqyrm <= (others => 'U');
  hzabqjqyrm <= "1";
end jtlycngz;

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (efunljv : buffer std_logic; vkmb : in time; nvub : linkage std_logic_vector(1 downto 3));
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture vhvbac of l is
  signal gse : integer;
  signal ijxfp : integer_vector(2 to 4);
  signal cheqnsfc : integer_vector(4 to 4);
  signal fuwina : integer;
  signal njvuxil : integer_vector(2 to 4);
  signal dkcxmwv : integer_vector(4 to 4);
  signal xlpllhd : std_logic_vector(4 to 4);
  signal pkyee : integer;
  signal xwbcvxocj : time;
  signal hg : std_logic;
  signal pov : real;
  signal dq : integer;
  signal fzhvi : std_logic;
  signal ygmalpft : real;
begin
  b : entity work.eeafx
    port map (p => ygmalpft, c => fzhvi, nxlenvf => vkmb, hocxmnxhmr => dq);
  ibkcayog : entity work.eeafx
    port map (p => pov, c => hg, nxlenvf => xwbcvxocj, hocxmnxhmr => pkyee);
  cj : entity work.lahmghyru
    port map (nzowht => xlpllhd, g => dkcxmwv, nvbtqqtou => njvuxil, gtpuv => fuwina);
  jmlc : entity work.lahmghyru
    port map (nzowht => xlpllhd, g => cheqnsfc, nvbtqqtou => ijxfp, gtpuv => gse);
  
  -- Single-driven assignments
  cheqnsfc <= dkcxmwv;
  pov <= 402.2;
  ygmalpft <= 8#1_0_6_2_0.0_1_0_4_0#;
  xwbcvxocj <= 333.402 us;
  dkcxmwv <= (others => 16#4_2_1_C_E#);
  
  -- Multi-driven assignments
  xlpllhd <= (others => 'L');
  efunljv <= efunljv;
  efunljv <= '1';
end vhvbac;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (vmcune : in time_vector(3 to 3); wbimdswqb : linkage integer; je : out time_vector(3 downto 1); zoqj : inout std_logic);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture kkosxr of p is
  signal htcssave : std_logic_vector(1 downto 3);
  signal ienr : std_logic;
  signal nooonldpn : time;
  signal zfsxrvg : std_logic;
  signal shywrvo : real;
begin
  idkea : entity work.eeafx
    port map (p => shywrvo, c => zfsxrvg, nxlenvf => nooonldpn, hocxmnxhmr => wbimdswqb);
  vptouj : entity work.l
    port map (efunljv => ienr, vkmb => nooonldpn, nvub => htcssave);
  
  -- Multi-driven assignments
  zoqj <= ienr;
  zfsxrvg <= '1';
  zoqj <= zoqj;
end kkosxr;



-- Seed after: 13676061938836904631,511364357853360275
