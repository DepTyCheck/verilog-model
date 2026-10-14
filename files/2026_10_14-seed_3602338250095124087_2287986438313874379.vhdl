-- Seed: 3602338250095124087,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity lxaopbgmc is
  port (ajrqta : buffer integer; ddhd : linkage std_logic);
end lxaopbgmc;

architecture fmyss of lxaopbgmc is
  
begin
  -- Single-driven assignments
  ajrqta <= 16#C_2_1#;
end fmyss;

library ieee;
use ieee.std_logic_1164.all;

entity ojrbd is
  port (ub : in std_logic);
end ojrbd;

architecture ipt of ojrbd is
  signal yfmjfv : integer;
  signal duzkckwqi : integer;
  signal exnpew : integer;
  signal gopu : integer;
begin
  tfbawe : entity work.lxaopbgmc
    port map (ajrqta => gopu, ddhd => ub);
  byk : entity work.lxaopbgmc
    port map (ajrqta => exnpew, ddhd => ub);
  thl : entity work.lxaopbgmc
    port map (ajrqta => duzkckwqi, ddhd => ub);
  k : entity work.lxaopbgmc
    port map (ajrqta => yfmjfv, ddhd => ub);
end ipt;

library ieee;
use ieee.std_logic_1164.all;

entity bntjbxioc is
  port (qquxcsoivj : inout integer; nkqs : inout std_logic; navjiaknso : linkage std_logic);
end bntjbxioc;

library ieee;
use ieee.std_logic_1164.all;

architecture ohjadazg of bntjbxioc is
  signal zjrypm : std_logic;
begin
  qtuxclclmi : entity work.ojrbd
    port map (ub => zjrypm);
  
  -- Single-driven assignments
  qquxcsoivj <= 24214;
  
  -- Multi-driven assignments
  nkqs <= '-';
end ohjadazg;

library ieee;
use ieee.std_logic_1164.all;

entity lnrnc is
  port (jri : out time; iseggweqa : out std_logic; msh : in std_logic_vector(4 to 1); avk : buffer integer);
end lnrnc;

architecture sjkwiw of lnrnc is
  signal p : integer;
begin
  h : entity work.ojrbd
    port map (ub => iseggweqa);
  apl : entity work.bntjbxioc
    port map (qquxcsoivj => p, nkqs => iseggweqa, navjiaknso => iseggweqa);
  
  -- Single-driven assignments
  jri <= 0 hr;
  avk <= avk;
end sjkwiw;



-- Seed after: 15445243732910060900,2287986438313874379
