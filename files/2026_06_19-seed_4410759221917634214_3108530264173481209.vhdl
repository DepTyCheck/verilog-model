-- Seed: 4410759221917634214,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity tagvpccxm is
  port (zlkejc : linkage real; rcpec : in std_logic_vector(1 downto 4); oq : in severity_level; naow : inout boolean);
end tagvpccxm;

architecture p of tagvpccxm is
  
begin
  -- Single-driven assignments
  naow <= TRUE;
end p;

entity eja is
  port (zmf : buffer time_vector(1 downto 0));
end eja;

library ieee;
use ieee.std_logic_1164.all;

architecture cmaiicscoe of eja is
  signal jmsrbgcpwq : boolean;
  signal yyjlzumyx : severity_level;
  signal j : std_logic_vector(1 downto 4);
  signal qsyc : real;
begin
  xzzvjbze : entity work.tagvpccxm
    port map (zlkejc => qsyc, rcpec => j, oq => yyjlzumyx, naow => jmsrbgcpwq);
  
  -- Multi-driven assignments
  j <= (others => '0');
  j <= (others => '0');
  j <= "";
end cmaiicscoe;

entity uizqna is
  port (lxbx : buffer string(3 downto 3));
end uizqna;

library ieee;
use ieee.std_logic_1164.all;

architecture nsaacbjg of uizqna is
  signal sts : time_vector(1 downto 0);
  signal elt : boolean;
  signal xf : severity_level;
  signal ldhbjkutx : std_logic_vector(1 downto 4);
  signal ulq : real;
begin
  drousxk : entity work.tagvpccxm
    port map (zlkejc => ulq, rcpec => ldhbjkutx, oq => xf, naow => elt);
  hchnxx : entity work.eja
    port map (zmf => sts);
  
  -- Multi-driven assignments
  ldhbjkutx <= (others => '0');
  ldhbjkutx <= "";
end nsaacbjg;

entity jskokwv is
  port (gwwh : buffer bit_vector(4 to 3));
end jskokwv;

library ieee;
use ieee.std_logic_1164.all;

architecture zkdkponf of jskokwv is
  signal f : boolean;
  signal twfuq : severity_level;
  signal dzyucoi : std_logic_vector(1 downto 4);
  signal q : real;
  signal okwsv : string(3 downto 3);
begin
  aewqvmqyxy : entity work.uizqna
    port map (lxbx => okwsv);
  kwmgwwrqd : entity work.tagvpccxm
    port map (zlkejc => q, rcpec => dzyucoi, oq => twfuq, naow => f);
  
  -- Single-driven assignments
  twfuq <= ERROR;
  gwwh <= (others => '0');
  
  -- Multi-driven assignments
  dzyucoi <= "";
  dzyucoi <= "";
  dzyucoi <= (others => '0');
end zkdkponf;



-- Seed after: 9236252598581226171,3108530264173481209
