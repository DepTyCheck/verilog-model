-- Seed: 9800836785704589105,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity dtwarae is
  port (eg : inout std_logic_vector(1 downto 2); yx : buffer std_logic_vector(3 downto 4); tmngl : in real);
end dtwarae;

architecture aeksfj of dtwarae is
  
begin
  -- Multi-driven assignments
  eg <= (others => '0');
  eg <= (others => '0');
  eg <= yx;
  yx <= "";
end aeksfj;

library ieee;
use ieee.std_logic_1164.all;

entity vcipsot is
  port (awo : out time_vector(1 to 2); zxroi : in bit; dbtjtfm : linkage std_logic);
end vcipsot;

library ieee;
use ieee.std_logic_1164.all;

architecture gz of vcipsot is
  signal yxnhvgw : std_logic_vector(3 downto 4);
  signal psagc : real;
  signal xlevu : std_logic_vector(1 downto 2);
  signal ewmoq : std_logic_vector(3 downto 4);
  signal qjrlqcj : real;
  signal ajkuno : std_logic_vector(1 downto 2);
begin
  qdg : entity work.dtwarae
    port map (eg => ajkuno, yx => ajkuno, tmngl => qjrlqcj);
  jecl : entity work.dtwarae
    port map (eg => ajkuno, yx => ewmoq, tmngl => qjrlqcj);
  nzbgy : entity work.dtwarae
    port map (eg => ajkuno, yx => xlevu, tmngl => psagc);
  vsd : entity work.dtwarae
    port map (eg => xlevu, yx => yxnhvgw, tmngl => qjrlqcj);
  
  -- Single-driven assignments
  awo <= awo;
  psagc <= 3.4021;
  qjrlqcj <= 2.1_2_3_0_2;
  
  -- Multi-driven assignments
  ewmoq <= ajkuno;
  ajkuno <= xlevu;
  ajkuno <= yxnhvgw;
  ajkuno <= ajkuno;
end gz;

library ieee;
use ieee.std_logic_1164.all;

entity qs is
  port (lei : inout std_logic; ujgznvz : out std_logic; ucqlevler : out time);
end qs;

library ieee;
use ieee.std_logic_1164.all;

architecture xrpsnzhm of qs is
  signal s : std_logic;
  signal nzygeouay : time_vector(1 to 2);
  signal mpjyaarmvh : bit;
  signal ktudikqtk : time_vector(1 to 2);
  signal kzhusugi : real;
  signal q : std_logic_vector(3 downto 4);
begin
  dxf : entity work.dtwarae
    port map (eg => q, yx => q, tmngl => kzhusugi);
  vmm : entity work.vcipsot
    port map (awo => ktudikqtk, zxroi => mpjyaarmvh, dbtjtfm => ujgznvz);
  tbqzpvezcj : entity work.vcipsot
    port map (awo => nzygeouay, zxroi => mpjyaarmvh, dbtjtfm => s);
  
  -- Single-driven assignments
  ucqlevler <= 1 hr;
end xrpsnzhm;



-- Seed after: 8727210159028883959,7311216359267151659
