-- Seed: 7313935033114013953,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (plk : out boolean_vector(4 to 3); fdmpqj : out time_vector(3 to 3); fopj : in std_logic);
end r;

architecture tdplrq of r is
  
begin
  
end tdplrq;

library ieee;
use ieee.std_logic_1164.all;

entity gztqg is
  port (jprtpd : inout real; wdlj : out std_logic_vector(1 to 0); eihrjl : out boolean_vector(3 downto 3));
end gztqg;

library ieee;
use ieee.std_logic_1164.all;

architecture mjkfxn of gztqg is
  signal jmfbwnq : time_vector(3 to 3);
  signal yalrelg : boolean_vector(4 to 3);
  signal ob : time_vector(3 to 3);
  signal goklinuui : boolean_vector(4 to 3);
  signal xqjeidk : std_logic;
  signal hdzrta : time_vector(3 to 3);
  signal idwth : boolean_vector(4 to 3);
begin
  wvbmratpqy : entity work.r
    port map (plk => idwth, fdmpqj => hdzrta, fopj => xqjeidk);
  aocob : entity work.r
    port map (plk => goklinuui, fdmpqj => ob, fopj => xqjeidk);
  ztatbdkfja : entity work.r
    port map (plk => yalrelg, fdmpqj => jmfbwnq, fopj => xqjeidk);
  
  -- Single-driven assignments
  jprtpd <= 8#1_4.4_1_6_6#;
  eihrjl <= eihrjl;
end mjkfxn;

entity dttbzzhq is
  port (yslalkut : out integer; zacskovlhv : linkage time; tit : inout boolean);
end dttbzzhq;

library ieee;
use ieee.std_logic_1164.all;

architecture afvvhk of dttbzzhq is
  signal lfc : time_vector(3 to 3);
  signal vppc : boolean_vector(4 to 3);
  signal rpmgjiist : std_logic;
  signal igq : time_vector(3 to 3);
  signal vletqike : boolean_vector(4 to 3);
begin
  blt : entity work.r
    port map (plk => vletqike, fdmpqj => igq, fopj => rpmgjiist);
  myasbb : entity work.r
    port map (plk => vppc, fdmpqj => lfc, fopj => rpmgjiist);
  
  -- Single-driven assignments
  yslalkut <= 0;
  tit <= tit;
  
  -- Multi-driven assignments
  rpmgjiist <= 'U';
  rpmgjiist <= '0';
  rpmgjiist <= '0';
  rpmgjiist <= 'X';
end afvvhk;



-- Seed after: 7521688103874678764,5511103086789671269
