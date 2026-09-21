-- Seed: 5769363136232644067,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity awhku is
  port (kxmeidcj : linkage time_vector(0 downto 4); svejgh : out std_logic_vector(1 downto 0); dv : in std_logic; ou : out std_logic_vector(4 to 4));
end awhku;

architecture eqmqj of awhku is
  
begin
  
end eqmqj;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (vknh : out bit; igwh : buffer real; lur : out std_logic_vector(1 to 4); nnibkf : buffer std_logic_vector(4 downto 3));
end a;

architecture ywjotwgqg of a is
  
begin
  -- Single-driven assignments
  igwh <= 1_3_4_3.3_2_1_3_3;
  
  -- Multi-driven assignments
  nnibkf <= nnibkf;
end ywjotwgqg;

library ieee;
use ieee.std_logic_1164.all;

entity chpgxjdu is
  port (yedvtrz : out integer; elzbvonhys : in boolean_vector(3 to 2); oqqrq : buffer std_logic_vector(1 to 1); ltsurtjh : inout std_logic);
end chpgxjdu;

library ieee;
use ieee.std_logic_1164.all;

architecture zot of chpgxjdu is
  signal v : std_logic_vector(4 to 4);
  signal qlnzpali : time_vector(0 downto 4);
  signal l : std_logic_vector(4 to 4);
  signal jtdbnrt : time_vector(0 downto 4);
  signal dp : std_logic_vector(4 downto 3);
  signal h : real;
  signal yxldshkzi : bit;
  signal hacuy : std_logic_vector(1 downto 0);
  signal qotqqshs : std_logic_vector(1 to 4);
  signal qoxajtny : real;
  signal tliwgxl : bit;
begin
  kxrxeq : entity work.a
    port map (vknh => tliwgxl, igwh => qoxajtny, lur => qotqqshs, nnibkf => hacuy);
  ld : entity work.a
    port map (vknh => yxldshkzi, igwh => h, lur => qotqqshs, nnibkf => dp);
  syfklixvsn : entity work.awhku
    port map (kxmeidcj => jtdbnrt, svejgh => hacuy, dv => ltsurtjh, ou => l);
  dnqplotghk : entity work.awhku
    port map (kxmeidcj => qlnzpali, svejgh => hacuy, dv => ltsurtjh, ou => v);
  
  -- Multi-driven assignments
  ltsurtjh <= 'H';
end zot;



-- Seed after: 10629557708523609603,12143220691580258643
