-- Seed: 4340528160772745035,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity ycjufctfu is
  port (jnutdrqbi : linkage integer; ffpoevgelu : buffer time; ive : inout std_logic_vector(2 downto 3); k : linkage std_logic);
end ycjufctfu;

architecture kwrhzpp of ycjufctfu is
  
begin
  -- Single-driven assignments
  ffpoevgelu <= 32234.341 ns;
  
  -- Multi-driven assignments
  ive <= (others => '0');
end kwrhzpp;

entity cpjm is
  port (ysat : out real_vector(2 downto 3); sqhaunfw : in real);
end cpjm;

architecture pfwr of cpjm is
  
begin
  -- Single-driven assignments
  ysat <= ysat;
end pfwr;

entity k is
  port (kdp : buffer real);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture mbmrgqgwww of k is
  signal fs : std_logic_vector(2 downto 3);
  signal vynohmta : time;
  signal ukyczlys : integer;
  signal vz : std_logic;
  signal qdhhtd : time;
  signal qfum : integer;
  signal phmgbsg : real_vector(2 downto 3);
  signal vbadjwk : std_logic;
  signal ip : std_logic_vector(2 downto 3);
  signal lvabn : time;
  signal kquyf : integer;
begin
  cu : entity work.ycjufctfu
    port map (jnutdrqbi => kquyf, ffpoevgelu => lvabn, ive => ip, k => vbadjwk);
  ltwukiamqq : entity work.cpjm
    port map (ysat => phmgbsg, sqhaunfw => kdp);
  sxf : entity work.ycjufctfu
    port map (jnutdrqbi => qfum, ffpoevgelu => qdhhtd, ive => ip, k => vz);
  gqxone : entity work.ycjufctfu
    port map (jnutdrqbi => ukyczlys, ffpoevgelu => vynohmta, ive => fs, k => vz);
  
  -- Single-driven assignments
  kdp <= 16#1_C_7.D#;
  
  -- Multi-driven assignments
  ip <= (others => '0');
end mbmrgqgwww;



-- Seed after: 12064856048946217610,13592003931158285879
