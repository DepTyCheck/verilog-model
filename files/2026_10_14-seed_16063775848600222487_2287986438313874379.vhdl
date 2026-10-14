-- Seed: 16063775848600222487,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity ptpx is
  port (csaklyy : inout time; jihxsg : buffer std_logic_vector(3 downto 4));
end ptpx;

architecture nttadr of ptpx is
  
begin
  -- Single-driven assignments
  csaklyy <= csaklyy;
  
  -- Multi-driven assignments
  jihxsg <= jihxsg;
end nttadr;

entity ou is
  port (gekbdryygb : in string(2 downto 3));
end ou;

library ieee;
use ieee.std_logic_1164.all;

architecture iki of ou is
  signal yk : std_logic_vector(3 downto 4);
  signal kgez : time;
  signal tocvuasqg : time;
  signal zyboy : time;
  signal za : std_logic_vector(3 downto 4);
  signal hapefry : time;
begin
  piqfjyrd : entity work.ptpx
    port map (csaklyy => hapefry, jihxsg => za);
  px : entity work.ptpx
    port map (csaklyy => zyboy, jihxsg => za);
  kpntjrqn : entity work.ptpx
    port map (csaklyy => tocvuasqg, jihxsg => za);
  bejrhyf : entity work.ptpx
    port map (csaklyy => kgez, jihxsg => yk);
  
  -- Multi-driven assignments
  za <= (others => '0');
  za <= (others => '0');
end iki;

library ieee;
use ieee.std_logic_1164.all;

entity tsbriewd is
  port (ajbdei : out std_logic_vector(3 to 3));
end tsbriewd;

library ieee;
use ieee.std_logic_1164.all;

architecture h of tsbriewd is
  signal yonh : std_logic_vector(3 downto 4);
  signal zzwixapdr : time;
begin
  pufjyjuk : entity work.ptpx
    port map (csaklyy => zzwixapdr, jihxsg => yonh);
  
  -- Multi-driven assignments
  ajbdei <= (others => '-');
end h;



-- Seed after: 5853442630687157559,2287986438313874379
