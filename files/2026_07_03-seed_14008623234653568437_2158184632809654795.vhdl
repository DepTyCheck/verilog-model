-- Seed: 14008623234653568437,2158184632809654795

use std.reflection.all;

entity jlk is
  port (repaqh : inout access_subtype_mirror);
end jlk;

architecture j of jlk is
  
begin
  
end j;

library ieee;
use ieee.std_logic_1164.all;

entity gqrcja is
  port (ntgjpk : in std_logic_vector(0 downto 3); lyapp : inout std_logic);
end gqrcja;

use std.reflection.all;

architecture usqsvjmjk of gqrcja is
  shared variable lkns : access_subtype_mirror;
  shared variable xs : access_subtype_mirror;
  shared variable xckdkjh : access_subtype_mirror;
begin
  dikmhbr : entity work.jlk
    port map (repaqh => xckdkjh);
  ewiz : entity work.jlk
    port map (repaqh => xs);
  awtzbfj : entity work.jlk
    port map (repaqh => lkns);
  
  -- Multi-driven assignments
  lyapp <= 'X';
  lyapp <= lyapp;
  lyapp <= '-';
  lyapp <= lyapp;
end usqsvjmjk;

use std.reflection.all;

entity nx is
  port (zrzhcc : inout physical_subtype_mirror; jmm : buffer real; ud : buffer time);
end nx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zahwojiukf of nx is
  shared variable ncvtvwgb : access_subtype_mirror;
  signal e : std_logic;
  signal yenxrm : std_logic;
  signal h : std_logic_vector(0 downto 3);
begin
  gocbe : entity work.gqrcja
    port map (ntgjpk => h, lyapp => yenxrm);
  nmxyp : entity work.gqrcja
    port map (ntgjpk => h, lyapp => e);
  jdeo : entity work.jlk
    port map (repaqh => ncvtvwgb);
  
  -- Single-driven assignments
  jmm <= 01.4_0_2;
  ud <= ud;
  
  -- Multi-driven assignments
  h <= h;
  h <= "";
  h <= (others => '0');
end zahwojiukf;



-- Seed after: 1860542089472552723,2158184632809654795
