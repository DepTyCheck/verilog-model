-- Seed: 10807071877795486297,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nsolxn is
  port (fgmftt : inout time; sxrl : in std_logic_vector(1 downto 2); xpsf : in bit; nemg : inout access_subtype_mirror);
end nsolxn;

architecture b of nsolxn is
  
begin
  -- Single-driven assignments
  fgmftt <= 16#F_B_B# ns;
end b;

use std.reflection.all;

entity ynab is
  port (dqxcmkepjw : in string(4 to 2); mfmjcgiw : inout file_value_mirror; qaliwf : inout enumeration_value_mirror);
end ynab;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture y of ynab is
  shared variable qfaszu : access_subtype_mirror;
  signal nypxymep : time;
  shared variable jep : access_subtype_mirror;
  signal mghlkrec : bit;
  signal wynbtdf : std_logic_vector(1 downto 2);
  signal gxkdeoo : time;
begin
  xufgjwml : entity work.nsolxn
    port map (fgmftt => gxkdeoo, sxrl => wynbtdf, xpsf => mghlkrec, nemg => jep);
  xoq : entity work.nsolxn
    port map (fgmftt => nypxymep, sxrl => wynbtdf, xpsf => mghlkrec, nemg => qfaszu);
  
  -- Single-driven assignments
  mghlkrec <= '1';
end y;



-- Seed after: 4145648028164193256,14426950258250697445
