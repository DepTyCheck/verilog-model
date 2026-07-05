-- Seed: 6659115173574537080,3181554006726329157

entity becblnsgwz is
  port (j : in bit);
end becblnsgwz;

architecture oqtvl of becblnsgwz is
  
begin
  
end oqtvl;

use std.reflection.all;

entity oesuas is
  port (obwu : inout access_subtype_mirror; ncnb : linkage real);
end oesuas;

architecture hoanp of oesuas is
  signal rtipjbz : bit;
  signal srnumxz : bit;
begin
  v : entity work.becblnsgwz
    port map (j => srnumxz);
  fz : entity work.becblnsgwz
    port map (j => rtipjbz);
end hoanp;

use std.reflection.all;

entity s is
  port (f : buffer string(1 to 5); vfkg : inout physical_value_mirror; np : inout enumeration_subtype_mirror);
end s;

architecture swfx of s is
  
begin
  -- Single-driven assignments
  f <= f;
end swfx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity isntlfzppe is
  port (vonskdiyi : out std_logic_vector(1 to 0); mihu : inout file_value_mirror; rheepnjxn : in std_logic_vector(3 downto 1));
end isntlfzppe;

use std.reflection.all;

architecture qzkt of isntlfzppe is
  signal jlq : real;
  shared variable tkhqya : access_subtype_mirror;
  signal ugxmltkc : bit;
  shared variable zvjoyjyv : enumeration_subtype_mirror;
  shared variable hvmtcvgys : physical_value_mirror;
  signal yp : string(1 to 5);
begin
  ceaxhgqixm : entity work.s
    port map (f => yp, vfkg => hvmtcvgys, np => zvjoyjyv);
  vvtmn : entity work.becblnsgwz
    port map (j => ugxmltkc);
  ubz : entity work.becblnsgwz
    port map (j => ugxmltkc);
  avhqn : entity work.oesuas
    port map (obwu => tkhqya, ncnb => jlq);
  
  -- Single-driven assignments
  ugxmltkc <= '1';
  
  -- Multi-driven assignments
  vonskdiyi <= vonskdiyi;
  vonskdiyi <= vonskdiyi;
end qzkt;



-- Seed after: 9762408860188816130,3181554006726329157
